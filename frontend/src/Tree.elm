module Tree exposing
    ( PolarNode
    , Tree(..)
    , depth
    , map
    , maxLayerWidth
    , toPolarNodes
    )

import Time exposing (ZoneName(..))


type Tree a
    = Node a (List (Tree a))


map : (a -> b) -> Tree a -> Tree b
map fn (Node x children) =
    Node (fn x) (List.map (map fn) children)


depth : Tree a -> Int
depth (Node _ children) =
    1 + List.foldl max 0 (List.map depth children)


maxLayerWidth : Tree a -> Int
maxLayerWidth t =
    layerWidthHelper [ t ] 1


layerWidthHelper : List (Tree a) -> Int -> Int
layerWidthHelper thisLayerRoots widestSoFar =
    case thisLayerRoots of
        [] ->
            widestSoFar

        ts ->
            let
                allChildrenBelow =
                    List.concatMap (\(Node _ cs) -> cs) ts

                newWidest =
                    max widestSoFar (List.length allChildrenBelow)
            in
            layerWidthHelper allChildrenBelow newWidest


type alias Spread a =
    { width : Int
    , startAngle : Float
    , endAngle : Float
    , content : a
    }


type alias WidthTag a =
    { content : a
    , width : Int
    }


widthTag : a -> Int -> WidthTag a
widthTag x w =
    { content = x
    , width = w
    }


annotateWidth : Tree a -> Tree (WidthTag a)
annotateWidth (Node x cs) =
    if List.isEmpty cs then
        Node (widthTag x 1) []

    else
        let
            annotatedChildren =
                List.map annotateWidth cs

            myWidth =
                List.foldl (+) 0 <| List.map (\(Node a _) -> a.width) <| annotatedChildren
        in
        Node (widthTag x myWidth) annotatedChildren


spreadChildrenHelper : Tree (Spread a) -> Tree (Spread a)
spreadChildrenHelper (Node s cs) =
    if List.isEmpty cs then
        Node s []

    else
        let
            anglePerWidth =
                (s.endAngle - s.startAngle) / toFloat s.width

            fn (Node sc theseChildren) ( curAngle, results ) =
                let
                    localEnd =
                        curAngle + toFloat sc.width * anglePerWidth

                    res =
                        Node { sc | startAngle = curAngle, endAngle = localEnd } theseChildren
                in
                ( localEnd, res :: results )

            spreadCs =
                List.foldl fn ( s.startAngle, [] ) cs
                    |> Tuple.second
                    |> List.reverse
                    |> List.map spreadChildrenHelper
        in
        Node s spreadCs


spreadChildren : Tree a -> Tree (Spread a)
spreadChildren t =
    let
        widthTagged =
            annotateWidth t

        spreadStart wt =
            { content = wt.content
            , width = wt.width
            , startAngle = 0
            , endAngle = 2 * pi
            }
    in
    spreadChildrenHelper (map spreadStart widthTagged)


type alias PolarNode a =
    { content : a
    , depth : Int
    , angle : Float
    }


toPolarNodesHelper : Int -> Tree (PolarNode a) -> List (PolarNode a)
toPolarNodesHelper dpth (Node pn cs) =
    { pn | depth = dpth } :: List.concatMap (toPolarNodesHelper (dpth + 1)) cs


toPolarNodes : Tree a -> List (PolarNode a)
toPolarNodes t =
    let
        spreadToPlr spr =
            { content = spr.content
            , depth = -1
            , angle = (spr.startAngle + spr.endAngle) / 2
            }

        mappedTree =
            map spreadToPlr <| spreadChildren t
    in
    toPolarNodesHelper 0 mappedTree
