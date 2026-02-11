module Tree exposing
    ( PolarNode
    , Tree(..)
    , WithPos
    , flatten
    , layoutTree
    , map
    , mapLeaves
    , maxLayerWidth
    , mkCartesian
    , mkEdges
    , singleton
    , testTree
    , toPolarEdges
    , toPolarNodes
    )

import BackendWrapper exposing (Node)
import List exposing (concatMap)
import Time exposing (ZoneName(..))


type Tree a
    = Node a (List (Tree a))


map : (a -> b) -> Tree a -> Tree b
map fn (Node x children) =
    Node (fn x) (List.map (map fn) children)


mapLeaves : (a -> a) -> Tree a -> Tree a
mapLeaves fn (Node x children) =
    case children of
        [] ->
            Node (fn x) []

        cs ->
            Node x (List.map (mapLeaves fn) cs)


flatten : Tree a -> List a
flatten (Node x cs) =
    x :: List.concatMap flatten cs


depthWithHelp : Int -> (Int -> a -> b) -> Tree a -> Tree b
depthWithHelp soFar fn (Node x cs) =
    let
        newContent =
            fn soFar x

        newChildren =
            List.map (depthWithHelp (soFar + 1) fn) cs
    in
    Node newContent newChildren


depthWith : (Int -> a -> b) -> Tree a -> Tree b
depthWith =
    depthWithHelp 0


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


spreadToPlr : Spread a -> PolarNode a
spreadToPlr spr =
    { content = spr.content
    , depth = -1
    , angle = (spr.startAngle + spr.endAngle) / 2
    }


toPolarNodes : Tree a -> Tree (PolarNode a)
toPolarNodes t =
    t
        |> spreadChildren
        |> map spreadToPlr
        |> depthWith (\d p -> { p | depth = d })


type alias Edge a =
    { from : a
    , to : a
    }


mkEdges : Tree a -> List (Edge a)
mkEdges (Node x cs) =
    let
        childEdges =
            concatMap mkEdges cs

        rootEdges =
            List.map (\(Node y _) -> { from = x, to = y }) cs
    in
    rootEdges ++ childEdges


toPolarEdges : Tree a -> List (Edge (PolarNode a))
toPolarEdges t =
    t
        |> spreadChildren
        |> map spreadToPlr
        |> depthWith (\d p -> { p | depth = d })
        |> mkEdges


mkCartesian : PolarNode a -> WithPos a
mkCartesian pn =
    let
        r =
            toFloat pn.depth * 250

        x =
            r * cos pn.angle

        y =
            r * sin pn.angle
    in
    { pos = ( x, y ), content = pn.content }


type alias WithPos a =
    { pos : ( Float, Float )
    , content : a
    }


layoutTree : Tree a -> Tree (WithPos a)
layoutTree tree =
    tree
        |> toPolarNodes
        |> map mkCartesian


singleton : a -> Tree a
singleton x =
    Node x []


testTree : Tree ()
testTree =
    Node ()
        [ Node ()
            [ Node () []
            , Node ()
                [ Node () []
                , Node () []
                ]
            , Node () []
            ]
        , Node ()
            [ Node () []
            ]
        , Node ()
            [ Node ()
                [ Node () []
                , Node () []
                , Node () []
                ]
            ]
        , Node () []
        ]
