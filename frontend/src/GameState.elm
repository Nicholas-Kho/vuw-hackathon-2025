module GameState exposing
    ( GameMode(..)
    , GameState
    , Msg(..)
    , UpdatedGame(..)
    , fromInitial
    , setRoaming
    , update
    , view
    )

import BackendWrapper exposing (Node, Subgraph, getNode, getOutgoing, xformSubgraph)
import Camera exposing (Camera, Vec2, focusOn, moveCam, stopAnimation, tickCam, vDistSqare, zoomAbout)
import Canvas
import Canvas.Settings
import Color
import Drawable exposing (renderGrid)
import Element exposing (Element)
import Generated.BackendApi exposing (InitialGameState, NodeId)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Navigation exposing (NTNode(..), NavTree, addInFlight, getTreeWithLoadingNodes, insertFetchResults, insertNeighborsAt, isFrontier, recomputeMemo)
import PlayerInput exposing (UserInput(..))
import Tree exposing (WithPos, layoutTree)


type alias GameState =
    { startAt : NodeId
    , gameMode : GameMode
    , nodeCache : Subgraph
    , nav : NavTree
    , focus : ( NodeId, Node )
    , cam : Camera
    }


type GameMode
    = Roaming
    | Find FindRules


type alias FindRules =
    { targetId : NodeId
    , targetNode : Node
    , movesLeft : Int
    }


setRoaming : GameState -> GameState
setRoaming gs =
    { gs | gameMode = Roaming }


type Msg
    = GotNodes Subgraph
    | GotInputs (List UserInput)
    | Tick Float
    | SizeChanged ( Float, Float )


type alias FetchList =
    List NodeId


type UpdatedGame
    = KeepGoing ( GameState, FetchList )
    | GameOver


fromInitial : ( NodeId, Node ) -> Node -> Camera -> InitialGameState -> GameState
fromInitial initialFocus goalNode cam igs =
    { startAt = igs.startAt
    , gameMode =
        Find
            { targetId = igs.endAt
            , targetNode = goalNode
            , movesLeft = 10
            }
    , nodeCache = xformSubgraph igs.subgraph
    , nav = Navigation.singleton ( igs.startAt, Tuple.second initialFocus )
    , cam = cam
    , focus = initialFocus
    }


simple : GameState -> UpdatedGame
simple gs =
    KeepGoing ( gs, [] )


update : Msg -> GameState -> UpdatedGame
update msg model =
    case msg of
        GotNodes sg ->
            expandCache sg model |> simple

        Tick delta ->
            handleTick delta model |> simple

        SizeChanged s ->
            resizeCamera s model |> simple

        GotInputs inps ->
            handleInputs inps model


handleInput : UserInput -> GameState -> UpdatedGame
handleInput uinp gs =
    case uinp of
        Pan dx dy ->
            let
                zoom =
                    gs.cam.zoom

                panBy =
                    ( dx / zoom, dy / zoom )
            in
            simple { gs | cam = stopAnimation <| moveCam panBy gs.cam }

        Zoom dz cursorPos ->
            simple { gs | cam = stopAnimation <| zoomAbout cursorPos -dz <| gs.cam }

        Action PlayerInput.Center ->
            simple { gs | cam = camToWorldOrigin gs.cam }

        Action (PlayerInput.Click pos) ->
            handleClick pos gs


isWinningNid : NodeId -> GameState -> Bool
isWinningNid nid gs =
    case gs.gameMode of
        Find rules ->
            nid == rules.targetId

        Roaming ->
            False


decrementMoves : GameState -> GameState
decrementMoves gs =
    case gs.gameMode of
        Roaming ->
            gs

        Find rules ->
            { gs | gameMode = Find { rules | movesLeft = rules.movesLeft - 1 } }


hasLost : GameState -> Bool
hasLost gs =
    case gs.gameMode of
        Roaming ->
            False

        Find rules ->
            rules.movesLeft <= 1


handleClick : Vec2 -> GameState -> UpdatedGame
handleClick pos gs =
    let
        clickPosWorld =
            Camera.camPosToWorldPos gs.cam pos

        clickedNode =
            getClickedNode (Tree.flatten <| Navigation.getLayout gs.nav) clickPosWorld
    in
    case clickedNode of
        Nothing ->
            simple gs

        Just ( _, Fetching ) ->
            simple gs

        Just ( nid, Loaded n ) ->
            if isWinningNid nid gs then
                GameOver

            else if hasLost gs then
                GameOver

            else
                let
                    ( updatedTree, stillNeedToFetch ) =
                        addNeighbors gs.nodeCache nid (getOutgoing n) gs.nav

                    decrementIfNeeded =
                        if isFrontier gs.nav ( nid, n ) then
                            decrementMoves

                        else
                            \x -> x

                    updatedUpdatedTree =
                        addInFlight nid stillNeedToFetch updatedTree |> recomputeMemo

                    newCam =
                        getTreeWithLoadingNodes updatedUpdatedTree
                            |> layoutTree
                            |> Tree.flatten
                            |> List.filter (\p -> Tuple.first p.content == nid)
                            |> List.map .pos
                            |> List.head
                            |> Maybe.map (\p -> zoomInOn p gs.cam)
                            |> Maybe.withDefault gs.cam
                in
                KeepGoing
                    ( { gs
                        | focus = ( nid, n )
                        , nav = updatedUpdatedTree
                        , cam = newCam
                      }
                        |> decrementIfNeeded
                    , stillNeedToFetch
                    )


getClickedNode : List (WithPos a) -> Vec2 -> Maybe a
getClickedNode ns clickWorld =
    let
        clickCounts n =
            vDistSqare clickWorld n.pos < 400
    in
    List.filter clickCounts ns |> List.head |> Maybe.map .content


partitionHits : Subgraph -> List NodeId -> ( List ( NodeId, Node ), List NodeId )
partitionHits =
    partitionHitsHelper ( [], [] )


partitionHitsHelper :
    ( List ( NodeId, Node ), List NodeId )
    -> Subgraph
    -> List NodeId
    -> ( List ( NodeId, Node ), List NodeId )
partitionHitsHelper result cache lookupThese =
    case lookupThese of
        [] ->
            result

        nid :: rest ->
            case getNode cache nid of
                Nothing ->
                    partitionHitsHelper (Tuple.mapSecond (\r -> nid :: r) result) cache rest

                Just n ->
                    partitionHitsHelper (Tuple.mapFirst (\r -> ( nid, n ) :: r) result) cache rest


addNeighbors : Subgraph -> NodeId -> List NodeId -> NavTree -> ( NavTree, List NodeId )
addNeighbors cache parent cids nt =
    let
        ( hits, misses ) =
            partitionHits cache cids

        newTree =
            insertNeighborsAt nt parent hits
    in
    ( newTree, misses )


expandCache : Subgraph -> GameState -> GameState
expandCache sg gs =
    let
        newNav =
            insertFetchResults sg gs.nav |> recomputeMemo
    in
    { gs
        | nodeCache = BackendWrapper.union sg gs.nodeCache
        , nav = newNav
    }


camToWorldOrigin : Camera -> Camera
camToWorldOrigin =
    focusOn ( 0, 0 ) 1


zoomInOn : ( Float, Float ) -> Camera -> Camera
zoomInOn p =
    focusOn p 1


handleTick : Float -> GameState -> GameState
handleTick deltaMs gs =
    { gs | cam = tickCam deltaMs gs.cam }


handleInputs : List UserInput -> GameState -> UpdatedGame
handleInputs uips g =
    List.foldl handleInputsHelp (simple g) uips


handleInputsHelp : UserInput -> UpdatedGame -> UpdatedGame
handleInputsHelp input acc =
    case acc of
        GameOver ->
            GameOver

        KeepGoing ( gs, cmd ) ->
            case handleInput input gs of
                GameOver ->
                    GameOver

                KeepGoing ( newGs, newCmd ) ->
                    KeepGoing ( newGs, cmd ++ newCmd )


resizeCamera : ( Float, Float ) -> GameState -> GameState
resizeCamera s gs =
    let
        oldCam =
            gs.cam

        newCam =
            { oldCam | canvasSize = s }
    in
    { gs | cam = newCam }


view : ( Int, Int ) -> List (Attribute msg) -> GameState -> Element msg
view size extraAttrs gs =
    let
        ( w, h ) =
            size
    in
    Element.html <|
        Canvas.toHtml size
            (extraAttrs
                ++ [ style "display" "block"
                   , style "box-sizing" "border-box"
                   ]
            )
            [ Canvas.shapes
                [ Canvas.Settings.fill Color.lightGrey ]
                [ Canvas.rect ( 0, 0 ) (toFloat w) (toFloat h) ]
            , renderGrid gs.cam 100
            , Drawable.drawNavTree (Tuple.first gs.focus) gs.cam gs.nav
            ]
