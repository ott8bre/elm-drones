module Main exposing (main)

import Array exposing (..)
import Drone exposing (..)
import Env exposing (..)
import Packet exposing (..)
import Playground exposing (..)
import Point exposing (..)


type Status
    = Play
    | Pause


type alias Model =
    { status : Status
    , space_key_down : Bool
    , size : Point
    , turn : Int
    , drones : List Drone
    , packets : List Packet
    , products : List Int
    }


initialModel : Model
initialModel =
    { status = Pause
    , space_key_down = False
    , turn = 0
    , size = Point 600 400
    , drones = List.repeat Env.numberOfDrones initDrone
    , packets = List.map orderToPackets orders |> List.concat
    , products = List.map waresToProducts warehouses
    }


startPoint : Point
startPoint =
    List.head warehouses |> Maybe.map .position |> Maybe.withDefault Point.origin


initDrone : Drone
initDrone =
    startPoint |> Drone.init Env.droneMaxLoad


orderToPackets : Target -> List Packet
orderToPackets order =
    let
        f x y =
            Packet.init x y startPoint order.position

        packs =
            Array.indexedMap f order.items
    in
    Array.toList packs
        |> List.filter (\p -> p.copies /= 0)


waresToProducts : Target -> Int
waresToProducts ware =
    Array.toList ware.items |> List.sum


main =
    game view update initialModel


update : Computer -> Model -> Model
update env model =
    let
        terminated =
            List.isEmpty model.packets && List.all Drone.isEmpty model.drones

        toggle =
            not model.space_key_down && env.keyboard.space
    in
    case model.status of
        Play ->
            let
                ( newPackets, newDrones ) =
                    updatePacketsDrones model.packets model.drones
            in
            { model
                | turn = 1 + model.turn
                , status =
                    if toggle || terminated then
                        Pause

                    else
                        model.status
                , space_key_down = env.keyboard.space
                , drones = newDrones
                , packets = newPackets
            }

        _ ->
            if toggle then
                if terminated then
                    initialModel

                else
                    { model
                        | status = Play
                        , space_key_down = env.keyboard.space
                    }

            else
                { model
                    | space_key_down = env.keyboard.space
                }


takeDrop packet ( drone, rest ) =
    if Drone.canLoad packet drone then
        ( drone |> Drone.enqueue (Take packet) |> Drone.enqueue (Drop packet), rest )

    else
        ( drone, packet :: rest )


updatePacketsDrones : List Packet -> List Drone -> ( List Packet, List Drone )
updatePacketsDrones packets drones =
    let
        func drone ( ps, ds ) =
            let
                ( newDrone, others ) =
                    List.foldr takeDrop ( drone, [] ) ps
            in
            ( others, Drone.update newDrone :: ds )
    in
    List.foldr func ( packets, [] ) drones


view : Computer -> Model -> List Shape
view env model =
    let
        background =
            rectangle black env.screen.width env.screen.height

        productsLayer =
            group <| (List.indexedMap (drawProducts (-env.screen.width / 2) (-env.screen.height / 2)) model.products |> List.concat)

        packetsLayer =
            group <| List.indexedMap (drawPackets (env.screen.width / 2) (env.screen.height / 2)) model.packets

        infoLayer =
            group <|
                case model.status of
                    Play ->
                        []

                    _ ->
                        [ "Press SPACE to start/pause"
                            |> words green
                            |> move 0 (-env.screen.height / 2 + 40)
                        , "Turn "
                            ++ String.fromInt model.turn
                            |> words green
                            |> move 0 (env.screen.height / 2 - 40)
                        ]

        warehousesLayer =
            group <| List.map (.position >> drawCircle 9 blue) warehouses

        dronesLayer =
            group <| List.map (.position >> drawCircle 5 white) model.drones

        ordersLayer =
            group <| List.map (.position >> drawCircle 7 red) orders
    in
    [ background
    , productsLayer
    , packetsLayer
    , infoLayer
    , warehousesLayer
    , ordersLayer
    , dronesLayer
    ]


drawCircle : Number -> Color -> Point -> Shape
drawCircle radius color point =
    circle color radius
        |> move point.x point.y


drawSquare : Number -> Color -> Point -> Shape
drawSquare side color point =
    rectangle color side side
        |> move point.x point.y


drawProducts : Number -> Number -> Int -> Int -> List Shape
drawProducts x y i size =
    let
        k =
            toFloat (1 + i)

        f =
            \h -> drawSquare 4 green <| Point (x + 5 * h) (y + 5 * k)

        range =
            List.range 1 size
    in
    List.map (f << toFloat) range


drawPackets : Float -> Float -> Int -> Packet -> Shape
drawPackets x y i order =
    let
        position =
            Point (x - 5 * toFloat (1 + i)) (y - 5)
    in
    drawSquare 4 brown position
