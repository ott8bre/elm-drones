module View exposing (view)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text
import Html exposing (..)
import Env exposing (..)
import Packet exposing (Packet)
import Drone exposing (Drone, Msg(Take, Drop))
import Model exposing (Game, State(Play, Pause, End))


view : Game -> Html msg
view model =
    let
        { width, height } =
            model.size

        scores =
            case model.state of
                Play ->
                    spacer 1 1

                _ ->
                    txt identity ("Turn " ++ toString model.turn)

        info =
            case model.state of
                Play ->
                    spacer 1 1

                _ ->
                    txt identity "press SPACE to start/pause"
    in
        toHtml <|
            container width height middle <|
                collage gameWidth
                    gameHeight
                    ([ rect gameWidth gameHeight
                        |> filled backgroundColor
                     , toForm scores
                        |> move ( 0, gameHeight / 2 - 40 )
                     , toForm info
                        |> move ( 0, 40 - gameHeight / 2 )
                     ]
                        ++ List.map (.address >> make 5 Color.blue) warehouses
                        ++ List.map (.address >> make 5 Color.red) orders
                        ++ List.map drawDrone model.drones
                        ++ (List.indexedMap (drawProducts (-gameWidth / 2) (-gameHeight / 2)) model.products |> List.concat)
                        ++ List.indexedMap (drawPackets (gameWidth / 2) (gameHeight / 2)) model.packets
                    )


drawProducts : Float -> Float -> Int -> Int -> List Form
drawProducts x y i size =
    let
        f =
            \h -> square Color.green ( x + 5 * h, y + 5 * k )

        range =
            List.range 1 size

        k =
            1 + i
    in
        List.map f range


drawPackets : Float -> Float -> Int -> Packet -> Form
drawPackets x y i order =
    let
        k =
            1 + i
    in
        square Color.brown ( x - 5 * k, y - 5 )


square : Color -> ( Float, Float ) -> Form
square color ( x, y ) =
    rect 4 4
        |> filled color
        |> move ( x, y )


drawDrone : Drone -> Form
drawDrone { position } =
    oval 5 5
        |> filled Color.white
        |> move ( position.x, position.y )


backgroundColor =
    rgb 10 10 10


wareColor =
    blue


orderColor =
    red


droneColor =
    white


textColor =
    rgb 160 200 160


txt f string =
    Text.fromString string
        |> Text.color textColor
        |> Text.monospace
        |> f
        |> leftAligned


make r c obj =
    oval r r
        |> filled c
        |> move ( obj.x, obj.y )
