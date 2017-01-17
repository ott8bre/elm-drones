module Main exposing (..)

import Keyboard
import Time exposing (..)
import Window exposing (Size)
import AnimationFrame
import Html
import View exposing (view)
import Update exposing (Msg(..), update, init)
import Model exposing (Game, State(Pause))


keyboardProcessor : number -> Msg
keyboardProcessor keyCode =
    case keyCode of
        32 ->
            TogglePlay

        _ ->
            NoOp

subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.ups keyboardProcessor
            --, Keyboard.downs (keyboardProcessor True)
            --, Keyboard.ups (keyboardProcessor False)
        , if model.state == Pause then Sub.none else AnimationFrame.diffs (Tick << inSeconds)
        ]

--main : Program Never


main =
    Html.program
        { init = init
        , update = \msg m -> update msg m ! []
        , view = view
        , subscriptions = subscriptions
        }
