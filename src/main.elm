module Main exposing (..)

import Keyboard
import Time exposing (..)
import Window exposing (Size)
import AnimationFrame
import Html
import View exposing (view)
import Update exposing (Msg(..), update, init)


keyboardProcessor : number -> Msg
keyboardProcessor keyCode =
    case keyCode of
        32 ->
            TogglePlay

        _ ->
            NoOp


--main : Program Never
main =
    Html.program
        { init = init
        , update = \msg m -> update msg m ! []
        , view = view
        , subscriptions =
            (\_ ->
                Sub.batch
                    [ Window.resizes Resize
                    , Keyboard.ups keyboardProcessor
                      --, Keyboard.downs (keyboardProcessor True)
                      --, Keyboard.ups (keyboardProcessor False)
                    , AnimationFrame.diffs (Tick << inSeconds)
                    ]
            )
        }
