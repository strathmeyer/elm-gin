module Main exposing (main)

import Browser
import Model
import Update
import View

main : Program () Model.Model Update.Msg
main =
    Browser.sandbox
        { init = Model.init
        , view = View.view
        , update = Update.update
        }
