module Main exposing (main)

import Html exposing (program)
import Model exposing (init, Model)
import Message exposing (Msg)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)

{--
    Using webpack makes me feel like your grandma using the iPhone.

    On the loading/splash screen, show things like:
        1. Making sure that you abide by the honour code...
        2. Checking if your CAP is positive...
        3. Measuring the correlation of CAP with real-world performance...
        4. Finding the class with the greatest proportion of students from the opposite gender...
        5. Searching for the class with the best bell curve...

-}

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
