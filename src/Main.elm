module Main exposing (main)

import Html exposing (program)
import Model exposing (init)
import Types exposing (Model)
import Message exposing (Msg)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)

{-|
    A program to help NUS students generate timetables based on their availability.

    The terminology used in the source code is as follows:
    -   Students take one or more modules per semester
    -   Each module has one or more lessons (lectures, tutorials, etc)
    -   Each lesson has one or more groups and a student has to choose one and only one group.
    -   Each group has one or more classes (lectures once a week, tutorials twice a week, etc)

    I'm currently in the process of rewriting the code so that variable and function names
    are consistent with the above definitions.

    TODO
    ====
    1.  On the loading/splash screen, show things like:
            - Making sure that you abide by the honour code...
            - Checking if your CAP is positive...
            - Measuring the correlation of CAP with real-world performance...
            - Finding the class with the greatest proportion of students from the opposite gender...
            - Searching for the class with the best bell curve...

-}

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
