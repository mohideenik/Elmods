module Model exposing (init, fetch)

import Message exposing (Msg)
import Array2D exposing (Array2D)
import Http
import Autocomplete
import Dict exposing (Dict)
import Json.Decode exposing (dict, string)
import Set exposing (Set)
import Types exposing (..)


initialModel : Model
initialModel = 
    { availability = Array2D.initialize 5 10 (\_ _ -> 0)
    , modules = Dict.empty
    , search = ""
    , showDropdown = True
    , selectedModules = (Set.insert "GER1000" Set.empty)
    , searchState = Autocomplete.empty
    , shortListedModules = []
    , semester = "1"
    , status = Set.empty
    , currentStatus = "Retrieving modules' information"
    , moduleInfo = Dict.empty
    , allLessons = []
    , optimizedSchedule = Nothing
    }

-- Fetch data for semester 1
fetch : Model -> Cmd Msg
fetch model =
    let
        apiUrl =
            "http://api.nusmods.com/2017-2018/" ++ model.semester ++ "/moduleList.json"
        request =
            Http.get apiUrl (dict string)
    in
        Http.send Message.ModuleFetch request


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , fetch initialModel
    )
