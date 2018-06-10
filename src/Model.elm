module Model exposing (init, fetch)

import Message exposing (Msg)
import Array2D
import Http
import Autocomplete
import Dict
import Json.Decode exposing (dict, string)
import Set
import Types exposing (..)


initialModel : Model
initialModel = 
    { availability =
        Array2D.fromList [
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
        ]
    , modules = Dict.empty
    , search = ""
    , showDropdown = True
    , selectedModules = Set.insert "CS2040" Set.empty
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
            "https://api.nusmods.com/2017-2018/" ++ model.semester ++ "/moduleList.json"
        request =
            Http.get apiUrl (dict string)
    in
        Http.send Message.ModuleFetch request


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , fetch initialModel
    )
