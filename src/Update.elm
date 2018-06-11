module Update exposing (update)

import Message exposing (Msg(..))
import Model exposing (fetch)
import Types exposing (..)
import Array2D
import Set
import Autocomplete
import Dict
import String exposing (contains, toLower)
import Process
import Time
import Task
import Optimise exposing (optimise, getModuleInfo, processCourseInfo)
import NusMods exposing (getShareUrl)

-- Function to process messages and return an updated model
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        AddHour day hour ->
            setHour model day hour 1 ! []
        RemoveHour day hour ->
            setHour model day hour 0 ! []
        ModuleFetch (Ok response) ->
            { model | modules = response } 
            |> update (RemoveStatus "Retrieving modules' information")
        RemoveStatus status ->
            let
                cmd = 
                    Process.sleep (Time.second)
                    |> Task.perform (\_ -> HideStatus status)
            in
                model ! [ cmd ]
        HideStatus status ->
            ( { model | status = Set.remove status model.status} 
              |> updateStatus
            ) ! []
        ModuleFetch (Err error) ->
            update NoOp model
        CourseFetch (Ok response) ->
            let
                nModel = { model 
                         | moduleInfo = 
                                model.moduleInfo 
                                |> Dict.insert response.moduleCode response 
                         }
                newModel = { nModel 
                           | allLessons = 
                                    Dict.values nModel.moduleInfo 
                                    |> processCourseInfo 
                           }
                done = Dict.size newModel.moduleInfo == Set.size newModel.selectedModules
            in
                if done then 
                    let
                        cmd = 
                            Task.succeed 
                                (optimise newModel.semester newModel.allLessons model.availability)
                            |> Task.perform (\result -> OptimizedResult result)
                    in
                    newModel ! [cmd]
                else
                    newModel ! []
        OptimizedResult result ->
            { model 
            | optimizedSchedule = result
            , url = Maybe.andThen (getShareUrl model.semester) result 
            , showFinalScreen = True } 
            |> update (RemoveStatus "Finding the perfect timetable for you.")
        HideFinalScreen ->
            { model | showFinalScreen = False } ! []
        CourseFetch (Err error) ->
            update NoOp model
        BlankDropdown ->
            { model | shortListedModules = [] } ! []
        UnblankDropdown ->
            { model | shortListedModules = acceptableCourses model.search model } ! []
        HideDropdown ->
            let 
                cmd =
                    Process.sleep (Time.millisecond * 300) 
                    |> Task.perform (\_ -> BlankDropdown)
            in
                model ! [ cmd ]
        ChangeSearch search ->
            updateSearch search model ! []
        Delete courseCode ->
            { model | selectedModules = Set.remove courseCode model.selectedModules } ! []
        ChangeSemester value ->
            let
                newModel = { model | semester = value, selectedModules = Set.empty } 
            in
                (newModel |> addStatus "Retrieving modules' information") ! [ fetch newModel ]
        AddCourse id ->
            addCourse model id
            |> update ResetSearch
        ResetSearch ->
            { model | searchState = Autocomplete.reset updateConfig model.searchState
                    , search = ""
                    , shortListedModules = [] } ! []
        SetAutocompleteState autoMsg ->
            let
                (newState, maybeMsg) =
                    Autocomplete.update 
                        updateConfig 
                        autoMsg 
                        5 
                        model.searchState 
                        model.shortListedModules
                newModel = {model | searchState = newState }
            in
                maybeMsg
                |> Maybe.map (\newMsg -> update newMsg newModel) 
                |> Maybe.withDefault (newModel ! [])
        StartOptimise ->
            if Set.size model.selectedModules > 0 then
                { model | moduleInfo = Dict.empty } 
                |> addStatus "Finding the perfect timetable for you." 
                |> getModuleInfo
            else
                model ! []

-- Add a status to the UI; shows any status if there are more than one
addStatus : String -> Model -> Model
addStatus newStatus model =
    { model | status = Set.insert newStatus model.status }
    |> updateStatus

-- Shows a status if there is one
updateStatus : Model -> Model
updateStatus model =
    if Set.isEmpty model.status then
        { model | currentStatus = "" }
    else if Set.member model.currentStatus model.status then
        model
    else
        let 
            nextStatus = Set.toList model.status |> List.head
        in
            { model | currentStatus = Maybe.withDefault "" nextStatus }

-- Configuration for the autocomplete plugin; used in the `updateSearch` function
updateConfig : Autocomplete.UpdateConfig Msg String
updateConfig =
    Autocomplete.updateConfig
        { toId =  \x -> x
        , onKeyDown =
            \code maybeId ->
                case code of 
                    13 ->
                        Maybe.map AddCourse maybeId
                    27 -> 
                        Just ResetSearch
                    _ ->
                        Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| AddCourse id
        , separateSelections = False
        }

-- Trigger the autocomplete plugin to re-render the updated dropdown
updateSearch : String -> Model -> Model
updateSearch search model =
    { model | search = search
            , searchState = 
                Autocomplete.resetToFirstItem 
                    updateConfig 
                    (acceptableCourses search model) 
                    5
                    model.searchState
            , shortListedModules = acceptableCourses search model }

-- Filters the modules list using the given search query
acceptableCourses : String -> Model -> List String
acceptableCourses search model =
    if  String.length search > 0 then
        Dict.keys (Dict.filter (\k v -> 
                let
                    key = toLower k
                    term = toLower search
                    value = toLower v
                in
                    (key |> contains term) || 
                    (value |> contains term)) 
                model.modules)
    else
        []

-- Changes the value of a particular cell to the given value x
setHour : Model -> Int -> Int -> Int -> Model
setHour model day hour x =
    let 
        updatedArray = Array2D.set day hour x model.availability 
    in
        { model | availability = updatedArray }

-- Adds a given course to the selectedModules Set
addCourse : Model -> String -> Model
addCourse model courseCode =
    { model | selectedModules = Set.insert courseCode model.selectedModules }
    |> updateSearch "" 
