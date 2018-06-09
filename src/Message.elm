module Message exposing (Msg(..))

import Http
import Dict exposing (Dict)
import Autocomplete
import Types exposing (CourseRecordRaw, TimeTableRecord, AugTimeTableRecord)
import Array2D exposing (Array2D)

type Msg
    -- Do nothing
    = NoOp
    -- The user has changed the value of the search box
    | ChangeSearch String
    -- Deselect timeslot
    | RemoveHour Int Int
    -- Select timeslot
    | AddHour Int Int
    -- Get list of modules from API
    | ModuleFetch (Result Http.Error (Dict String String))
    -- Delete course from selection
    | Delete String
    -- Add course to selection
    | AddCourse String
    -- Update state of autocomplete 
    | SetAutocompleteState Autocomplete.Msg
    -- Reset search box
    | ResetSearch
    -- User changes semester
    | ChangeSemester String
    -- Hide dropdown when user selects elsewhere with a delay
    | HideDropdown
    -- Actual command to blank out dropdown
    | BlankDropdown
    -- And a command to revert the Blank command
    | UnblankDropdown
    -- Start the optimizer
    | StartOptimise
    -- Http result from fetch course
    | CourseFetch (Result Http.Error CourseRecordRaw)
    -- Status to remove after a small delay
    | RemoveStatus String
    -- Actual function to remove status
    | HideStatus String
    -- Result from the optimizer
    | OptimizedResult (Maybe (Array2D (List AugTimeTableRecord)))

    