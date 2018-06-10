module Types exposing (..)

import Array2D exposing (Array2D)
import Dict exposing (Dict)
import Set exposing (Set)
import Autocomplete

type alias Availability = Array2D Int

type alias RawClassRecord =
  { classNo : String
  , lessonType : String
  , weekText : String
  , dayText : String
  , startTime : String
  , endTime : String
  , venue : String
  } 

type alias ClassRecord =
  { classNo : String
  , lessonType : String
  , weekText : String
  , dayText : String
  , startTime : String
  , endTime : String
  , venue : String
  , startIndex : Int
  , endIndex : Int
  , moduleCode : String
  , dayIndex : Int
  } 

type alias RawClasses = List RawClassRecord

type alias RawModuleRecord = 
  { moduleCode : String
  , timetable : RawClasses
  }

type alias RawGroup = List RawClasses

type alias RawLesson =
  { groups : RawGroup
  , lessonType : String
  }

type alias PartialModuleRecord = 
  { moduleCode : String
  , lessons : List RawLesson
  }

type alias Group = List ClassRecord

type alias Lesson =
  { groups : List Group
  , lessonType : String
  , moduleCode : String
  }

type alias AllLessons =
  List Lesson

type alias Day = Int
type alias Hour = Int
type alias CourseCode = String
type alias Description = String

type alias Schedule = Array2D (List ClassRecord)

type alias Model =
    { availability: Availability
    , modules : Dict CourseCode Description
    , search : String
    , showDropdown: Bool
    , selectedModules : Set CourseCode
    , searchState : Autocomplete.State
    , shortListedModules : List CourseCode
    , semester : String
    , status : Set String
    , currentStatus : String
    , moduleInfo : Dict CourseCode RawModuleRecord
    , allLessons : AllLessons
    , url : Maybe String
    , showFinalScreen : Bool
    , optimizedSchedule : Maybe Schedule
    }
