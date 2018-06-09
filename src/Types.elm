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

type alias RawGroup = List RawClassRecord

type alias ModuleRecordRaw = 
  { moduleCode : String
  , timetable : List RawClassRecord
  }

type alias RawLesson =
  { groups : List RawGroup
  , lessonType : String
  }

type alias ModuleRecord = 
  { moduleCode : String
  , lessons : List RawLesson
  }

type alias LessonDetails =
  { groups : List (List ClassRecord)
  , lessonType : String
  , moduleCode : String
  }

type alias AllLessons =
  List LessonDetails

type alias Day = Int
type alias Hour = Int
type alias CourseCode = String
type alias Description = String

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
    , moduleInfo : Dict String ModuleRecordRaw
    , allLessons : AllLessons
    , optimizedSchedule : Maybe (Array2D (List ClassRecord))
    }

type alias Schedule = Array2D (List ClassRecord)
