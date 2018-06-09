module Types exposing (CourseRecord, TimeTableRecord, CourseRecordRaw, Lesson, Class, Aggregate, LessonDetails, AugTimeTableRecord)

type alias TimeTableRecord =
  { classNo : String
  , lessonType : String
  , weekText : String
  , dayText : String
  , startTime : String
  , endTime : String
  , venue : String
  } 

type alias AugTimeTableRecord =
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

type alias CourseRecordRaw = 
  { moduleCode : String
  , timetable : List TimeTableRecord
  }
  
type alias Class = List TimeTableRecord

type alias Lesson =
  { classes : List Class
  , lessonType : String
  }

type alias CourseRecord = 
  { moduleCode : String
  , lessons : List Lesson
  }

type alias LessonDetails =
  { classes : List (List AugTimeTableRecord)
  , lessonType : String
  , moduleCode : String
  }


type alias Aggregate =
  List LessonDetails