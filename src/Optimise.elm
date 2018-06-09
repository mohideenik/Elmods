module Optimise exposing (optimise, getModuleInfo, processCourseInfo)

import Array2D exposing (Array2D)
import Model exposing (Model)
import Message exposing (Msg(..))
import Set
import Json.Decode exposing (string, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Types exposing (TimeTableRecord, CourseRecordRaw, CourseRecord, Lesson, Class, Aggregate, LessonDetails, AugTimeTableRecord)
import List.Extra
import Array2D exposing (Array2D)
import String

type alias Schedule = Array2D (List AugTimeTableRecord)

optimise : Model -> Array2D Int -> Maybe Schedule
optimise model availability = 
  Debug.log "Optimized" (addModules availability model.processedCourseInfo (emptyTable availability))

addModules : Array2D Int -> Aggregate -> Schedule -> Maybe Schedule
addModules availability lessonList scheduleSoFar =
  case lessonList of
    [] -> 
      Just scheduleSoFar
    h :: t ->
      let
          choices = h.classes
      in
          List.foldr
            (\class acc ->
              if acc == Nothing then 
                (assignLessons scheduleSoFar availability class
                |> Maybe.andThen
                    (\updatedSchedule ->
                      addModules availability t updatedSchedule
                    ))
              else
                acc
            )
            Nothing
            choices

assignLessons : Schedule -> Array2D Int -> List AugTimeTableRecord -> Maybe Schedule
assignLessons schedule availability lessons =
    List.foldr
      (\lesson acc ->
        Maybe.andThen (\newSchedule -> assignLesson newSchedule availability lesson) acc
      )
      (Just schedule)
      lessons

assignLesson : Schedule -> Array2D Int -> AugTimeTableRecord -> Maybe Schedule
assignLesson schedule availability lesson = 
    let
      indices = List.range lesson.startIndex lesson.endIndex
      boxes = List.map (\hour -> Array2D.get lesson.dayIndex hour schedule) indices
      hourAvailability =
        (List.map 
          (\lessons -> 
            case lessons of
              Nothing -> False
              Just lessons -> 
                let
                  results = List.map (lessonClash lesson) lessons
                in
                  (List.all (\p -> p == False) results)
          )
          boxes)
      avail = List.all (\p -> p == True) hourAvailability
    in
      if avail == True && availabilityClash availability lesson == True then
        addToSchedule lesson schedule
      else
        Nothing

addToSchedule : AugTimeTableRecord -> Schedule -> Maybe Schedule
addToSchedule lesson schedule =
    let
        indices = List.range lesson.startIndex lesson.endIndex
    in
        List.foldr
        (\hour acc ->
            Maybe.andThen (\scheduleSoFar ->
                let
                    hourLesson = Array2D.get lesson.dayIndex hour scheduleSoFar
                    newHour = Maybe.map (\hl -> lesson :: hl) hourLesson
                in
                    Maybe.map 
                      (\newH -> Array2D.set lesson.dayIndex hour newH scheduleSoFar)
                      newHour
            ) acc
        )        
        (Just schedule)
        indices

availabilityClash : Array2D Int -> AugTimeTableRecord -> Bool
availabilityClash availability lesson =
  List.range lesson.startIndex lesson.endIndex
  |> List.map
      (\hour -> 
        Array2D.get lesson.dayIndex hour availability == Just 1
      )
  |> List.all (\p -> p == True)

lessonClash : AugTimeTableRecord -> AugTimeTableRecord -> Bool
lessonClash lessonA lessonB =
  let
    result =
      if lessonA.dayIndex /= lessonB.dayIndex then
        False
      else if lessonA.startTime <= lessonB.startTime &&
              lessonA.endTime >= lessonB.startTime then
        True
      else if lessonB.startTime <= lessonA.startTime &&
              lessonB.endTime >= lessonA.startTime then
        True
      else
        False
  in
    result
      

emptyTable : Array2D Int -> Schedule
emptyTable availability = 
    Array2D.initialize
      (Array2D.rows availability)
      (Array2D.columns availability)
      (\_ _ -> [])

processCourseInfo : List CourseRecordRaw -> Aggregate
processCourseInfo courseRecordRawList =
  List.map processEachCourse courseRecordRawList
  |> List.concatMap 
    (\cr -> 
      List.map 
        (\lesson -> 
          { classes = lesson.classes
          , moduleCode = cr.moduleCode
          , lessonType = lesson.lessonType 
          }
        ) 
        cr.lessons
    )
  |> List.map 
    (\lesson ->
      { lessonType = lesson.lessonType
      , moduleCode = lesson.moduleCode 
      , classes = 
        List.map 
          (\class ->
            List.map 
              (\x -> 
                { classNo = x.classNo
                , lessonType = x.lessonType
                , weekText = x.weekText
                , dayText = x.dayText
                , startTime = x.startTime
                , endTime = x.endTime
                , venue = x.venue
                , startIndex = startHourToIndex x.startTime
                , endIndex = endHourToIndex x.endTime
                , moduleCode = lesson.moduleCode
                , dayIndex = weekToIndex x.dayText
                } 
              ) 
              class) 
          lesson.classes
      } 
    )

weekToIndex : String -> Int
weekToIndex weekday =
  case weekday of
    "Monday" -> 0
    "Tuesday" -> 1
    "Wednesday" -> 2
    "Thursday" -> 3
    "Friday" -> 4
    _ -> 0

startHourToIndex : String -> Int
startHourToIndex hour = 
  case String.toInt hour of
    Ok result -> 
      (result // 100) - 8
    Err error ->
      0

endHourToIndex : String -> Int
endHourToIndex hour =
  case String.toInt hour of
    Ok result ->
      if rem result 100 > 0 then
        (result // 100) - 8
      else
        (result // 100) - 9
    Err error ->
      0

processEachCourse : CourseRecordRaw -> CourseRecord
processEachCourse course =
  { moduleCode = course.moduleCode
  , lessons = processLessons course.timetable }

processLessons : List TimeTableRecord -> List Lesson
processLessons timetableList =
  List.sortBy .lessonType timetableList
    |> List.Extra.groupWhile (\x y -> x.lessonType == y.lessonType)
    |> List.map (\x -> chunk (processClasses x))

chunk : List Class -> Lesson
chunk classList = 
  case classList of 
    [] -> { lessonType = "None", classes = [] }
    h :: t -> 
      case h of 
        [] -> { lessonType = "None", classes = h :: t} 
        hh :: tt -> { lessonType = hh.lessonType, classes = h :: t }

processClasses : List TimeTableRecord -> List Class
processClasses timetableList =
  List.sortBy .classNo timetableList
  |> List.Extra.groupWhile (\x y -> x.classNo == y.classNo)

getModuleInfo : Model -> (Model, Cmd Msg)
getModuleInfo model =
  let
    moduleList = Set.toList model.selectedModules
  in
    model ! List.map (fetch model.semester) moduleList

fetch : String -> String -> Cmd Msg
fetch semester courseCode =
  let
      apiUrl = 
            "http://api.nusmods.com/2017-2018/" 
            ++ semester 
            ++ "/modules/" 
            ++ courseCode 
            ++ "/index.json"
      request =
        Http.get apiUrl decodeCourse
  in
      Http.send CourseFetch request

decodeCourse : Decoder CourseRecordRaw
decodeCourse =
  decode CourseRecordRaw
    |> required "ModuleCode" string
    |> required "Timetable" (list decodeTimeTable)

decodeTimeTable : Decoder TimeTableRecord
decodeTimeTable = 
  decode TimeTableRecord
    |> required "ClassNo" string
    |> required "LessonType" string
    |> required "WeekText" string
    |> required "DayText" string
    |> required "StartTime" string
    |> required "EndTime" string
    |> required "Venue" string
