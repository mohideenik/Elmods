module Optimise exposing (optimise, getModuleInfo, processCourseInfo)

import Message exposing (Msg(..))
import Set
import Json.Decode exposing (string, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Types exposing (..)
import List.Extra
import Array2D
import String

optimise : String -> AllLessons -> Availability -> Maybe Schedule
optimise semester allLessons availability = 
    addLessons availability allLessons (emptyTable availability)
    |> Debug.log "Optimized"
      

addLessons : Availability -> AllLessons -> Schedule -> Maybe Schedule
addLessons availability lessonList scheduleSoFar =
  case lessonList of
    [] -> 
      Just scheduleSoFar
    h :: t ->
      let
          choices = h.groups
      in
          List.foldr
            (\class acc ->
              if acc == Nothing then 
                assignGroup scheduleSoFar availability class
                |> Maybe.andThen
                    (\updatedSchedule ->
                      addLessons availability t updatedSchedule
                    )
              else
                acc
            )
            Nothing
            choices

assignGroup : Schedule -> Availability -> Group -> Maybe Schedule
assignGroup schedule availability group =
    List.foldr
      (\lesson acc ->
        Maybe.andThen (\newSchedule -> assignClass newSchedule availability lesson) acc
      )
      (Just schedule)
      group

assignClass : Schedule -> Availability -> ClassRecord -> Maybe Schedule
assignClass schedule availability class = 
    let
      indices = List.range class.startIndex class.endIndex
      boxes = List.map (\hour -> Array2D.get class.dayIndex hour schedule) indices
      hourAvailability =
        List.map 
          (\assignedLessons -> 
            assignedLessons
            |> Maybe.map 
                (\lessons -> 
                  lessons
                  |> List.map (lessonClash class) 
                  |> List.all (\p -> p == False)
                )
            |> Maybe.withDefault False
          )
          boxes
      avail = List.all (\p -> p == True) hourAvailability
    in
      if avail == True && availabilityClash availability class == True then
        addToSchedule class schedule
      else
        Nothing

addToSchedule : ClassRecord -> Schedule -> Maybe Schedule
addToSchedule class schedule =
    let
        indices = List.range class.startIndex class.endIndex
    in
        List.foldr
        (\hour acc ->
            Maybe.andThen 
              (\scheduleSoFar ->
                  Array2D.get class.dayIndex hour scheduleSoFar
                  |> Maybe.map ((::) class)
                  |> Maybe.map (\newH -> Array2D.set class.dayIndex hour newH scheduleSoFar)
              ) 
              acc
        )        
        (Just schedule)
        indices

availabilityClash : Availability -> ClassRecord -> Bool
availabilityClash availability class =
  List.range class.startIndex class.endIndex
  |> List.map
      (\hour -> 
        Array2D.get class.dayIndex hour availability == Just 1
      )
  |> List.all (\p -> p == True)

lessonClash : ClassRecord -> ClassRecord -> Bool
lessonClash classA classB =
  let
    result =
      if classA.dayIndex /= classB.dayIndex then
        False
      else if classA.startTime <= classB.startTime &&
              classA.endTime >= classB.startTime then
        True
      else if classB.startTime <= classA.startTime &&
              classB.endTime >= classA.startTime then
        True
      else
        False
  in
    result
      

emptyTable : Availability -> Schedule
emptyTable availability = 
    Array2D.initialize
      (Array2D.rows availability)
      (Array2D.columns availability)
      (\_ _ -> [])

processCourseInfo : List RawModuleRecord -> AllLessons
processCourseInfo moduleRecordRawList =
  List.map processEachCourse moduleRecordRawList
  |> List.concatMap 
    (\cr -> 
      List.map 
        (\lesson -> 
          { classes = lesson.groups
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
      , groups = 
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

processEachCourse : RawModuleRecord -> PartialModuleRecord
processEachCourse course =
  { moduleCode = course.moduleCode
  , lessons = processLessons course.timetable }

processLessons : List RawClassRecord -> List RawLesson
processLessons timetableList =
  List.sortBy .lessonType timetableList
    |> List.Extra.groupWhile (\x y -> x.lessonType == y.lessonType)
    |> List.map (\x -> chunk (processClasses x))

chunk : List RawClasses -> RawLesson
chunk classList = 
  case classList of 
    [] -> { lessonType = "None", groups = [] }
    h :: t -> 
      case h of 
        [] -> { lessonType = "None", groups = h :: t} 
        hh :: tt -> { lessonType = hh.lessonType, groups = h :: t }

processClasses : RawClasses -> RawGroup
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
            "https://api.nusmods.com/2017-2018/" 
            ++ semester 
            ++ "/modules/" 
            ++ courseCode 
            ++ "/index.json"
      request =
        Http.get apiUrl decodeCourse
  in
      Http.send CourseFetch request

decodeCourse : Decoder RawModuleRecord
decodeCourse =
  decode RawModuleRecord
    |> required "ModuleCode" string
    |> required "Timetable" (list decodeTimeTable)

decodeTimeTable : Decoder RawClassRecord
decodeTimeTable = 
  decode RawClassRecord
    |> required "ClassNo" string
    |> required "LessonType" string
    |> required "WeekText" string
    |> required "DayText" string
    |> required "StartTime" string
    |> required "EndTime" string
    |> required "Venue" string
