module Optimise exposing (optimise, getModuleInfo, processCourseInfo)

import Array2D exposing (Array2D)
import Message exposing (Msg(..))
import Set
import Json.Decode exposing (string, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Types exposing (..)
import List.Extra
import Array2D exposing (Array2D)
import String


optimise : Model -> Availability -> Maybe Schedule
optimise model availability = 
  Debug.log "Optimized" (addModules availability model.allLessons (emptyTable availability))

addModules : Availability -> AllLessons -> Schedule -> Maybe Schedule
addModules availability lessonList scheduleSoFar =
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

assignLessons : Schedule -> Availability -> List ClassRecord -> Maybe Schedule
assignLessons schedule availability lessons =
    List.foldr
      (\lesson acc ->
        Maybe.andThen (\newSchedule -> assignLesson newSchedule availability lesson) acc
      )
      (Just schedule)
      lessons

assignLesson : Schedule -> Availability -> ClassRecord -> Maybe Schedule
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

addToSchedule : ClassRecord -> Schedule -> Maybe Schedule
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

availabilityClash : Availability -> ClassRecord -> Bool
availabilityClash availability lesson =
  List.range lesson.startIndex lesson.endIndex
  |> List.map
      (\hour -> 
        Array2D.get lesson.dayIndex hour availability == Just 1
      )
  |> List.all (\p -> p == True)

lessonClash : ClassRecord -> ClassRecord -> Bool
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
      

emptyTable : Availability -> Schedule
emptyTable availability = 
    Array2D.initialize
      (Array2D.rows availability)
      (Array2D.columns availability)
      (\_ _ -> [])

processCourseInfo : List ModuleRecordRaw -> AllLessons
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

processEachCourse : ModuleRecordRaw -> ModuleRecord
processEachCourse course =
  { moduleCode = course.moduleCode
  , lessons = processLessons course.timetable }

processLessons : List RawClassRecord -> List RawLesson
processLessons timetableList =
  List.sortBy .lessonType timetableList
    |> List.Extra.groupWhile (\x y -> x.lessonType == y.lessonType)
    |> List.map (\x -> chunk (processClasses x))

chunk : List RawGroup -> RawLesson
chunk classList = 
  case classList of 
    [] -> { lessonType = "None", groups = [] }
    h :: t -> 
      case h of 
        [] -> { lessonType = "None", groups = h :: t} 
        hh :: tt -> { lessonType = hh.lessonType, groups = h :: t }

processClasses : List RawClassRecord -> List RawGroup
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

decodeCourse : Decoder ModuleRecordRaw
decodeCourse =
  decode ModuleRecordRaw
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
