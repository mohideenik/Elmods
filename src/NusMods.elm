module NusMods exposing (..)

import Types exposing (..)
import Dict exposing (Dict)
import Array
import Array2D
import Maybe.Extra exposing (combine)

getShareUrl : String -> Schedule -> Maybe String
getShareUrl semester schedule =
  let
      allLessons = 
        List.range 0 4
        |> List.map (\r -> Array2D.getRow r schedule)
        |> combine
        |> Maybe.map (\day -> List.concatMap Array.toList day)
        |> Maybe.map (\list -> List.concat list)
        |> Maybe.map (List.foldr (\elem acc -> 
              let
                  maybeRecord = Dict.get elem.moduleCode acc
                  currentRecord = Maybe.withDefault Dict.empty maybeRecord
                  updatedRecord = currentRecord |> Dict.insert elem.lessonType elem.classNo
              in
                  acc |> Dict.insert elem.moduleCode updatedRecord 
           ) Dict.empty)
        |> Maybe.map Dict.toList
        |> Maybe.map (List.map (\(x, y) -> (x, Dict.toList y)))
        |> Maybe.map (List.foldr (\(course, lessons) acc -> 
              let
                lessonString =
                  List.foldr
                    (\(kind, number) acc -> 
                      (Maybe.withDefault 
                        "TUT" 
                        (Dict.get kind serializeTable)) 
                      ++ ":" ++ number ++ "," ++ acc)
                    ""
                    lessons
                  |> String.dropRight 1
              in
                acc ++ course ++ "=" ++ lessonString ++ "&"
           ) "?" )
        |> Maybe.map (\url -> "https://nusmods.com/timetable/sem-" ++ semester ++ "/share" ++ url)
        |> Maybe.map (String.dropRight 1)
  in
      allLessons

serializeTable : Dict String String
serializeTable =
  Dict.fromList [
    ("Design Lecture", "DLEC"),
    ("Laboratory", "LAB"),
    ("Lecture", "LEC"),
    ("Packaged Lecture", "PLEC"),
    ("Packaged Tutorial", "PTUT"),
    ("Recitation", "REC"),
    ("Sectional Teaching", "SEC"),
    ("Seminar-Style Module Class", "SEM"),
    ("Tutorial", "TUT"),
    ("Tutorial Type 2", "TUT2"),
    ("Tutorial Type 3", "TUT3"),
    ("Workshop", "WS")
  ]