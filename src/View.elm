module View exposing (view)

import Types exposing (..)
import Message exposing (Msg(..))
import Html exposing (Html
                      , div
                      , text
                      , table
                      , h2
                      , tr
                      , td
                      , input
                      , select
                      , option
                      , i
                      , button)
import Html.Events exposing (onClick, onInput, onBlur, onFocus)
import Html.Attributes exposing (class, id, colspan, placeholder, value, classList)
import List exposing (range, map)
import Array2D
import Set
import Autocomplete

-- Wrap the main table with a container and add heading
view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [  id "main-ctr",  class "col-12 shadow" ] 
              [  h2 [] [text "Your Schedule"]
              ,  mainTable model
              ,  showStatus model ] ]
            
-- Show the status if any
showStatus : Model -> Html Msg
showStatus model =
  if String.isEmpty model.currentStatus then
    div [ class "status-bar hidden" ] [ text "Done!" ]
  else
    div [ class "status-bar" ]
        [ i [ class "fas fa-spinner fa-spin" ] [], text " ", text model.currentStatus ]

-- Creates a table with the headers and the 5 rows
mainTable : Model -> Html Msg
mainTable model = 
    table [ class "time-table" ]
          [  header
          ,  row model 0 "Mon" 
          ,  row model 1 "Tue" 
          ,  row model 2 "Wed" 
          ,  row model 3 "Thu" 
          ,  row model 4 "Fri"
          ,  formRow model
          ,  moduleList model ]

-- I think this function should be self-explanatory
header : Html Msg
header =
    tr [ class "times" ]
       [  td [ class "day" ] []
       ,  td [] [ text "0800 "] 
       ,  td [] [ text "0900 "] 
       ,  td [] [ text "1000 "] 
       ,  td [] [ text "1100 "] 
       ,  td [] [ text "1200 "] 
       ,  td [] [ text "1300 "] 
       ,  td [] [ text "1400 "] 
       ,  td [] [ text "1500 "] 
       ,  td [] [ text "1600 "] 
       ,  td [] [ text "1700 "] ]

-- Create a row, inclusive of cells and title, for a given day
row : Model -> Day -> String -> Html Msg
row model day dayName =
  tr []
     ( td [ class "day"] [ text dayName ] :: cells model day)

-- Render all cells for a given row
cells : Model -> Day -> List (Html Msg)
cells model day =
  let 
    hours = range 0 9 
  in
    map (cell model day) hours     

-- Render one cell with colour dependent on selection status
cell : Model -> Day -> Hour -> Html Msg
cell model day hour =
  let result = Array2D.get day hour model.availability in
    case result of 
      Just 1 ->
        td [  class "highlightable active",  onClick (RemoveHour day hour) ] []
      _ ->
        td [  class "highlightable",  onClick (AddHour day hour) ] []
        
-- Render the row containing input allowing users to add modules and select term
formRow : Model -> Html Msg
formRow model = 
  tr []
     [  td [class "modules-blank-cell no-padding"] []
     , searchInput model
     , semesterSelection
     , optimizeButton]

viewConfig : Autocomplete.ViewConfig String
viewConfig =
  let
    customizedLi keySelected mouseSelected course =
      { attributes = [ classList [ ("item", True), ("active", keySelected || mouseSelected) ] ]
      , children = [ Html.text course ]
      }
  in
    Autocomplete.viewConfig
      { toId =  \x -> x
      , ul = [ class "combolist shadow" ] -- set classes for your list
      , li = customizedLi -- given selection states and a person, create some Html!
      }

-- Simple input that shows dropdown as user types
searchInput : Model -> Html Msg
searchInput model = 
  td [colspan 6
     , class "search-ctr"] 
     [ input [ class "input search-input"
              , placeholder "Search for Modules (Press Enter to Add Modules)"
              , onInput ChangeSearch
              , onBlur HideDropdown
              , onFocus UnblankDropdown
              , value model.search ] []
       , Html.map 
            SetAutocompleteState 
            (Autocomplete.view 
              viewConfig 
              5 
              model.searchState 
              model.shortListedModules)] 

-- Show semesters for users to select
semesterSelection : Html Msg
semesterSelection =
  td [ colspan 2, class "search-ctr px-2" ]
     [ select [class "input", onInput ChangeSemester]
              (List.map 
                (\(x, y) ->  option [value (toString y)] [text x]) 
                [ ("Semester 1", 1)
                , ("Semester 2", 2)
                , ("Special Term 1", 3)
                , ("Special Term 2", 4) ] ) ]

-- The optimize button
optimizeButton : Html Msg
optimizeButton = 
  td [ colspan 2, class "search-ctr" ]
     [ button [ class "btn btn-optimize input", onClick StartOptimise ]
              [ text "Figure It Out" ] ]

-- Show list of modules at bottom of window
moduleList : Model -> Html Msg
moduleList model =
  tr []
     [ td [class "modules-blank-cell"] []
     , td [colspan 10, class "modules-ctr no-padding"] 
          [ div [class "row"]
                (showModules model) ] ]

-- Get modules to show from the set
showModules : Model -> List (Html Msg)
showModules model =
  if Set.size model.selectedModules == 0 then
    [ div [class "col-12"] [text "You've yet to select any modules."] ]
  else
    Set.foldr 
      (\x acc -> div [class "col-md-2"] 
                     [ text x, text " "
                     , i [class "red fas fa-times"
                         , onClick (Delete x)] 
                         [] ] :: acc) 
      [] 
      model.selectedModules
