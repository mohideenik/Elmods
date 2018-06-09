module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Message exposing (Msg(..))
import Autocomplete

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutocompleteState Autocomplete.subscription

