import Html exposing (Html, section, header, h1, div, input, text, ul, li, button)
import Html.Events exposing (..)
import Json.Decode as Json
import Html.Attributes exposing (..)
import String

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

{--
    Our model is simple. We want to store the newEntry from the text box and list of entires entered so far.
--}
type alias Model =
  {   newEntry : String
  ,   entries : List String
  }

{--
    Initialize model as a Model with empty string and null list
--}
model : Model
model =
  Model "" []

-- UPDATE

{--
    Two kinds of messages are generated from View.
--}

type Msg
  = UpdateField String 
  | Add

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add -> { model | 
                entries =
                    if String.isEmpty model.newEntry then
                        model.entries
                    else
                        model.newEntry :: model.entries
                ,   newEntry = ""
            }
    
    UpdateField entry -> { model | newEntry = entry }


-- VIEW

-- currently TextBox don't understand Enter key
-- https://stackoverflow.com/a/41072936
onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then -- 13 is ascii code for enter
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

-- function takes a string and returns a HTML element which can take a message
todoLister : String -> Html msg
todoLister msg =
  li [] [ text msg ]

-- View has one text box and one unordered list
view : Model -> Html Msg
view model =
    div []
        [
            input
               [ placeholder "What needs to be done?"
               , autofocus True
               , value model.newEntry
               , name "newTodo"
               , onInput UpdateField
               , onEnter Add
               ] [] 
        ,   div [] [ul [] (List.map todoLister model.entries)]
        ]
    
    