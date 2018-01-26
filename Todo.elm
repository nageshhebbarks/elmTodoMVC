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

type alias Model =
  {   newEntry : String
  ,   entries : List String
  }

model : Model
model =
  Model "" []

-- UPDATE

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
                        model.newEntry :: model.entries,
                newEntry = ""
            }
    
    UpdateField entry -> { model | newEntry = entry }


-- VIEW

-- https://stackoverflow.com/a/41072936
onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

todoLister : String -> Html msg
todoLister msg =
  li [] [ text msg ]

view : Model -> Html Msg
view model =
    div []
        [
            header
                [ class "header" ]
                [ h1 [] [ text "todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , value model.newEntry
                    , name "newTodo"
                    , onInput UpdateField
                    , onEnter Add
                    ]
                    []
                ]
        ,    div [] [ul [] (List.map todoLister model.entries)]
        ]
    
    