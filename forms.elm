import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import String exposing (length, any, join)
import Char exposing (isUpper, isLower, isDigit)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

model : Model
model =
  Model "" "" ""

-- UPDATE
type Msg
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain passwordAgain ->
      { model | passwordAgain = passwordAgain }

validateMatch : Model -> String
validateMatch model =
  if model.password /= model.passwordAgain then "Passwords do not match!" else ""

validateLength : Model -> String
validateLength model =
  if length model.password < 8 then "Passwords must be at least 8 characters!" else ""

validateCharacters : Model -> String
validateCharacters model =
  if any isUpper model.password && any isLower model.password && any isDigit model.password then
    ""
  else
    "Passwords must contains uppercase, lowecase and numeric characters!"

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]

error : Model -> (String, String)
error model =
  if join "" [validateMatch model, validateLength model, validateCharacters model] /= "" then
    ("red", join "" [validateMatch model, validateLength model, validateCharacters model])
  else
    ("green", "ok")

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      error model
  in
    div [style [("color", color)] ] [ text message ]
