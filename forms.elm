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
  , age : Int
  }

model : Model
model =
  Model "" "" "" 0

-- UPDATE
type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain passwordAgain ->
      { model | passwordAgain = passwordAgain }

    Age age ->
      { model | age = Result.withDefault -1 (String.toInt age) }

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

validateAge : Model -> String
validateAge model =
  if model.age < 0 then "Age must be a positive number" else ""

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "number", placeholder "Age", onInput Age ] []
    , viewValidation model
    ]

error : Model -> (String, String)
error model =
  if join "" [validateMatch model, validateLength model, validateCharacters model, validateAge model] /= "" then
    ("red", join "" [validateMatch model, validateLength model, validateCharacters model, validateAge model])
  else
    ("green", "ok")

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      error model
  in
    div [style [("color", color)] ] [ text message ]
