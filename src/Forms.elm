import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing (isDigit, isUpper, isLower)
import Array exposing (push)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validation :
    { age : Bool
    , password : 
      { length : Bool
      , hasDigit : Bool
      , hasUpper : Bool
      , hasLower : Bool
      , equality : Bool
      }
    }
  }


init : Model
init =
  { name = ""
  , password = ""
  , passwordAgain = ""
  , age = ""
  , validation = 
    { age = True
    , password =
      { length = True
      , hasDigit = True
      , hasUpper = True
      , hasLower = True
      , equality = True
      }
    }
  }



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

    Submit ->
      let validation =  { age = String.all isDigit model.age
                        , password =
                          { length = String.length model.password >= 8
                          , hasDigit = String.any isDigit model.password
                          , hasUpper = String.any isUpper model.password
                          , hasLower = String.any isLower model.password
                          , equality = model.password == model.passwordAgain
                          }
                        }
      in
      { model | validation = validation }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "text" "Age" model.age Age
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , button [ onClick Submit ] [ text "Check" ]
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if not model.validation.age then
    div [ style "color" "red" ] [ text "Age must be a number" ]
  else if not model.validation.password.length then
    div [ style "color" "red" ] [ text "Password must be more than or equal 8 characters in length" ]
  else if not model.validation.password.hasDigit then
    div [ style "color" "red" ] [ text "Password must have numeric characters " ] 
  else if not model.validation.password.hasUpper then
    div [ style "color" "red" ] [ text "Password must have an Uppercase characters " ]
  else if not model.validation.password.hasLower then
    div [ style "color" "red" ] [ text "Password must have an lowercase characters " ] 
  else if not model.validation.password.equality then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
    div [ style "color" "green" ] [ text "OK" ]
