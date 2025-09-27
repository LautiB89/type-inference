module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import LambdaParser exposing (parse, viewExpr)
import Html.Events exposing (onClick)


-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String, showImplicitParens : Bool }


init : Model
init =
    { content = "", showImplicitParens = False }



-- UPDATE


type Msg
    = Change String | ToggleImplicitParens


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }
        ToggleImplicitParens ->
            { model | showImplicitParens = not model.showImplicitParens }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to parse", value model.content, onInput Change ] []
        , button [ onClick ToggleImplicitParens ] [ text "toggle implicit parens" ]
        , div [] [ text (viewExpr model.showImplicitParens (parse model.content)) ]
        ]
