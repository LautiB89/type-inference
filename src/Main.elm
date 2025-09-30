module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h2, h3, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import LambdaParser exposing (parse, viewExpr, rectify)



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
    = Change String
    | ToggleImplicitParens


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
    let
        ifShowParens a b =
            if model.showImplicitParens then
                a

            else
                b
    in
    div [ style "max-width" "600px", style "margin" "40px auto", style "font-family" "sans-serif", style "font-size" "24px" ]
        [ h2 [] [ text "λ-Calculus parser" ]
        , textarea
            [ value model.content
            , onInput Change
            , placeholder "(\\x.x x) (\\y.y y)"
            , rows 4
            , cols 60
            , style "margin-bottom" "12px"
            ]
            []
        , div [ style "margin-bottom" "12px" ]
            [ button
                [ onClick ToggleImplicitParens
                , style "background"
                    (ifShowParens "#007acc" "#eee")
                , style "color"
                    (ifShowParens "white" "black")
                , style "border" "none"
                , style "padding" "8px 16px"
                , style "cursor" "pointer"
                ]
                [ text
                    (ifShowParens "Hide" "Show" ++ " implicit parens")
                ]
            ]
        , h3 [] [ text "Parsed output:" ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "12px"
            , style "border-radius" "4px"
            , style "min-height" "40px"
            , style "font-family" "monospace"
            ]
            [ text (viewExpr model.showImplicitParens (parse model.content)) ]
        , h3 [] [ text "Rectified output:" ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "12px"
            , style "border-radius" "4px"
            , style "min-height" "40px"
            , style "font-family" "monospace"
            ]
            [ text (viewExpr model.showImplicitParens (Result.map rectify (parse model.content))) ]
        ]
