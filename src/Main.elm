module Main exposing (..)

import Browser
import Expr exposing (Expr, fromExpr)
import Html exposing (Html, button, div, h2, h4, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import LambdaParser exposing (parse)
import MinRectify exposing (minRectify)
import Restrictions exposing (Restrictions, Substitution, fromRestrictions, mgu, simplifySubstitution)
import Type exposing (Type, fromType)
import TypedExpr exposing (Context, TypedExpr, decorate, fromContext, fromTypedExpr, infer)



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


expressionViewer : String -> String -> Html Msg
expressionViewer title s =
    div []
        [ h4 [] [ text title ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "12px"
            , style "border-radius" "4px"
            , style "min-height" "40px"
            , style "font-family" "monospace"
            ]
            [ text s ]
        ]


type alias FullTrace =
    { str : String
    , untyped : Expr
    , typed : TypedExpr
    , ctx : Context
    , res : Restrictions
    , t : Type
    , sus : Substitution
    }


fullTrace : String -> Result String FullTrace
fullTrace s =
    case parse s of
        Err _ ->
            Err "err"

        Ok expr ->
            let
                rectified : Expr
                rectified =
                    minRectify expr

                ( context, typed, n1 ) =
                    decorate rectified

                may =
                    infer typed context n1
            in
            case may of
                Nothing ->
                    Err ""

                Just ( t, r, _ ) ->
                    Result.andThen
                        (\sus ->
                            Ok
                                { str = s
                                , untyped = expr
                                , typed = typed
                                , ctx = context
                                , res = r
                                , t = t
                                , sus = simplifySubstitution sus
                                }
                        )
                        (mgu r)


view : Model -> Html Msg
view model =
    let
        ifShowParens a b =
            if model.showImplicitParens then
                a

            else
                b

        aaa =
            fullTrace model.content
    in
    div
        [ style "max-width" "600px"
        , style "margin" "40px auto"
        , style "font-family" "sans-serif"
        , style "font-size" "24px"
        ]
        [ h2 [] [ text "λ-Calculus parser" ]
        , textarea
            [ value model.content
            , onInput Change
            , placeholder "(\\x.x x) (\\y.y y)"
            , rows 4
            , cols 60
            , style "margin-bottom" "12px"
            , style "font-size" "16px"
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
        , case aaa of
            Err err ->
                div
                    [ style "background" "#f9f9f9"
                    , style "padding" "12px"
                    , style "border-radius" "4px"
                    , style "min-height" "40px"
                    , style "font-family" "monospace"
                    ]
                    [ text err ]

            Ok { ctx, res, str, sus, t, typed, untyped } ->
                div []
                    [ expressionViewer "Untyped term:" (fromExpr model.showImplicitParens untyped)
                    , expressionViewer "Typed term:" (fromTypedExpr model.showImplicitParens typed)
                    , expressionViewer "Type:" (fromType t)
                    , expressionViewer "Context:" (fromContext ctx)
                    , expressionViewer "Restrictions:" (fromRestrictions res)
                    ]
        ]
