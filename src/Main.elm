module Main exposing (..)

import Browser
import Dict
import Expr exposing (Expr, fromExpr)
import Html exposing (Html, button, div, h2, h4, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import LambdaParser exposing (parse)
import MinRectify exposing (minRectify)
import Restrictions
    exposing
        ( Restrictions
        , Substitution
        , fromRestrictions
        , fromSubstitution
        , mgu
        , simplifySubstitution
        , substitute
        )
import Type exposing (Type(..), fromType)
import TypedExpr exposing (Context, TypedExpr(..), decorate, foldrTypedExpr, fromContext, fromTypedExpr, infer)



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


stepDiv : String -> List (Html Msg) -> Html Msg
stepDiv title xs =
    div []
        [ h4 [] [ text title ]
        , div
            [ style "background" "#f9f9f9"
            , style "padding" "4px"
            , style "border-radius" "4px"
            , style "min-height" "40px"
            , style "font-family" "monospace"
            , style "display" "flex"
            , style "flex-direction" "column"
            ]
            xs
        ]


type alias FullTrace =
    { str : String
    , rectified : Expr
    , untyped : Expr
    , typed : TypedExpr
    , ctx : Context
    , res : Restrictions
    , t : Type
    , sus : Substitution
    , nextFreshN : Int
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
                                , rectified = rectified
                                , untyped = expr
                                , typed = typed
                                , ctx = context
                                , res = r
                                , t = t
                                , sus = simplifySubstitution sus
                                , nextFreshN = n1
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
        , style "font-size" "16px"
        ]
        [ h2 [] [ text "Algoritmo I" ]
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
                    (ifShowParens "Esconder" "Mostrar" ++ " paréntesis implícitos")
                ]
            ]
        , case aaa of
            Err err ->
                stepDiv "Ocurrió un error" [ text err ]

            Ok { ctx, rectified, res, sus, t, typed, untyped, nextFreshN } ->
                div []
                    [ stepDiv "0. Término sin tipo" [ text (fromExpr model.showImplicitParens untyped) ]
                    , stepDiv "1. Rectificación" [ text (fromExpr model.showImplicitParens rectified) ]
                    , stepDiv "2. Anotación"
                        [ div [] [ text ("M0: " ++ fromTypedExpr model.showImplicitParens typed) ]
                        , div [] [ text ("Γ0: " ++ fromContext ctx) ]
                        ]
                    , stepDiv "3. Generación de restricciones"
                        [ div [] [ text ("Tipo: " ++ fromType t) ]
                        , div [] [ text ("Restricciones: " ++ fromRestrictions res) ]
                        ]
                    , stepDiv "4. Unificación"
                        [ div [] [ text ("Sustitución: " ++ fromSubstitution sus nextFreshN) ] ]
                    , stepDiv "Resultado"
                        [ div [] [ text ("Γ: " ++ fromContext (Dict.map (\_ t1 -> substitute sus t1) ctx)) ]
                        , div [] [ text ("M: " ++ fromTypedExpr model.showImplicitParens (substituteExpr sus typed)) ]
                        , div [] [ text ("Tipo: " ++ fromType (substitute sus t)) ]
                        ]
                    ]
        ]


substituteExpr : Substitution -> TypedExpr -> TypedExpr
substituteExpr s =
    foldrTypedExpr
        TEVar
        (\id t rec -> TEAbs id (substitute s t) rec)
        TEApp
        TEConstTrue
        TEConstFalse
        TEIsZero
        TEConstZero
        TESucc
        TEPred
        TEIf
