module Main exposing (AlgoIError(..), FullTrace, Model, Msg(..), main)

import Browser
import Dict
import Expr exposing (Expr, fromExpr)
import Html exposing (Html, button, div, h2, h4, text, textarea)
import Html.Attributes exposing (cols, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import LambdaParser exposing (parse)
import MinRectify exposing (minRectify)
import Restrictions
    exposing
        ( MguError
        , Restrictions
        , Substitution
        , fromMguError
        , fromRestrictions
        , fromSubstitution
        , mgu
        , simplifySubstitution
        , substitute
        )
import Type exposing (Type, fromType)
import TypedExpr
    exposing
        ( Context
        , TypedExpr(..)
        , decorate
        , foldrTypedExpr
        , fromContext
        , fromTypedExpr
        , infer
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { content : String
    , showImplicitParens : Bool
    }


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
    { inputStr : String
    , rectExpr : Expr
    , plainExpr : Expr
    , typedExpr : TypedExpr
    , context : Context
    , restrictions : Restrictions
    , exprType : Type
    , substitution : Substitution
    , nextFreshN : Int
    }


type AlgoIError
    = ParsingError
    | UnexpectedInferError
    | MguErr MguError


showAlgoIError : AlgoIError -> String
showAlgoIError err =
    case err of
        ParsingError ->
            "Error de parsing"

        UnexpectedInferError ->
            "Error inesperado durante la inferencia"

        MguErr mguErr ->
            "MGU falló: " ++ fromMguError mguErr


fullTrace : String -> Result AlgoIError FullTrace
fullTrace s =
    case parse s of
        Err _ ->
            Err ParsingError

        Ok parsedExpr ->
            let
                rectified : Expr
                rectified =
                    minRectify parsedExpr

                ( context, typed, n1 ) =
                    decorate rectified

                ( may, _ ) =
                    infer context typed n1
            in
            case may of
                Nothing ->
                    Err UnexpectedInferError

                Just ( exprType, restrictions ) ->
                    Result.mapError MguErr
                        (Result.map
                            (\sus ->
                                { inputStr = s
                                , rectExpr = rectified
                                , plainExpr = parsedExpr
                                , typedExpr = typed
                                , context = context
                                , restrictions = restrictions
                                , exprType = exprType
                                , substitution = simplifySubstitution sus
                                , nextFreshN = n1
                                }
                            )
                            (mgu restrictions)
                        )


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
                stepDiv "Ocurrió un error" [ text (showAlgoIError err) ]

            Ok { context, rectExpr, restrictions, substitution, exprType, typedExpr, plainExpr, nextFreshN } ->
                div []
                    [ stepDiv "0. Término sin tipo" [ text (fromExpr model.showImplicitParens plainExpr) ]
                    , stepDiv "1. Rectificación" [ text (fromExpr model.showImplicitParens rectExpr) ]
                    , stepDiv "2. Anotación"
                        [ div [] [ text ("M0: " ++ fromTypedExpr model.showImplicitParens typedExpr) ]
                        , div [] [ text ("Γ0: " ++ fromContext context) ]
                        ]
                    , stepDiv "3. Generación de restricciones"
                        [ div [] [ text ("Tipo: " ++ fromType exprType) ]
                        , div [] [ text ("Restricciones: " ++ fromRestrictions restrictions) ]
                        ]
                    , stepDiv "4. Unificación"
                        [ div [] [ text ("Sustitución: " ++ fromSubstitution substitution nextFreshN) ] ]
                    , stepDiv "Resultado"
                        [ div [] [ text ("Γ: " ++ fromContext (Dict.map (\_ t1 -> substitute substitution t1) context)) ]
                        , div [] [ text ("M: " ++ fromTypedExpr model.showImplicitParens (substituteExpr substitution typedExpr)) ]
                        , div [] [ text ("Tipo: " ++ fromType (substitute substitution exprType)) ]
                        ]
                    ]
        ]


substituteExpr : Substitution -> TypedExpr -> TypedExpr
substituteExpr s =
    foldrTypedExpr
        TEVar
        (\id t -> TEAbs id (substitute s t))
        TEApp
        TEConstTrue
        TEConstFalse
        TEIsZero
        TEConstZero
        TESucc
        TEPred
        TEIf
