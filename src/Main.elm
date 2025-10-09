module Main exposing (Model, Msg(..), main)

import Browser
import Dict
import Expr exposing (Expr, fromExpr)
import ExprParser exposing (parse)
import Html exposing (Html, button, div, h2, h4, input, label, text, textarea)
import Html.Attributes exposing (cols, for, id, placeholder, rows, style, type_, value)
import Html.Events exposing (onClick, onInput)
import MinRectify exposing (minRectify)
import Restrictions
    exposing
        ( MguError
        , Restrictions
        , fromMguError
        , fromRestrictions
        , mgu
        )
import Substitution exposing (Substitution, fromSubstitution, simplifySubs, substitute)
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
    { content = ""
    , showImplicitParens = False
    }



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


type Trace
    = ParsingError String
    | InferError
        { inputStr : String
        , plainExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , nextFreshN : Int
        }
    | MguFailed
        MguError
        { inputStr : String
        , plainExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , restrictions : Restrictions
        , exprType : Type
        , nextFreshN : Int
        }
    | MguOk
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


getTrace : String -> Trace
getTrace s =
    case parse s of
        Err _ ->
            ParsingError s

        Ok parsedExpr ->
            let
                rectified : Expr
                rectified =
                    minRectify parsedExpr

                ( context, annotated, n1 ) =
                    decorate rectified

                ( may, n2 ) =
                    infer context annotated n1

                okInferResult =
                    { inputStr = s
                    , plainExpr = parsedExpr
                    , rectExpr = rectified
                    , context = context
                    , annotatedExpr = annotated
                    , nextFreshN = n1
                    }
            in
            case may of
                Nothing ->
                    InferError okInferResult

                Just ( exprType, restrictions ) ->
                    case mgu restrictions of
                        Err mguErr ->
                            MguFailed
                                mguErr
                                { inputStr = s
                                , plainExpr = parsedExpr
                                , rectExpr = rectified
                                , context = context
                                , annotatedExpr = annotated
                                , nextFreshN = n1
                                , restrictions = restrictions
                                , exprType = exprType
                                }

                        Ok substitution ->
                            MguOk
                                { inputStr = s
                                , rectExpr = rectified
                                , plainExpr = parsedExpr
                                , typedExpr = annotated
                                , context = context
                                , restrictions = restrictions
                                , exprType = exprType
                                , substitution = simplifySubs substitution
                                , nextFreshN = n2
                                }


view : Model -> Html Msg
view model =
    let
        ifShowParens a b =
            if model.showImplicitParens then
                a

            else
                b

        trace =
            getTrace model.content
    in
    div
        [ style "width" "70%"
        , style "margin" "40px auto"
        , style "font-family" "sans-serif"
        , style "font-size" "16px"
        ]
        [ h2 [] [ text "Algoritmo I" ]
        , textarea
            [ value model.content
            , onInput Change
            , placeholder "(\\x.x x) (\\y.y y)"
            , rows 2
            , cols 60
            , style "margin-bottom" "12px"
            , style "font-size" "16px"
            ]
            []
        , div []
            [ input
                [ onClick ToggleImplicitParens
                , id "toggleParens"
                , type_ "checkbox"
                ]
                []
            , label
                [ for "toggleParens" ]
                [ text "Mostrar paréntesis implícitos" ]
            ]
        , case trace of
            ParsingError s ->
                stepDiv "Ocurrió un error" [ text ("No se pudo reconocer el término " ++ s) ]

            InferError t ->
                div []
                    [ stepDiv "0. Término sin tipo" [ text (fromExpr model.showImplicitParens t.plainExpr) ]
                    , stepDiv "1. Rectificación" [ text (fromExpr model.showImplicitParens t.rectExpr) ]
                    , stepDiv "2. Anotación"
                        [ div [] [ text ("M₀: " ++ fromTypedExpr model.showImplicitParens t.annotatedExpr) ]
                        , div [] [ text ("Γ₀: " ++ fromContext t.context) ]
                        ]
                    , stepDiv "3. Generación de restricciones"
                        [ div [] [ text "Error inesperado en el algoritmo de inferencia" ]
                        ]
                    ]

            MguFailed mguErr t ->
                div []
                    [ stepDiv "0. Término sin tipo" [ text (fromExpr model.showImplicitParens t.plainExpr) ]
                    , stepDiv "1. Rectificación" [ text (fromExpr model.showImplicitParens t.rectExpr) ]
                    , stepDiv "2. Anotación"
                        [ div [] [ text ("M₀: " ++ fromTypedExpr model.showImplicitParens t.annotatedExpr) ]
                        , div [] [ text ("Γ₀: " ++ fromContext t.context) ]
                        ]
                    , stepDiv "3. Generación de restricciones"
                        [ div [] [ text ("Tipo: " ++ fromType t.exprType) ]
                        , div [] [ text ("Restricciones: " ++ fromRestrictions t.restrictions) ]
                        ]
                    , stepDiv "4. Unificación"
                        [ div [] [ text (fromMguError mguErr) ] ]
                    ]

            MguOk t ->
                div []
                    [ stepDiv "0. Término sin tipo" [ text (fromExpr model.showImplicitParens t.plainExpr) ]
                    , stepDiv "1. Rectificación" [ text (fromExpr model.showImplicitParens t.rectExpr) ]
                    , stepDiv "2. Anotación"
                        [ div [] [ text ("M₀: " ++ fromTypedExpr model.showImplicitParens t.typedExpr) ]
                        , div [] [ text ("Γ₀: " ++ fromContext t.context) ]
                        ]
                    , stepDiv "3. Generación de restricciones"
                        [ div [] [ text ("Tipo: " ++ fromType t.exprType) ]
                        , div [] [ text ("Restricciones: " ++ fromRestrictions t.restrictions) ]
                        ]
                    , stepDiv "4. Unificación"
                        [ div [] [ text ("Sustitución: " ++ fromSubstitution t.substitution t.nextFreshN) ] ]
                    , stepDiv "Resultado"
                        [ div []
                            [ text
                                (fromContext (Dict.map (\_ t1 -> substitute t.substitution t1) t.context)
                                    ++ (" ⊢ " ++ fromTypedExpr model.showImplicitParens (substituteExpr t.substitution t.typedExpr))
                                    ++ (" : " ++ fromType (substitute t.substitution t.exprType))
                                )
                            ]
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
