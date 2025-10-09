module Main exposing (Model, Msg(..), main)

import Browser
import Dict
import Expr exposing (Expr, fromExpr)
import ExprParser exposing (parse)
import Html exposing (Html, button, div, h2, h4, text, textarea)
import Html.Attributes exposing (cols, placeholder, rows, style, value)
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


type State
    = Initial String
    | ParseErr String
    | ParseOk { input : String, parsedExpr : Expr }
    | Rectified { input : String, parsedExpr : Expr, rectExpr : Expr }
    | Annotated
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , nextFreshN : Int
        }
    | InferErr
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , nextFreshN : Int
        }
    | InferOk
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , exprType : Type
        , restrictions : Restrictions
        , nextFreshN : Int
        }
    | UnificationErr
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , exprType : Type
        , restrictions : Restrictions
        , mguError : MguError
        , nextFreshN : Int
        }
    | UnificationOk
        { input : String
        , parsedExpr : Expr
        , rectExpr : Expr
        , context : Context
        , annotatedExpr : TypedExpr
        , exprType : Type
        , restrictions : Restrictions
        , substitution : Substitution
        , nextFreshN : Int
        }


type alias Model =
    { showImplicitParens : Bool
    , state : State
    }


init : Model
init =
    { showImplicitParens = False
    , state = Initial ""
    }



-- UPDATE


type Msg
    = Change String
    | ToggleImplicitParens
    | Reset
    | Previous
    | Next


next : State -> State
next step =
    case step of
        Initial input ->
            parse input
                |> Result.map (\expr -> ParseOk { input = input, parsedExpr = expr })
                |> Result.withDefault (ParseErr input)

        ParseErr input ->
            ParseErr input

        ParseOk { input, parsedExpr } ->
            Rectified
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = minRectify parsedExpr
                }

        Rectified { input, parsedExpr, rectExpr } ->
            let
                ( context, annotatedExpr, nextFreshN ) =
                    decorate rectExpr
            in
            Annotated
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , nextFreshN = nextFreshN
                }

        Annotated data ->
            let
                ( maybeRes, nextFreshN ) =
                    infer data.context data.annotatedExpr data.nextFreshN
            in
            case maybeRes of
                Just ( exprType, restrictions ) ->
                    InferOk
                        { input = data.input
                        , parsedExpr = data.parsedExpr
                        , rectExpr = data.rectExpr
                        , context = data.context
                        , annotatedExpr = data.annotatedExpr
                        , exprType = exprType
                        , restrictions = restrictions
                        , nextFreshN = nextFreshN
                        }

                Nothing ->
                    InferErr
                        { input = data.input
                        , parsedExpr = data.parsedExpr
                        , rectExpr = data.rectExpr
                        , context = data.context
                        , annotatedExpr = data.annotatedExpr
                        , nextFreshN = nextFreshN
                        }

        InferErr data ->
            InferErr data

        InferOk data ->
            let
                mguRes =
                    mgu data.restrictions
            in
            case mguRes of
                Ok substitution ->
                    UnificationOk
                        { input = data.input
                        , parsedExpr = data.parsedExpr
                        , rectExpr = data.rectExpr
                        , context = data.context
                        , annotatedExpr = data.annotatedExpr
                        , exprType = data.exprType
                        , restrictions = data.restrictions
                        , nextFreshN = data.nextFreshN
                        , substitution = substitution
                        }

                Err mguErr ->
                    UnificationErr
                        { input = data.input
                        , parsedExpr = data.parsedExpr
                        , rectExpr = data.rectExpr
                        , context = data.context
                        , annotatedExpr = data.annotatedExpr
                        , exprType = data.exprType
                        , restrictions = data.restrictions
                        , nextFreshN = data.nextFreshN
                        , mguError = mguErr
                        }

        UnificationErr e ->
            UnificationErr e

        UnificationOk data ->
            UnificationOk data


previous : State -> State
previous step =
    case step of
        Initial _ ->
            step

        ParseErr input ->
            Initial input

        ParseOk { input } ->
            Initial input

        Rectified { input, parsedExpr } ->
            ParseOk { input = input, parsedExpr = parsedExpr }

        Annotated { input, parsedExpr, rectExpr } ->
            Rectified { input = input, parsedExpr = parsedExpr, rectExpr = rectExpr }

        InferErr { input, parsedExpr, rectExpr, context, annotatedExpr, nextFreshN } ->
            Annotated
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , nextFreshN = nextFreshN
                }

        InferOk { input, parsedExpr, rectExpr, context, annotatedExpr, nextFreshN } ->
            Annotated
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , nextFreshN = nextFreshN
                }

        UnificationErr { input, parsedExpr, rectExpr, context, annotatedExpr, exprType, restrictions, nextFreshN } ->
            InferOk
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , exprType = exprType
                , restrictions = restrictions
                , nextFreshN = nextFreshN
                }

        UnificationOk { input, parsedExpr, rectExpr, context, annotatedExpr, exprType, restrictions, nextFreshN } ->
            InferOk
                { input = input
                , parsedExpr = parsedExpr
                , rectExpr = rectExpr
                , context = context
                , annotatedExpr = annotatedExpr
                , exprType = exprType
                , restrictions = restrictions
                , nextFreshN = nextFreshN
                }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { content = newContent, state = next (Initial newContent) }

        ToggleImplicitParens ->
            { model | showImplicitParens = not model.showImplicitParens }

        Reset ->
            { model | state = Initial "" }

        Next ->
            { model | state = next model.state }

        Previous ->
            { model | state = previous model.state }



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
