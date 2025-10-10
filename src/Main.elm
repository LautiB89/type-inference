module Main exposing (Model, Msg(..), main)

import Browser
import Expr exposing (Expr, fromExpr)
import ExprParser exposing (parse)
import Html exposing (Html, button, div, h2, h3, input, label, li, ol, span, text, textarea, ul)
import Html.Attributes exposing (cols, for, id, rows, style, type_, value)
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
                        , substitution = simplifySubs substitution
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
        Change newInput ->
            { model | state = next (Initial newInput) }

        ToggleImplicitParens ->
            { model | showImplicitParens = not model.showImplicitParens }

        Reset ->
            { model | state = Initial "" }

        Next ->
            { model | state = next model.state }

        Previous ->
            { model | state = previous model.state }



-- VIEW


stepDiv : List (Html Msg) -> Html Msg
stepDiv xs =
    div
        [ style "background" "#f9f9f9"
        , style "padding" "4px"
        , style "border-radius" "8px"
        , style "font-family" "monospace"
        ]
        xs


view : Model -> Html Msg
view model =
    div
        [ style "width" "70%"
        , style "margin" "40px auto"
        , style "font-family" "sans-serif"
        , style "font-size" "16px"
        ]
        [ h2 [] [ text "Algoritmo I" ]
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
        , viewStep model
        ]


stepStateButton : String -> Msg -> Html Msg
stepStateButton s m =
    button
        [ onClick m
        , style "background" "#eee"
        , style "border" "none"
        , style "padding" "8px 16px"
        , style "cursor" "pointer"
        ]
        [ text s ]


stepFooter : List (Html Msg) -> Html Msg
stepFooter =
    div
        [ style "margin-top" "28px"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "flex-end"
        , style "gap" "6px"
        ]


exprTextArea : String -> Html Msg
exprTextArea input =
    textarea
        [ value input
        , onInput Change
        , rows 2
        , cols 60
        , style "margin-bottom" "12px"
        , style "font-size" "16px"
        ]
        []


viewStep : Model -> Html Msg
viewStep model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (case model.state of
            Initial input ->
                [ h3 [] [ text "Escribí tu expresión" ]
                , exprTextArea input
                ]

            ParseErr input ->
                [ h3 [] [ text "Escribí tu expresión" ]
                , exprTextArea input
                , span [] [ text "No se puede reconocer a qué término se corresponde." ]
                ]

            ParseOk data ->
                [ h3 [] [ text "Escribí tu expresión" ]
                , exprTextArea data.input
                , text "El término es"
                , stepDiv [ text <| fromExpr model.showImplicitParens data.parsedExpr ]
                , stepFooter [ stepStateButton "Empezar" Next ]
                ]

            Rectified data ->
                [ h3 [] [ text "1. Rectificar el término" ]
                , text "Decimos que un término está rectificado si:"
                , ul [ style "margin" "2px" ]
                    [ li [] [ text "No hay dos variables ligadas con el mismo nombre." ]
                    , li [] [ text "No hay una variable ligada con el mismo nombre que una variable libre." ]
                    ]
                , div [ style "margin-bottom" "12px" ] [ text "Siempre podemos rectificar un término a través de alpha-renombres." ]
                , text "Término inicial"
                , stepDiv [ text <| fromExpr model.showImplicitParens data.parsedExpr ]
                , div [ style "margin-top" "12px" ] [ text "Término rectificado" ]
                , stepDiv [ text <| fromExpr model.showImplicitParens data.rectExpr ]
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Siguiente" Next
                    ]
                ]

            Annotated data ->
                [ h3 [] [ text "2. Anotar el término" ]
                , text "Producimos un contexto Γ₀ y un término M₀"
                , ul [ style "margin" "2px" ]
                    [ li [] [ text "El contexto Γ₀ le da tipo a todas las variables libres de U." ]
                    , li [] [ text "El término M₀ está anotado de tal modo que Erase(M₀) = U." ]
                    ]
                , div [ style "margin-bottom" "12px" ]
                    [ text "Todos las tipos y las anotaciones que se agregan son incógnitas frescas."
                    ]
                , text "Resultado"
                , stepDiv
                    [ div [] [ text <| "M₀ = " ++ fromTypedExpr model.showImplicitParens data.annotatedExpr ]
                    , div [] [ text <| "Γ₀ = " ++ fromContext data.context ]
                    ]
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Siguiente" Next
                    ]
                ]

            InferErr _ ->
                [ h3 [] [ text "3. Calcular el conjunto de restricciones" ]
                , text "Ocurrió un error inesperado al generar las restricciones."
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Volver a empezar" Reset
                    ]
                ]

            InferOk data ->
                [ h3 [] [ text "3. Calcular el conjunto de restricciones" ]
                , text "Resultado"
                , stepDiv
                    [ div [] [ text <| "τ = " ++ fromType data.exprType ]
                    , div [] [ text <| "E = " ++ fromRestrictions data.restrictions ]
                    ]
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Siguiente" Next
                    ]
                ]

            UnificationErr data ->
                [ h3 [] [ text "4. Unificación" ]
                , text "Dados Γ y M, resultantes de anotar un término rectificado U, una vez calculado I(Γ | M) = (τ | E):"
                , ol [ style "margin" "2px" ]
                    [ li [] [ text "Calculamos S = mgu(E)." ]
                    , li [] [ text "Si no existe el unificador, el término U no es tipable." ]
                    , li [] [ text "Si existe el unificador, el término U es tipable y devolvemos: S(Γ) ⊢ S(M) : S(τ)" ]
                    ]
                , div [] [ text "Resultado" ]
                , stepDiv
                    [ text "El algoritmo de unificación falla con "
                    , text (fromMguError data.mguError)
                    ]
                , div [] [ text "Por lo tanto, el término no es tipable." ]
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Siguiente" Next
                    ]
                ]

            UnificationOk data ->
                [ h3 [] [ text "4. Unificación" ]
                , text "Dados Γ y M, resultantes de anotar un término rectificado U, una vez calculado I(Γ | M) = (τ | E):"
                , ol [ style "margin" "2px" ]
                    [ li [] [ text "Calculamos S = mgu(E)." ]
                    , li [] [ text "Si no existe el unificador, el término U no es tipable." ]
                    , li [] [ text "Si existe el unificador, el término U es tipable y devolvemos: S(Γ) ⊢ S(M) : S(τ)" ]
                    ]
                , div [] [ text "Resultado" ]
                , stepDiv
                    [ text <| "S = MGU(E) = " ++ fromSubstitution data.substitution data.nextFreshN
                    ]
                , div
                    [ style "margin-top" "16px", style "margin-bottom" "8px" ]
                    [ text "Por lo tanto, el término es tipable y su juicio más general es" ]
                , stepDiv
                    [ text <|
                        fromContext data.context
                            ++ " ⊢ "
                            ++ fromTypedExpr model.showImplicitParens (substituteExpr data.substitution data.annotatedExpr)
                            ++ " : "
                            ++ fromType data.exprType
                    ]
                , stepFooter
                    [ stepStateButton "Atrás" Previous
                    , stepStateButton "Volver a empezar" Reset
                    ]
                ]
        )


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
