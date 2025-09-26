module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (Parser, DeadEnd, (|.), (|=), succeed, symbol, float, spaces, run, oneOf)
import String exposing (fromInt)
import Parser exposing (int)
import Parser exposing (keyword)
import Parser exposing (lazy)

-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Recec = Base | Next Recec

type alias Model =
  { content : String  }

type NatExpr = 
    ConstZero | 
    Succ NatExpr | 
    Pred NatExpr 

type BoolExpr =
    ConstTrue | 
    ConstFalse | 
    IsZero NatExpr

type alias Id = Int

type Expr = 
    Var Id | 
    Abs Id Expr |
    App Expr Expr  |
    Bool BoolExpr |
    Nat NatExpr |
    If Expr Expr Expr

natExpr : Parser NatExpr
natExpr =
  oneOf [
    succeed ConstZero
      |. symbol "zero",
    succeed Succ
      |. keyword "succ"
      |. symbol "("
      |= lazy (\_ -> natExpr)
      |. symbol ")",
    succeed Pred
      |. keyword "pred"
      |. symbol "("
      |= lazy (\_ -> natExpr)
      |. symbol ")"
  ]

boolExpr : Parser BoolExpr
boolExpr =
  oneOf 
    [ succeed ConstTrue
        |. keyword "True"
    , succeed ConstFalse
        |. keyword "False"
    , succeed IsZero
        |. keyword "isZero"
        |. symbol "("
        |= lazy (\_ -> natExpr)
        |. symbol ")"
    ]

lambdaExpr : Parser Expr
lambdaExpr =
  let
    varExpr = Parser.map Var int
    absExpr =
      succeed Abs
        |. symbol "("
        |. symbol "\\"
        |= int
        |. spaces
        |. keyword "->"
        |. spaces
        |= lazy (\_ -> lambdaExpr)
        |. spaces
        |. symbol ")"
    appExpr = 
      succeed App
        |. symbol "("
        |= lazy (\_ -> lambdaExpr)
        |. spaces
        |= lazy (\_ -> lambdaExpr)
        |. symbol ")"

    ifExpr =
      succeed If
        |. keyword "if"
        |. spaces
        |= lazy (\_ -> lambdaExpr)
        |. spaces
        |. keyword "then"
        |. spaces
        |= lazy (\_ -> lambdaExpr)
        |. spaces
        |. keyword "else"
        |. spaces
        |= lazy (\_ -> lambdaExpr)

  in
    oneOf [
      varExpr,
      absExpr,
      appExpr,
      Parser.map Bool boolExpr,
      Parser.map Nat natExpr,
      ifExpr
    ]

init : Model
init =
  { content = "" }

-- UPDATE

type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW

type Boolean
  = MyTrue
  | MyFalse
  | MyOr Boolean Boolean

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to parse", value model.content, onInput Change ] []
    , div [] [ text (viewExpr (run lambdaExpr model.content)) ]
    ]

viewExpr : Result (List DeadEnd) Expr -> String
viewExpr res = case res of 
    Ok expr -> fromExpr expr
    Err _ -> "Falló el parsing"

fromNat : NatExpr -> String
fromNat expr = case expr of
  ConstZero -> "0"
  Succ expr2 -> "succ(" ++ fromNat expr2 ++ ")"
  Pred expr2 -> "pred(" ++ fromNat expr2 ++ ")"

fromBool : BoolExpr -> String
fromBool expr = case expr of
  ConstTrue -> "True"
  ConstFalse -> "False"
  IsZero natExpr1 -> "isZero(" ++ fromNat natExpr1 ++ ")"

fromExpr : Expr -> String
fromExpr expr = case expr of
  Var id ->  "x" ++ fromInt id
  Abs id expr2 -> "(\\x" ++ fromInt id ++ " -> " ++ fromExpr expr2 ++ ")"
  App expr1 expr2  ->  fromExpr expr1 ++ " " ++ fromExpr expr2 
  Bool boolExpr1 -> fromBool boolExpr1 
  Nat natExpr1 -> fromNat natExpr1
  If expr1 expr2 expr3 -> 
    "if " ++ 
      fromExpr expr1 ++ 
      " then " ++ 
      fromExpr expr2 ++
      " else " ++ 
      fromExpr expr3
