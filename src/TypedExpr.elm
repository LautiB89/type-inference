module TypedExpr exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Expr exposing (Expr(..), Id)
import NaiveRectify exposing (freeExprVars)
import String exposing (fromInt)
type Type
    = TVar Int
    | TNat
    | TBool
    | TAbs Type Type


type TypedExpr
    = TEVar Id
    | TEAbs Id Type TypedExpr
    | TEApp TypedExpr TypedExpr
    | TEConstTrue
    | TEConstFalse
    | TEIsZero TypedExpr
    | TEConstZero
    | TESucc TypedExpr
    | TEPred TypedExpr
    | TEIf TypedExpr TypedExpr TypedExpr


type alias Context =
    Dict Id Type


type Restriction
    = Unifies Type Type


type alias AlgorithmIRes =
    { typedExpr : TypedExpr
    , exprType : Type
    }


type alias AlgorithmICtx =
    { context : Context
    , setRestriction : Set Restriction
    , nextFreshVar : Int
    }


exprContext : Expr -> ( Int, Context )
exprContext expr =
    freeExprVars expr
        |> Set.foldr (\x ( n, d ) -> ( n + 1, Dict.insert x (TVar n) d )) ( 1, Dict.empty )


decorate : Expr -> ( Context, TypedExpr )
decorate expr =
    let
        ( n, context ) =
            exprContext expr
    in
    (context, Tuple.first (decorateHelper expr n) )



decorateHelper : Expr -> Int -> ( TypedExpr, Int )
decorateHelper expr n = case expr of
    Var id -> (TEVar id, n)
    Abs id e -> 
        let
            (rec, n2) = decorateHelper e n
        in
        (TEAbs id (TVar n2) rec, n2 + 1)
        
    App e1 e2 ->
        let 
            (rec1, n1) = decorateHelper e1 n
        
            (rec2, n2) = decorateHelper e2 n1
        in
            (TEApp rec1 rec2, n2)

    ConstTrue -> (TEConstTrue, n)
    ConstFalse -> (TEConstFalse, n)

    IsZero e ->
        let
            (rec, n1) = decorateHelper e n
        in
            (TEIsZero rec, n1)

    ConstZero -> (TEConstZero, n)

    Succ e ->
        let
            (rec, n1) = decorateHelper e n
        in
            (TESucc rec, n1)

    Pred e ->
        let
            (rec, n1) = decorateHelper e n
        in
            (TEPred rec, n1)


    If e1 e2 e3 ->
        let 
            (rec1, n1) = decorateHelper e1 n
        
            (rec2, n2) = decorateHelper e2 n1
            (rec3, n3) = decorateHelper e3 n2
        in
            (TEIf rec1 rec2 rec3, n3)



foldrTypedExpr :
    (Id -> a) -- Var
    -> (Id -> Type -> a -> a) -- Abs
    -> (a -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (a -> a) -- IsZero
    -> a -- ConstZero
    -> (a -> a) -- Succ
    -> (a -> a) -- Pred
    -> (a -> a -> a -> a) -- If
    -> TypedExpr
    -> a
foldrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            foldrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        TEVar id ->
            fVar id

        TEAbs id t e ->
            fAbs id t (rec e)

        TEApp e1 e2 ->
            fApp (rec e1) (rec e2)

        TEConstTrue ->
            fTrue

        TEConstFalse ->
            fFalse

        TEIsZero e ->
            fIsZero (rec e)

        TEConstZero ->
            fZero

        TESucc e ->
            fSucc (rec e)

        TEPred e ->
            fPred (rec e)

        TEIf e1 e2 e3 ->
            fIf (rec e1) (rec e2) (rec e3)


recrTypedExpr :
    (Id -> a) -- Var
    -> (Id -> Type -> TypedExpr -> a -> a) -- Abs
    -> (TypedExpr -> a -> TypedExpr -> a -> a) -- App
    -> a -- ConstTrue
    -> a -- ConstFalse
    -> (TypedExpr -> a -> a) -- IsZero
    -> a -- ConstZero
    -> (TypedExpr -> a -> a) -- Succ
    -> (TypedExpr -> a -> a) -- Pred
    -> (TypedExpr -> a -> TypedExpr -> a -> TypedExpr -> a -> a) -- If
    -> TypedExpr
    -> a
recrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf expr =
    let
        rec =
            recrTypedExpr fVar fAbs fApp fTrue fFalse fIsZero fZero fSucc fPred fIf
    in
    case expr of
        TEVar id ->
            fVar id

        TEAbs id t e ->
            fAbs id t e (rec e)

        TEApp e1 e2 ->
            fApp e1 (rec e1) e2 (rec e2)

        TEConstTrue ->
            fTrue

        TEConstFalse ->
            fFalse

        TEIsZero e ->
            fIsZero e (rec e)

        TEConstZero ->
            fZero

        TESucc e ->
            fSucc e (rec e)

        TEPred e ->
            fPred e (rec e)

        TEIf e1 e2 e3 ->
            fIf e1 (rec e1) e2 (rec e2) e3 (rec e3)


fromTypedExpr : Bool -> TypedExpr -> String
fromTypedExpr showImplicitParens =
    recrTypedExpr
        (\id -> id)
        (\id t _ rec -> "(λ" ++ id ++ " : " ++ fromType t ++ " . " ++ rec ++ ")")
        (\e1 rec1 e2 rec2 ->
            maybeParens rec1 (isApp e1 && showImplicitParens) ++ " " ++ maybeParens rec2 (isApp e2)
        )
        "true"
        "false"
        (\_ rec -> "isZero(" ++ rec ++ ")")
        "0"
        (\_ rec -> "succ(" ++ rec ++ ")")
        (\_ rec -> "pred(" ++ rec ++ ")")
        (\_ rec1 _ rec2 _ rec3 ->
            "if "
                ++ rec1
                ++ " then "
                ++ rec2
                ++ " else "
                ++ rec3
        )

fromType : Type -> String
fromType t = case t of
    TVar n -> "X" ++ fromInt n 
    TNat -> "Nat" 
    TBool -> "Bool"
    TAbs t1 t2 -> (fromType t1) ++ " -> " ++ (fromType t2) 

maybeParens : String -> Bool -> String
maybeParens s b =
    if b then
        "(" ++ s ++ ")"

    else
        s

isApp : TypedExpr -> Bool
isApp expr =
    case expr of
        TEApp _ _ ->
            True

        _ ->
            False
