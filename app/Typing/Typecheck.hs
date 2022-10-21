{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Typing.Typecheck where

import Typing.Syntax ( Sign, TypId, Lvl )
import Typing.TCM ( TCM, complain, fresh, getVar, adjustVar, VarState(..)
    , setUpper, setLower, getUpper, getLower, unique, setLvl, newVar, redirect, getLvl, getTypId )
import qualified Typing.Syntax as Syn
import Surface.AST ( Name(..) )
import qualified Surface.AST as Sur

import qualified Data.Map as M
import Data.Functor ( (<&>), ($>) )

import Control.Monad ( when, forM_, forM, mapM, join )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, put, modify )

import Debug.Trace

--- Let Polymorphism ---
data Scheme = Mono Syn.Type
            | Poly !Lvl Syn.Type

instantiate :: Scheme -> TCM Syn.Type
instantiate (Mono typ) = pure typ

instantiate (Poly lvl typ) = evalStateT (instPoly lvl typ) []
    where
    -- TODO instantiate metavars in coercions? or not allow coercions at all...!
    instPoly :: Lvl -> Syn.Type -> StateT [(TypId, Syn.Type)] TCM Syn.Type
    instPoly lvl (Syn.Tvar tid) = do
        tid <- lift $ getTypId tid
        vs <- lift $ getVar tid
        curr <- get
        if level vs > lvl then do
            case lookup tid curr of
                Nothing -> do
                    name <- lift $ unique $ name vs

                    up <- mapM (instPoly lvl) (upper vs)
                    lo <- mapM (instPoly lvl) (lower vs)
                    insted <- lift $ newVar $ VS name lvl up lo

                    modify ((tid, insted):)
                    pure insted
                Just insted -> pure insted
        else pure (Syn.Tvar tid)

    instPoly lvl (Syn.Arrow ltyp rtyp) = Syn.Arrow <$> instPoly lvl ltyp <*> instPoly lvl rtyp
    instPoly lvl (Syn.RecordType fields) = Syn.RecordType <$> forM fields (\(lbl, typ) -> (lbl,) <$> instPoly lvl typ)

    instPoly lvl Syn.Nat = pure Syn.Nat
    instPoly lvl Syn.Int = pure Syn.Int
    instPoly lvl Syn.Real = pure Syn.Real
    instPoly lvl Syn.Bool = pure Syn.Bool


--- Inference ---
type Ctx = [(Name, Scheme)]

infer :: Ctx -> Lvl -> Sur.Expr -> TCM (Syn.Expr, Syn.Type)
infer ctx lvl (Sur.Var name) = case lookup name ctx of
    Nothing -> complain $ "undefined variable " <> show name
    Just typ -> do
        typ <- instantiate typ
        pure (Syn.Var name, typ)

infer ctx lvl (Sur.Let name body rest) = do
    (body, btyp) <- infer ctx (lvl + 1) body
    (rest, typ) <- infer ((name, Poly lvl btyp) : ctx) lvl rest
    pure (Syn.Let name btyp body rest, typ)

infer ctx lvl (Sur.Lam name body) = do
    xtyp <- fresh name lvl
    (body, btyp) <- infer ((name, Mono xtyp) : ctx) lvl body
    pure (Syn.Lam name xtyp body, Syn.Arrow xtyp btyp)

infer ctx lvl (Sur.App fnc arg) = do
    (fnc, ftyp) <- infer ctx lvl fnc
    (arg, atyp) <- infer ctx lvl arg
    name <- unique (N "r")
    rtyp <- fresh name lvl
    coerce <- constrain ftyp (Syn.Arrow atyp rtyp)
    pure (Syn.App (coerce fnc) arg, rtyp)

infer ctx lvl (Sur.Record entries) = do
    (entries, fields) <- unzip <$> mapM (\(lbl, val) -> do
        (val, typ) <- infer ctx lvl val
        pure ((lbl, typ, val), (lbl, typ))
        ) entries
    pure (Syn.Record entries, Syn.RecordType fields)

infer ctx lvl (Sur.Proj rcd lbl) = do
    (rcd, rtyp) <- infer ctx lvl rcd
    typ <- fresh (N "l") lvl
    coerce <- constrain rtyp (Syn.RecordType [(lbl, typ)])
    pure (Syn.Proj (coerce rcd) lbl, typ)

infer ctx lvl (Sur.NatLit n) = pure (Syn.NatLit n, Syn.Nat)
infer ctx lvl (Sur.IntLit n) = pure (Syn.IntLit n, Syn.Int)
infer ctx lvl (Sur.RealLit n) = pure (Syn.RealLit n, Syn.Real)
infer ctx lvl (Sur.BoolLit n) = pure (Syn.BoolLit n, Syn.Bool)


--- Constraints ---
constrain :: Syn.Type -> Syn.Type -> TCM (Syn.Expr -> Syn.Expr)
constrain lhs rhs | lhs == rhs = pure id

constrain (Syn.Tvar tid) (Syn.Tvar tid') = do
    unify tid tid'
    pure id

constrain (Syn.Tvar tid) typ = do
    lvl <- getLvl tid
    extrude lvl typ -- move into `addUpper`?

    addUpper typ tid

    lower <- getLower tid
    forM_ lower (`constrain` typ) -- repeats check in `addUpper`?
    pure $ Syn.Coerce (Syn.Tvar tid) typ

constrain typ (Syn.Tvar tid) = do
    lvl <- getLvl tid
    extrude lvl typ

    addLower typ tid

    upper <- getUpper tid
    forM_ upper (typ `constrain`)
    pure $ Syn.Coerce typ (Syn.Tvar tid)

constrain (Syn.Arrow ltyp rtyp) (Syn.Arrow ltyp' rtyp') = do
    lcoerce <- constrain ltyp' ltyp -- ignore actual result of these,
    rcoerce <- constrain rtyp rtyp' -- they're delayed till zonking.
                                    -- currently changing this though

    name <- unique (N "x")
    pure $ \f -> Syn.Lam name ltyp' (rcoerce $ Syn.App f $ lcoerce (Syn.Var name))

    -- pure $ Syn.Coerce (Syn.Arrow ltyp rtyp) (Syn.Arrow ltyp' rtyp')

constrain (Syn.RecordType fields) (Syn.RecordType fields') = do
    coerces <- constrainRecords fields fields'
    pure $ \f -> Syn.Record $ zipWith (\(name, typ) coe -> (name, typ, coe f)) fields' coerces

    where
    constrainRecords :: [(Name, Syn.Type)] -> [(Name, Syn.Type)] -> TCM [Syn.Expr -> Syn.Expr]
    constrainRecords fields [] = pure []
    constrainRecords fields ((lbl, ftyp') : fields') =
        case lookup lbl fields of
            Nothing -> complain $ "record type missing field " <> show lbl
            Just ftyp -> do
                fcoerce <- constrain ftyp ftyp'
                rest <- constrainRecords fields fields'
                pure $ (\val -> fcoerce (Syn.Proj val lbl)) : rest

constrain Syn.Nat Syn.Int = pure $ Syn.Coerce Syn.Nat Syn.Int
constrain Syn.Nat Syn.Real = pure $ Syn.Coerce Syn.Int Syn.Real . Syn.Coerce Syn.Nat Syn.Int
constrain Syn.Int Syn.Real = pure $ Syn.Coerce Syn.Int Syn.Real

constrain _ _ = complain "uncoercable"

-- TODO: need to adjust levels?
unify :: TypId -> TypId -> TCM ()
unify tid tid' = do
    lo' <- getLower tid'
    up' <- getUpper tid'
    forM_ lo' $ occurs tid -- repeats check in `addLower` and `addUpper`?
    forM_ up' $ occurs tid

    lo <- getLower tid
    up <- getUpper tid
    forM_ lo (`addLower` tid')
    forM_ up (`addUpper` tid')

    redirect tid tid'

--- Type Variable Operations ---
-- TODO: need to adjust levels?
addUpper :: Syn.Type -> TypId -> TCM ()
addUpper typ tid = do
    occurs tid typ

    up <- getUpper tid
    newUp <- maybe (pure typ) (infimum typ) up
    setUpper newUp tid

    lo <- getLower tid
    mapM_ (`constrain` typ) lo

addLower :: Syn.Type -> TypId -> TCM ()
addLower typ tid = do
    occurs tid typ

    lo <- getLower tid
    newLo <- maybe (pure typ) (supremum typ) lo
    setLower newLo tid

    up <- getUpper tid
    mapM_ (typ `constrain`) up

occurs :: Syn.TypId -> Syn.Type -> TCM ()
occurs tid (Syn.Tvar tid')
    | tid == tid' = do
        nm <- name <$> getVar tid
        complain $ "recursive types unsupported: " <> show nm
    | otherwise = do
        up <- getUpper tid'
        lo <- getLower tid'
        forM_ up (occurs tid)
        forM_ lo (occurs tid)
occurs tid (Syn.Arrow ltyp rtyp) = occurs tid ltyp >> occurs tid rtyp
occurs tid (Syn.RecordType fields) = forM_ fields (occurs tid . snd)

occurs _ Syn.Nat = pure ()
occurs _ Syn.Int = pure ()
occurs _ Syn.Real = pure ()
occurs _ Syn.Bool = pure ()

extrude :: Lvl -> Syn.Type -> TCM ()
extrude lvl (Syn.Tvar tid) = do
    lvl' <- getLvl tid
    setLvl tid (min lvl lvl')

    getUpper tid >>= mapM_ (extrude lvl)
    getLower tid >>= mapM_ (extrude lvl)


extrude lvl (Syn.Arrow ltyp rtyp) =
    extrude lvl ltyp >> extrude lvl rtyp
extrude lvl (Syn.RecordType fields) =
    forM_ fields (\(name, typ) -> (name,) <$> extrude lvl typ)
extrude lvl Syn.Nat = pure ()
extrude lvl Syn.Int = pure ()
extrude lvl Syn.Real = pure ()
extrude lvl Syn.Bool = pure ()


--- Lattice Operations ---
infimum :: Syn.Type -> Syn.Type -> TCM Syn.Type
infimum typ typ' | typ == typ' = pure typ

infimum (Syn.Tvar tid) (Syn.Tvar tid') = unify tid tid' $> Syn.Tvar tid'
infimum (Syn.Tvar tid) typ = addUpper typ tid $> Syn.Tvar tid
infimum typ (Syn.Tvar tid) = addUpper typ tid $> Syn.Tvar tid

infimum (Syn.Arrow ltyp rtyp) (Syn.Arrow ltyp' rtyp') =
    Syn.Arrow <$> supremum ltyp ltyp' <*> infimum rtyp rtyp'
infimum (Syn.RecordType fields) (Syn.RecordType fields') =
    let fieldsmap  = pure <$> M.fromList fields
        fieldsmap' = pure <$> M.fromList fields'
        infimumA = (\mt mt' -> do { mt <- mt; mt' <- mt'; infimum mt mt'}) in
        Syn.RecordType . M.toList <$> sequenceA (M.unionWith infimumA fieldsmap fieldsmap')

infimum Syn.Nat Syn.Int = pure Syn.Nat
infimum Syn.Int Syn.Nat = pure Syn.Nat
infimum Syn.Nat Syn.Real = pure Syn.Nat
infimum Syn.Real Syn.Nat = pure Syn.Nat
infimum Syn.Int Syn.Real = pure Syn.Int
infimum Syn.Real Syn.Int = pure Syn.Int

infimum typ typ' = complain $ "no infimum of " <> show typ <> " and " <> show typ'

supremum :: Syn.Type -> Syn.Type -> TCM Syn.Type
supremum typ typ' | typ == typ' = pure typ

supremum (Syn.Tvar tid) (Syn.Tvar tid') = unify tid tid' $> Syn.Tvar tid'
supremum (Syn.Tvar tid) typ = addLower typ tid $> Syn.Tvar tid
supremum typ (Syn.Tvar tid) = addLower typ tid $> Syn.Tvar tid

supremum (Syn.Arrow ltyp rtyp) (Syn.Arrow ltyp' rtyp') =
    Syn.Arrow <$> infimum ltyp ltyp' <*> supremum rtyp rtyp'
supremum (Syn.RecordType fields) (Syn.RecordType fields') =
    let fieldsmap  = pure <$> M.fromList fields
        fieldsmap' = pure <$> M.fromList fields'
        supremumA = (\mt mt' -> do { mt <- mt; mt' <- mt'; supremum mt mt'}) in
        Syn.RecordType . M.toList <$> sequenceA (M.intersectionWith supremumA fieldsmap fieldsmap')

supremum Syn.Nat Syn.Int = pure Syn.Int
supremum Syn.Int Syn.Nat = pure Syn.Int
supremum Syn.Nat Syn.Real = pure Syn.Real
supremum Syn.Real Syn.Nat = pure Syn.Real
supremum Syn.Int Syn.Real = pure Syn.Real
supremum Syn.Real Syn.Int = pure Syn.Real

supremum typ typ' = complain $ "no supremum of " <> show typ <> " and " <> show typ'



--- Builtin Context ---
builtins :: TCM Ctx
builtins = do
    v <- fresh (N "cond") 1
    pure
        [
        (N "not", Mono $ Syn.Arrow Syn.Bool Syn.Bool),
        (N "succ", Mono $ Syn.Arrow Syn.Int Syn.Int),
        (N "neg", Mono $ Syn.Arrow Syn.Real Syn.Real),
        (N "add", Mono $ Syn.Arrow Syn.Real (Syn.Arrow Syn.Real Syn.Real)),
        (N "addN", Mono $ Syn.Arrow Syn.Nat (Syn.Arrow Syn.Nat Syn.Nat)),
        (N "if", Poly 0 $ Syn.Arrow Syn.Bool (Syn.Arrow v (Syn.Arrow v v)))
        ]