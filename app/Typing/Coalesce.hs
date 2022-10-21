{-# LANGUAGE TupleSections #-}

module Typing.Coalesce ( coalesce, zonk ) where

import Surface.AST ( Name(..) )
import Typing.Syntax ( TypId, Sign )
import Typing.TCM ( TCM, unique, getLower, getUpper, fresh, getVar, VarState (..), complain, getTypId )
import qualified Typing.Syntax as Syn

import qualified Core.AST as C
import Core.Pretty ()

import Data.Maybe ( maybe )
import qualified Data.Map.Strict as M
import Data.Bifunctor ( second )

{-
positive -> union with lower  | supremum of lower
negative -> inter with higher | infimum of higher
-}

coalesce :: Syn.Type -> TCM C.Type
coalesce = coalesceSign True


coalesceSign :: Sign -> Syn.Type -> TCM C.Type
coalesceSign sgn Syn.Nat = pure C.Nat
coalesceSign sgn Syn.Int = pure C.Int
coalesceSign sgn Syn.Real = pure C.Real
coalesceSign sgn Syn.Bool = pure C.Bool

-- coalesceSign sgn Syn.Top = pure C.Top
-- coalesceSign sgn Syn.Bot = pure C.Bot

coalesceSign sgn (Syn.Arrow ltyp rtyp) = C.Arrow <$> coalesceSign (not sgn) ltyp <*> coalesceSign sgn rtyp
coalesceSign sgn (Syn.RecordType fields) = C.RecordType <$> mapM (\(lbl, typ) -> do
    typ <- coalesceSign sgn typ
    pure (lbl, typ)) fields

coalesceSign sgn (Syn.Tvar tid) = do
    bound <- (if sgn then getLower else getUpper) tid
    case bound of
        Nothing -> do
            otherBound <- (if sgn then getUpper else getLower) tid
            case otherBound of
                Nothing -> do
                    nm <- name <$> getVar tid
                    lvl <- level <$> getVar tid
                    pure $ C.Tvar nm lvl
                Just otherBound ->
                    coalesceSign sgn otherBound
        Just bound -> coalesceSign sgn bound


zonk :: Syn.Expr -> TCM C.Expr
zonk (Syn.Var name) = pure $ C.Var name
zonk (Syn.Let name typ body rest) =
    C.Let name <$> coalesce typ <*> zonk body <*> zonk rest

zonk (Syn.Lam name typ body) = C.Lam name <$> coalesce typ <*> zonk body
zonk (Syn.App fnc arg) = C.App <$> zonk fnc <*> zonk arg

zonk (Syn.Record entries) =
    C.Record <$> mapM (\(name, typ, val) -> (name,,) <$> coalesce typ <*> zonk val) entries
zonk (Syn.Proj expr lbl) =
    C.Proj <$> zonk expr <*> pure lbl

zonk (Syn.NatLit n) = pure $ C.NatLit n
zonk (Syn.IntLit n) = pure $ C.IntLit n
zonk (Syn.RealLit n) = pure $ C.RealLit n
zonk (Syn.BoolLit n) = pure $ C.BoolLit n

zonk (Syn.Coerce src dst expr) = do
    coerce <- zonkCoerce <$> coalesce src <*> coalesce dst
    coerce <*> zonk expr

zonkCoerce :: C.Type -> C.Type -> TCM (C.Expr -> C.Expr)
zonkCoerce src dst | src == dst = pure id

zonkCoerce (C.Tvar _ _) _ = pure id
zonkCoerce _ (C.Tvar _ _) = pure id

zonkCoerce C.Nat C.Int = pure C.NatToInt
zonkCoerce C.Int C.Real = pure C.IntToReal
zonkCoerce C.Nat C.Real = pure $ C.IntToReal . C.NatToInt

zonkCoerce (C.Arrow ltyp rtyp) (C.Arrow ltyp' rtyp') = do
    lcoerce <- zonkCoerce ltyp' ltyp
    rcoerce <- zonkCoerce rtyp rtyp'
    name <- unique (N "x")
    pure $ \f -> C.Lam name ltyp' (rcoerce $ C.App f $ lcoerce (C.Var name))

zonkCoerce (C.RecordType fields) (C.RecordType fields') = do
    coerces <- constrainRecords fields fields'
    pure $ \f -> C.Record $ zipWith (\(name, typ) coe -> (name, typ, coe f)) fields' coerces

    where
    constrainRecords :: [(Name, C.Type)] -> [(Name, C.Type)] -> TCM [C.Expr -> C.Expr]
    constrainRecords fields [] = pure []
    constrainRecords fields ((lbl, ftyp') : fields') =
        case lookup lbl fields of
            Nothing -> complain $ "record type missing field " <> show lbl
            Just ftyp -> do
                fcoerce <- zonkCoerce ftyp ftyp'
                rest <- constrainRecords fields fields'
                pure $ (\val -> fcoerce (C.Proj val lbl)) : rest

zonkCoerce typ typ' =
    complain $ "uncoercable types during zonking: " <> show typ <> " >> " <> show typ'