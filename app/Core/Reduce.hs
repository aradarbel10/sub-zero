{-# LANGUAGE ViewPatterns #-}

module Core.Reduce ( reduce ) where

import Surface.AST ( Name )
import Core.AST ( Expr (..) )

import Data.List ( find )
import Data.Maybe ( fromJust )

-- named substitution with shadowing
subs :: Expr -> Name -> Expr -> Expr
subs expr name new = case expr of
    Var name'
        | name == name' -> new
        | otherwise -> Var name'
    Let name' typ body rest
        | name == name' -> Let name' typ body rest
        | otherwise -> Let name' typ (subs body name new) (subs rest name new)
    Lam name' typ body
        | name == name' -> Lam name' typ body
        | otherwise -> Lam name' typ (subs body name new)
    App expr expr' -> App (subs expr name new) (subs expr' name new)
    Record entries -> Record $ map (\(lbl, typ, val) -> (lbl, typ, subs val name new)) entries
    Proj expr lbl -> Proj (subs expr name new) lbl
    NatLit n -> NatLit n
    IntLit n -> IntLit n
    RealLit n -> RealLit n
    BoolLit n -> BoolLit n
    NatToInt expr -> NatToInt $ subs expr name new
    IntToReal expr -> IntToReal $ subs expr name new

-- beta reductions
reduce :: Expr -> Expr
reduce (Var name) = Var name

reduce (Let name typ body rest) = Let name typ (reduce body) (reduce rest)

reduce (Lam name typ body) = Lam name typ (reduce body)

reduce (App (reduce -> Lam name _ body) arg) = reduce $ subs body name arg
reduce (App fnc arg) = App (reduce fnc) (reduce arg)

reduce (Record entries) = Record $ map (\(lbl, typ, val) -> (lbl, typ, reduce val)) entries

reduce (Proj (reduce -> Record entries) lbl) =
    let (_, _, entry) = fromJust $ find (\(lbl', _, _) -> lbl == lbl') entries
    in reduce entry
reduce (Proj expr lbl) = Proj (reduce expr) lbl

reduce (NatLit n) = NatLit n
reduce (IntLit n) = IntLit n
reduce (RealLit n) = RealLit n
reduce (BoolLit n) = BoolLit n

reduce (NatToInt expr) = NatToInt (reduce expr)
reduce (IntToReal expr) = IntToReal (reduce expr)