{-# LANGUAGE InstanceSigs #-}

module Core.Pretty where

import Core.AST ( Type(..), Expr(..) )
import Data.List ( intercalate )

-- given a "base" precedence and a string,
-- make a function that will wrap the string in parens
-- if given a prec higher than base

-- intuitively, it prints an `n` inside an `m`
prec :: Int -> String -> (Int -> String)
prec n str m = if n <= m then str else "(" <> str <> ")"


instance Show Expr where
    show :: Expr -> String
    show typ = show' typ maxBound

        where
        show' :: Expr -> Int -> String
        show' (Var name) = prec 0 (show name)
        show' (Let name typ body rest) = prec 40 $
            "let " <> show name <> " : " <> show typ <> " = " <> show' body 35 <> " in " <> show' rest 40
        show' (Lam name typ body) = prec 30 $
            "λ" <> show name <> " : " <> show typ <> " . " <> show' body 30
        show' (App fnc arg) = prec 4 $
            show' fnc 4 <> " " <> show' arg 3
        show' (Record entries) = prec 0 $ "{" <> intercalate "; "
            (map (\(lbl, typ, val) -> show lbl <> " : " <> show typ <> " = " <> show' val maxBound) entries) <> "}"
        show' (Proj expr lbl) = prec 2 $
            show' expr 2 <> "." <> show lbl
        show' (NatLit n) = prec 0 $ show n
        show' (IntLit n) = prec 0 $ show n
        show' (RealLit n) = prec 0 $ show n
        show' (BoolLit n) = prec 0 $ show n

        show' (NatToInt n) = prec 4 $ "ℕtoℤ " <> show' n 3
        show' (IntToReal n) = prec 4 $ "ℤtoℝ " <> show' n 3

instance Show Type where
    show :: Type -> String
    show typ = show' typ maxBound

        where
        show' :: Type -> Int -> String
        show' (Tvar name lvl) = prec 0 $ "?" <> show name
        show' (Arrow ltyp rtyp) = prec 8 $ show' ltyp 7 <> " → " <> show' rtyp 8
        show' (RecordType fields) = prec 0 $ "{" <> intercalate "; "
            (map (\(lbl, typ) -> show lbl <> " : " <> show' typ maxBound) fields) <> "}"
        show' Nat = prec 0 "ℕ"
        show' Int = prec 0 "ℤ"
        show' Real = prec 0 "ℝ"
        show' Bool = prec 0 "𝔹"