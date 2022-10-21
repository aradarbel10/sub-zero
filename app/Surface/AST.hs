{-# LANGUAGE InstanceSigs #-}

module Surface.AST where

data Name = NM !String !Int
          | N !String
      deriving (Eq, Ord)

instance Show Name where
      show :: Name -> String
      show (NM nm ix) = nm <> show ix
      show (N nm) = nm


data Expr = Var !Name
          | Let !Name Expr Expr

          | Lam !Name Expr
          | App Expr Expr

          | Record [(Name, Expr)]
          | Proj Expr !Name

          | NatLit !Integer
          | IntLit !Integer
          | RealLit !Double
          | BoolLit !Bool
    deriving Show

data Type = Arrow Type Type
          | RecordType [(Name, Type)]

          | Nat
          | Int
          | Real
          | Bool
    deriving Show