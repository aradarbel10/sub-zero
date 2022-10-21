module Core.AST where

import Surface.AST ( Name )
import Typing.Syntax ( Lvl )

data Expr = Var !Name
          | Let !Name Type Expr Expr

          | Lam !Name Type Expr
          | App Expr Expr

          | Record [(Name, Type, Expr)]
          | Proj Expr !Name

          | NatLit !Integer
          | IntLit !Integer
          | RealLit !Double
          | BoolLit !Bool

          | NatToInt Expr
          | IntToReal Expr

data Type = Arrow Type Type
          | RecordType [(Name, Type)]

          | Tvar !Name !Lvl

          | Nat
          | Int
          | Real
          | Bool
    deriving Eq