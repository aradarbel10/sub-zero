module Typing.Syntax where

import Surface.AST ( Name )
import qualified Surface.AST as Sur

type Lvl = Int
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

          | Coerce Type Type Expr
    deriving (Show)

type TypId = Int
data Type = Tvar !TypId

          | Arrow Type Type
          | RecordType [(Name, Type)]

          | Nat
          | Int
          | Real
          | Bool
    deriving (Eq, Ord, Show)

type Sign = Bool