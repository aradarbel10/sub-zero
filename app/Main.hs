module Main where

import qualified Data.IntMap as IM

import Surface.AST
import Surface.Parse

import Typing.TCM ( runTCM, VarState(VS), TCS(..) )
import Typing.Typecheck ( infer, builtins )
import Typing.Coalesce ( coalesce, zonk )

import Core.Pretty ()
import Core.Reduce ( reduce )

inferFull :: Expr -> IO ()
inferFull expr = do
    let res = runTCM $ do
            builtins <- builtins
            (expr, typ) <- infer builtins 0 expr
            typ <- coalesce typ
            expr <- zonk expr
            expr <- pure $ reduce expr
            pure (expr, typ)
    case res of
        Left err -> putStrLn $ "type error: " <> err <> "\n"
        Right ((expr, typ), env) ->
               print expr
            >> putStr "\t: " >> print typ
            -- >> print (mctx env)
            >> putStr "\n"


execFull :: String -> IO ()
execFull str = do
    case parseExpr str of
        Left err -> putStrLn $ "parse error: " <> err <> "\n"
        Right expr -> print expr >> inferFull expr


-- test cases from SimpleSub
testBasic :: IO ()
testBasic = do
    execFull "42"
    execFull "fun x . 42"
    execFull "fun x . x"
    execFull "λ x . x 42"
    execFull "λ f . { a = f 42; b = f -42; c = f 42.0 }"
    execFull "x ↦ add x 1"
    execFull "(fun x . x) 42"
    execFull "fun f . fun x . f (f x)"

testBooleans :: IO ()
testBooleans = do
    execFull "True"
    execFull "not True"
    execFull "b ↦ not b"
    execFull "(b ↦ not b) True"
    execFull "if"
    execFull "λ x . λ y . λ z . if x y z"
    -- counterexamples
    execFull "succ True"
    execFull "x ↦ succ (not x)"
    execFull "(x ↦ not x.f) { f = 42 }"

testRecords :: IO ()
testRecords = do
    execFull "fun x . x.f"
    execFull "{}"
    execFull "{ f = 42 }"
    execFull "{ f = 42 }.f"
    execFull "(x ↦ x.f) { f = 42; g = False }"
    execFull "fun f . { x = f 42 }.x"
    execFull "if True {a = 42; b = True} {b = False; c = -42}"

testPoly :: IO ()
testPoly = do
    execFull "fun x . let y = fun z . z in y"
    execFull "let f = fun x . x in { a = f 0; b = f True }"
    execFull "fun x . let y = x in y"
    execFull "fun y . let f = fun x . x in { a = f y; b = f True }"
    execFull "fun y . let f = fun x . y x in { a = f 0; b = f True }" -- counterexample
    execFull "fun y . let f = fun x . x y in { a = f (fun z . z); b = f (fun z . True) }"

    execFull "let once = fun f . fun x . f x in once"
    execFull "let twice = fun f . fun x . f (f x) in twice"


main :: IO ()
main = do
    testBasic
    testBooleans
    testRecords
    testPoly