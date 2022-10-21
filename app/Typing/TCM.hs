{-# LANGUAGE DerivingVia #-}

module Typing.TCM
    ( TCM, runTCM
    , VarState(..), TCS(..)
    , complain
    , fresh, unique, newVar
    , getVar, getTypId, adjustVar
    , redirect
    , setLvl, getLvl
    , setUpper, getUpper
    , setLower, getLower
    ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Control.Monad ( foldM )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( StateT(StateT), runStateT, get, gets, put, modify )

import Surface.AST ( Name(..) )
import Typing.Syntax ( Expr(..), Type(..), TypId, Lvl )

--- Type Checking Monad ---
data VarState = VS { name :: !Name, level :: !Lvl, upper :: Maybe Type, lower :: Maybe Type }
    deriving Show

data MctxVarState = MVS VarState | Ref TypId
    deriving Show
type MetaCtx = IM.IntMap MctxVarState

data TCS = TCS { freshid :: !TypId, freshi :: !Int, mctx :: MetaCtx }
newtype TCM a = TCM (StateT TCS (Either String) a)
    deriving (Functor, Applicative, Monad) via (StateT TCS (Either String))

--- Utilities ---
runTCM :: TCM a -> Either String (a, TCS)
runTCM (TCM a) = runStateT a (TCS { freshid = 0, freshi = 0, mctx = IM.empty })

unTCM :: TCM a -> StateT TCS (Either String) a
unTCM (TCM stt) = stt

nextid :: TCM TypId
nextid = TCM $ do
    curr <- get
    let ix = freshid curr
    put $ curr { freshid = ix + 1 }
    pure ix

nexti :: TCM TypId
nexti = TCM $ do
    curr <- get
    let ix = freshi curr
    put $ curr { freshi = ix + 1 }
    pure ix

unique :: Name -> TCM Name
unique name = do
    NM (case name of
        NM nm' _ -> nm'
        N nm' -> nm') <$> nexti

complain :: String -> TCM a
complain msg = TCM $ lift (Left msg)

--- Variable Primitives Following Redirections ---

-- mutation
setVarRaw :: TypId -> MctxVarState -> TCM ()
setVarRaw tid mvs = TCM $ do
    curr <- get
    put $ curr { mctx = IM.insert tid mvs (mctx curr) }

redirect :: TypId -> TypId -> TCM ()
redirect from to = setVarRaw from (Ref to)

setVar :: TypId -> VarState -> TCM ()
setVar tid vs = do
    mvs <- TCM $ gets ((IM.! tid) . mctx)
    case mvs of
        MVS _ -> setVarRaw tid (MVS vs)
        Ref tid' -> setVar tid' vs

-- inspection
getVar :: TypId -> TCM VarState
getVar tid = do
    mvs <- TCM $ gets ((IM.! tid) . mctx)
    case mvs of
        MVS vs -> pure vs
        Ref tid' -> getVar tid'

getTypId :: TypId -> TCM TypId
getTypId tid = do
    mvs <- TCM $ gets ((IM.! tid) . mctx)
    case mvs of
        MVS _ -> pure tid
        Ref tid' -> getTypId tid'

-- creation
newVar :: VarState -> TCM Type
newVar vs = do
    ix <- nextid
    setVarRaw ix (MVS vs) -- definitely not already there, so we can set raw
    pure $ Tvar ix

fresh :: Name -> Lvl -> TCM Type
fresh name lvl = do
    name <- unique name
    newVar $ VS name lvl Nothing Nothing

-- alteration
adjustVar :: TypId -> (VarState -> VarState) -> TCM ()
adjustVar tid f = do
    vs <- getVar tid
    setVar tid (f vs)

--- Variable Manipulation ---
setLvl :: TypId -> Lvl -> TCM ()
setLvl tid lvl = adjustVar tid (\vs -> vs { level = lvl })

getLvl :: TypId -> TCM Lvl
getLvl tid = level <$> getVar tid

setUpper :: Type -> TypId -> TCM ()
setUpper typ tid = adjustVar tid (\vs -> vs { upper = Just typ })

setLower :: Type -> TypId -> TCM ()
setLower typ tid = adjustVar tid (\vs -> vs { lower = Just typ })

getUpper :: TypId -> TCM (Maybe Type)
getUpper tid = upper <$> getVar tid

getLower :: TypId -> TCM (Maybe Type)
getLower tid = lower <$> getVar tid