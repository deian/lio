{-# LANGUAGE MultiParamTypeClasses
           , Rank2Types
           , ExistentialQuantification
           , ImpredicativeTypes
           , FlexibleContexts
           , GeneralizedNewtypeDeriving #-}
module Data.WeakRef (newRef, readRef, writeRef, freeRef) where

import LIO
import LIO.TCB (ioTCB)
import Control.Concurrent (killThread)
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.DCLabel
import LIO.DCLabel.FreshPrincipal
import LIO.WeakLIORef
import Control.Monad.State
import Data.List (foldl')
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

data WeakRef a = WeakRef { wRef  :: WeakLIORef DCLabel a
                         , wPriv :: DCPriv }

newtype WeakRefHandler a = WeakRefHandler (WeakRef a)

instance Ord DCLabel where (<=) = canFlowTo

data MDCState = MDCState { mdcMap  :: LMVar DCLabel (Map DCLabel [DC ()]) 
                         , mdcPriv :: DCPriv }

newtype MDC a = MDC { unMDC :: StateT MDCState DC a }
                deriving (Monad, Functor, MonadState MDCState)


instance MonadLIO DCLabel MDC where
  liftLIO = MDC . lift


--
-- Create a new ref
--

-- | TODO wrap with mask:
modifyLMVarP :: Priv l p => p -> LMVar l a -> (a -> (a, b)) -> LIO l b
modifyLMVarP p lv f = do
  v <- takeLMVarP p lv
  let (a, b) = f v
  putLMVarP p lv a
  return b

modifyLMVarP_ :: Priv l p => p -> LMVar l a -> (a -> a) -> LIO l ()
modifyLMVarP_ p lv f = modifyLMVarP p lv (\x -> (f x,()))
              

newRef :: a -> MDC (WeakRefHandler a, WeakRef a)
newRef x = do
  (p, priv) <- liftLIO $ freshPrincipal
  l <- getLabel
  let l' = dcLabel (toComponent p) anybody `lub` l
  newRefTCB l' priv x

newRefTCB :: DCLabel -> DCPriv -> a -> MDC (WeakRefHandler a, WeakRef a)
newRefTCB l' priv x = do
  (h,r) <- liftLIO $ newWeakLIORef l' x
  MDCState _map _priv <- get
  liftLIO $ modifyLMVarP_ _priv _map $ Map.adjust (freeWeakLIORefP priv h:) l'
  let wr = WeakRef r priv
  return (WeakRefHandler wr, wr)


--
-- Read references
--

class Copyable a where copy :: a -> a

readRef :: WeakRef a -> MDC a
readRef (WeakRef r _) = liftLIO $ readWeakLIORef r


readRefTrunc :: Copyable a => WeakRef a -> MDC a
readRefTrunc (WeakRef r p) = do
  v <- liftLIO $ readWeakLIORefP p r
  return $ copy v

--
-- Write references
--

writeRef :: WeakRef a -> a -> MDC ()
writeRef (WeakRef r _) x = liftLIO $ writeWeakLIORef r x

--
-- Dealloc refrences
--

-- | Free all affected references and kill all affected threads
freeRef :: WeakRefHandler a -> MDC ()
freeRef (WeakRefHandler (WeakRef r _)) = do
  MDCState _map _priv <- get
  ds <- liftLIO $ modifyLMVarP _priv _map $ 
    Map.partitionWithKey (\l _ -> labelOf r `canFlowTo` l)
  liftLIO $ forM_ (concat $ Map.elems ds) id


getLubPriv :: [(forall a. WeakRef a)] -> [(DCLabel, DCPriv)]
getLubPriv []     = []
getLubPriv (r:rs) = (labelOf (wRef r), wPriv r) : getLubPriv rs

withRef :: [(forall a. WeakRef a)] -> MDC a -> MDC (WeakRefHandler (Maybe a), WeakRef (Maybe a))
withRef rs act = do
  lcur <- liftLIO getLabel
  let (ls,ps) = unzip $ getLubPriv rs
      l = foldl' lub lcur ls
      p = mconcat ps
  (h,r) <- newRefTCB l p Nothing
  s <- get
  lock <- liftLIO $ newEmptyLMVar lcur
  tid <- liftLIO $ withClearanceP p l $ forkLIO $ do
    takeLMVar lock
    void $ evalStateT (act' r) s
  MDCState _map _priv <- get
  liftLIO $ modifyLMVarP_ _priv _map $ Map.adjust (ioTCB (killThread tid):) l
  liftLIO $ putLMVar lock ()
  return (h,r)
   where act' r = unMDC $ act >>= writeRef r . Just