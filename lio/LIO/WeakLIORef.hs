{-# LANGUAGE DeriveDataTypeable #-}
module LIO.WeakLIORef (
    WeakLIORef, WeakLIORefHandler, WeakLIORefPair
  -- * Basic Functions
  -- ** Create weak 'LIORef's
  , newWeakLIORef, newWeakLIORefP
  -- ** Read 'WeakLIORef's
  , readWeakLIORef, readWeakLIORefP
  -- ** Write 'WeakLIORef's
  , writeWeakLIORef, writeWeakLIORefP
  -- ** Modify 'WeakLIORef's
  , atomicModifyWeakLIORef, atomicModifyWeakLIORefP
  -- ** Deallocate 'WeakLIORef's
  , isValid, isValidP
  , freeWeakLIORef, freeWeakLIORefP
  ) where

import Data.Typeable

import Control.Monad
import Control.Exception

import LIO
import LIO.LIORef


-- | Weak LIO reference is an 'LIORef' that can be deallocated.
newtype WeakLIORef l a = WeakLIORef { unWeakLIORef :: LIORef l (Maybe a) }

-- | A wrapper for a weak reference that is supplied at allocation time
newtype WeakLIORefHandler l a = WeakLIORefHandler (WeakLIORef l a)

type WeakLIORefPair l a = (WeakLIORefHandler l a, WeakLIORef l a)

-- | Weak LIO reference error. Thrown when accessing a deallocated reference.
data WeakLIORefError = WeakLIORefDeallocated
                     | WeakLIORefDoubleFree
                       deriving Typeable

instance Show WeakLIORefError where
  show WeakLIORefDeallocated = "Reference has been deallocated"
  show WeakLIORefDoubleFree  = "Deallocaitng a free reference"

instance Exception WeakLIORefError

--
-- Create weak labeled references
--


-- | Get the label of a 'WeakLIORef'.
instance LabelOf WeakLIORef where
  labelOf = labelOf . unWeakLIORef

-- | To create a new weak reference the label of the reference must be
-- below the thread's current clearance and above the current label.
-- If this is the case, the reference is built. Otherwise an exception
-- will be thrown by the underlying 'guardAlloc' guard.
newWeakLIORef :: MonadLIO l m
              => l                  -- ^ Label of reference
              -> a                  -- ^ Initial value
              -> m (WeakLIORefPair l a) -- ^ Mutable reference
newWeakLIORef = newWeakLIORefP NoPrivs

-- | Same as 'newWeakLIORef' except @newWeakLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the weak reference to the current label and clearance.
newWeakLIORefP :: (MonadLIO l m, Priv l p) => p -> l -> a -> m (WeakLIORefPair l a)
newWeakLIORefP p l a = do
  lr <- WeakLIORef `liftM` newLIORefP p l (Just a)
  return (WeakLIORefHandler lr, lr)

--
-- Read 'LIORef's
--

-- | Read the value of a weak labeled reference. A read succeeds only
-- if the label of the reference is below the current clearance and the
-- reference has not been deallocated. Moreover, the current label is
-- raised to the join of the current label and the reference label. To
-- avoid failures (introduced by the 'taint' guard) use 'labelOf' to
-- check that a read will succeed.
readWeakLIORef :: MonadLIO l m => WeakLIORef l a -> m a
readWeakLIORef = readWeakLIORefP NoPrivs

-- | Same as 'readWeakLIORef' except @readWeakLIORefP@ takes a
-- privilege object which is used when the current label is raised.
readWeakLIORefP :: (MonadLIO l m, Priv l p) => p -> WeakLIORef l a -> m a
readWeakLIORefP p (WeakLIORef lr) = do
  mv <- readLIORefP p lr
  maybe (throwLIO WeakLIORefDeallocated) return mv

--
-- Write 'WeakLIORef's
--

-- | Atomically updates the contents of a weak labeled reference. It
-- is required that the label of the reference be above the current
-- label, but below the current clearance. Moreover, since this function
-- can be used to directly check if the reference has been deallocated,
-- the computation is \"tainted\" by the reference label (i.e., the
-- current label is raised to the 'join' of the current and reference
-- labels). These checks and label raise are done by 'guardWrite', which
-- will raise an exception if any of the IFC conditions cannot be
-- satisfied. An exception will also be raised if the reference has been
-- deallocated.
writeWeakLIORef :: MonadLIO l m => WeakLIORef l a -> a -> m ()
writeWeakLIORef = writeWeakLIORefP NoPrivs

-- | Same as 'writeWeakLIORef' except @writeWeakLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
writeWeakLIORefP :: (MonadLIO l m, Priv l p) => p -> WeakLIORef l a -> a -> m ()
writeWeakLIORefP p lr a = atomicModifyWeakLIORefP p lr $ const (a,())


--
-- Atomic modify
--

-- | Atomically modifies the contents of a weak 'LIORef'. It is required
-- that the label of the reference be above the current label, but
-- below the current clearance. Moreover, since this function can be
-- used to directly read the value of the stored reference, the
-- computation is \"tainted\" by the reference label (i.e., the
-- current label is raised to the 'join' of the current and reference
-- labels). These checks and label raise are done by 'guardWrite',
-- which will raise an exception if any of the IFC conditions cannot
-- be satisfied. An exception will also be raised if the reference has been
-- deallocated.
atomicModifyWeakLIORef :: MonadLIO l m => WeakLIORef l a -> (a -> (a, b)) -> m b
atomicModifyWeakLIORef = atomicModifyWeakLIORefP NoPrivs

-- | Same as 'atomicModifyWeakLIORef' except @atomicModifyWeakLIORefP@ takes
-- a set of privileges which are accounted for in label comparisons.
atomicModifyWeakLIORefP :: (MonadLIO l m, Priv l p)
                        => p -> WeakLIORef l a -> (a -> (a, b)) -> m b
atomicModifyWeakLIORefP p (WeakLIORef lr) f = do
  dealloc <- atomicModifyLIORefP p lr $ \mv ->
                 case mv of
                  Just a -> let (a', b) = f a
                            in (Just a', Just b)
                  _      -> (Nothing, Nothing)
  maybe (throwLIO WeakLIORefDeallocated) return dealloc

 
--
-- Deallocate
--

-- | Free a weak labeled reference. It is required that the label of
-- the reference be above the current label, but below the current
-- clearance. Moreover, the computation is \"tainted\" by the reference
-- label (i.e., the current label is raised to the 'join' of the current
-- and reference labels) to reflect the internal check of the reference's
-- existence. These checks and label raise are done by 'guardWrite',
-- which will raise an exception if any of the IFC conditions cannot be
-- satisfied. An exception will also be raised if the reference has been
-- deallocated.
freeWeakLIORef :: MonadLIO l m => WeakLIORefHandler l a -> m ()
freeWeakLIORef = freeWeakLIORefP NoPrivs

-- | Same as 'freeWeakLIORef' except @freeWeakLIORefP@ takes
-- a set of privileges which are accounted for in label comparisons.
freeWeakLIORefP :: (MonadLIO l m, Priv l p) => p -> WeakLIORefHandler l a -> m ()
freeWeakLIORefP p (WeakLIORefHandler (WeakLIORef lr)) = do
  dealloc <- atomicModifyLIORefP p lr $ \mv ->
                 case mv of
                  Just _ -> (Nothing, False)
                  _      -> (Nothing, True )
  when dealloc $ throwLIO WeakLIORefDoubleFree

-- | Check if a weak labeled reference is still valid, i.e., has not
-- been deallocated. Note that, like 'readWeakLIORef', this raises the
-- current label to reflect the check on the object existence.
isValid :: MonadLIO l m => WeakLIORef l a -> m Bool
isValid = isValidP NoPrivs

-- | Same as 'isValid', but uses privileges when raising the current label.
isValidP :: (MonadLIO l m, Priv l p) => p -> WeakLIORef l a -> m Bool
isValidP p lr = liftLIO $ catchLIOP p (readWeakLIORef lr >> return True)
                                      (\WeakLIORefDeallocated -> return False)