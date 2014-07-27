{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LIO.DB.Simple.FS where

import LIO
import LIO.Error (withContext)
import LIO.TCB (ioTCB, Labeled(..))
import LIO.DCLabel
import LIO.DB.Simple

import Data.List (isInfixOf)
import Control.Applicative
import Control.Monad
import System.FilePath
import qualified System.Directory as IO
import qualified Data.ByteString.Char8 as S8

newtype FileDB a = FileDB { unFileDB :: DC a }
   deriving (Functor, Applicative, Monad)

instance MonadLIO DCLabel FileDB where
  liftLIO = FileDB

instance (Show t, Read t, Policy DCLabel FileDB t) => 
         SimpleDB DCLabel FileDB t where
  withModelP m priv act = unFileDB $ withDBContext m "withModelP" $ do
    validatePath $ handleModelName m
    -- Creating model
    liftLIO $ do
      ioTCB $ IO.createDirectoryIfMissing True ("model" </> handleModelName m)
    act

  lFindObjP priv key = withDBContext (keyToHandle key) "lFindObjP" $ do
    validatePath $ keyModelName key
    validatePath $ fromKey key

    -- Reading from model
    lmodel <- modelLabel priv $ keyToHandle key
    liftLIO $ taintP priv lmodel

    -- Get object
    obj <- liftLIO $ do
      bs <- ioTCB $ S8.readFile ("model" </> keyModelName key </> fromKey key)
      return $ read . S8.unpack $ bs
    -- Compute label of object
    lobj <- objLabel priv obj
    -- Return labeled object (may not be bounded by current
    -- label/clearance)
    liftLIO $ return $ LabeledTCB lobj obj

  saveP priv obj = withDBContext (keyToHandle $ keyField obj) "saveP" $ do
    let key = keyField obj
    validatePath $ keyModelName key
    validatePath $ fromKey key

    -- Can you write to the model?
    -- writeFile may fail, so taint
    lmodel <- modelLabel priv $ keyToHandle key
    liftLIO $ guardWriteP priv lmodel

    -- Can you create such a labeled object?
    lobj <- objLabel priv obj
    liftLIO $ guardAllocP priv lobj

    liftLIO . ioTCB $ 
      S8.writeFile ("model" </> keyModelName key </> fromKey key)
                   (S8.pack (show obj))
    return key

  allKeysP m priv = withDBContext m "allKeysP" $ do
    let dir = handleModelName m
    validatePath dir

    -- Reading from model
    lmodel <- modelLabel priv m
    liftLIO $ taintP priv lmodel

    ls <- liftLIO . ioTCB $ 
            IO.getDirectoryContents $ "model" </> dir
    let keys = map Key $ filter (`notElem` [".", ".."]) ls
    return keys

  deleteP priv key = withDBContext (keyToHandle key) "deleteP" $ do
    validatePath $ keyModelName key
    validatePath $ fromKey key

    -- Can you write to the model?
    -- writeFile may fail, so taint
    lmodel <- modelLabel priv $ keyToHandle key
    liftLIO $ guardWriteP priv lmodel

    -- Can you create such a labeled object?
    lobj <- lFindObjP priv key
    liftLIO $ guardAllocP priv $ labelOf lobj
 
    liftLIO . ioTCB $ do
       IO.removeFile ("model" </> keyModelName key </> fromKey key)

  catchDB _ act handler =
    let lioHandler e = unFileDB (handler e)
    in FileDB $ catch (unFileDB act) lioHandler
    
  withDBContext m str act = do
    FileDB $ withContext (handleModelName m ++ ":" ++ str) (unFileDB act)

validatePath :: MonadLIO l m => FilePath -> m ()
validatePath fp = liftLIO $ do
  when (".." `isInfixOf` fp || "/" `isInfixOf` fp) $ fail "Invalid path"
