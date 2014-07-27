{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LIO.DB.Simple where

import LIO
import LIO.Error (withContext)
import Data.Maybe
import Control.Monad

data Key t = Key { fromKey :: String }
  deriving (Eq, Show, Read)

--
-- Models and Polcies
--


type ModelName = String

data ModelHandle t = ModelHandle

class Model t where
  -- | Extract key
  keyField   :: t -> Key t
  modelName  :: t -> ModelName

class SimpleDB l m t => Policy l m t where
  -- | Polcy to determine label of the model
  modelLabel :: PrivDesc l p => Priv p -> ModelHandle t -> m l
  -- | Polcy to determine label of the object
  objLabel   :: PrivDesc l p => Priv p -> t -> m l
  
--
-- DB interface
--

class (Label l, MonadLIO l m, Model t) => SimpleDB l m t | t -> l m where
  -- | Run DB action
  withModel :: ModelHandle t -> m a -> LIO l a
  withModel m = withModelP m noPrivs

  withModelP :: PrivDesc l p => ModelHandle t -> Priv p -> m a -> LIO l a

  -- | Doe an object with the given key exist
  exists :: Key t -> m Bool 
  exists = existsP noPrivs

  existsP :: PrivDesc l p => Priv p -> Key t -> m Bool
  existsP priv key = withDBContext (keyToHandle key) "existsP" $ do
    ks <- allKeysP (keyToHandle key) priv
    return $ key `elem` ks

  -- | Get object by id.
  findObj :: Key t -> m t
  findObj = findObjP noPrivs

  findObjP :: PrivDesc l p => Priv p -> Key t -> m t
  findObjP priv key = withDBContext (keyToHandle key) "findObjP" $ do
    lv <- lFindObjP priv key
    liftLIO $ unlabelP priv lv

  -- | Get object without taining the current context
  lFindObj :: Key t -> m (Labeled l t)
  lFindObj = lFindObjP noPrivs

  lFindObjP :: PrivDesc l p => Priv p -> Key t -> m (Labeled l t)

  -- | Get all objects below the current clearance.
  findAll :: ModelHandle t -> m [t]
  findAll m = findAllP m noPrivs

  findAllP :: PrivDesc l p => ModelHandle t -> Priv p -> m [t]
  findAllP m priv =  withDBContext m "findAllP" $ do
    lvs <- lFindAllP m priv
    liftLIO $ do
      clr <- getClearance
      let lvs' = filter (\l -> labelOf l `canFlowTo` clr) lvs
      mapM (unlabelP priv) lvs'

  lFindAll :: ModelHandle t -> m [Labeled l t]
  lFindAll m = lFindAllP m noPrivs

  -- | Get all model objects.
  lFindAllP :: PrivDesc l p => ModelHandle t -> Priv p -> m [Labeled l t]
  lFindAllP m priv = withDBContext m "lFindAllP" $ do
    keys <- allKeysP m priv
    mapM (lFindObjP priv) keys
               

  allKeys :: ModelHandle t -> m [Key t]
  allKeys m = allKeysP m noPrivs

  allKeysP :: PrivDesc l p => ModelHandle t -> Priv p -> m [Key t]

  -- | Save new existing object.
  insert :: t -> m (Key t)
  insert = insertP noPrivs

  insertP :: PrivDesc l p => Priv p -> t -> m (Key t)
  insertP priv obj = withDBContext (keyToHandle key) "insertP" $ do
    e <- existsP priv key
    when e $ liftLIO $ fail $ "Entry '" ++ fromKey key ++ 
                              "' exists in model '" ++ keyModelName key ++ "'"
    saveP priv obj
     where key = keyField obj

  insert_ :: t -> m ()
  insert_ o = insert o >> return ()

  insertP_ :: PrivDesc l p => Priv p -> t -> m ()
  insertP_ priv obj = insertP priv obj >> return ()

  -- | Save potentially existing object.
  save :: t -> m (Key t)
  save = saveP noPrivs

  saveP :: PrivDesc l p => Priv p -> t -> m (Key t)

  save_ :: t -> m ()
  save_ o = save o >> return ()

  saveP_ :: PrivDesc l p => Priv p -> t -> m ()
  saveP_ priv obj = saveP priv obj >> return ()

  -- | Delete existing object.
  delete :: Key t -> m ()
  delete = deleteP noPrivs

  deleteP :: PrivDesc l p => Priv p -> Key t -> m ()

  -- | See 'catch'.
  catchDB :: Exception e => ModelHandle t -> m a -> (e -> m a) -> m a

  -- | See 'handle'.
  handleDB :: Exception e => ModelHandle t -> (e -> m a) -> m a -> m a
  handleDB m = flip (catchDB m)

  -- | See 'try'.
  tryDB :: Exception e => ModelHandle t -> m a -> m (Either e a)
  tryDB m a = catchDB m (a >>= \v -> return (Right v)) (\e -> return (Left e))

  -- | See 'onException'
  onExceptionDB :: ModelHandle t -> m a -> m b -> m a
  onExceptionDB m io what = catchDB m io $ \(e :: SomeException) -> 
                              do _ <- what
                                 liftLIO $ throwLIO e

  -- | See 'withContext'.
  withDBContext :: ModelHandle t -> String -> m a -> m a
  withDBContext _ _ act = act


-- | Get the model name corresponding to the key
keyModelName :: forall t. Model t => Key t -> ModelName
keyModelName _ = modelName (undefined :: t)

-- | Get the model name corresponding to the handle
handleModelName :: forall t. Model t => ModelHandle t -> ModelName
handleModelName _ = modelName (undefined :: t)

-- | Get the model handle corresponding to the key
keyToHandle :: Model t => Key t -> ModelHandle t
keyToHandle _ = ModelHandle
