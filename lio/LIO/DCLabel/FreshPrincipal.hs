{-# LANGUAGE Trustworthy #-}
module LIO.DCLabel.FreshPrincipal (freshPrincipal) where

import           LIO.TCB (ioTCB)
import           LIO.DCLabel
import           LIO.Privs.TCB (mintTCB)
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
import           System.Random (randomIO)
import           Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString.Lazy.Char8 as L8


shaCounter :: IORef Int
shaCounter = unsafePerformIO (randomIO >>= newIORef)

-- | Create a new princiapl and corresponding principal.
-- With very high-probability, this principal is unique.
freshPrincipal :: DC (Principal, DCPriv)
freshPrincipal = do
  c <- ioTCB $ atomicModifyIORef shaCounter $ \c -> (c+1,c)
  let p = principal
        . L.toStrict
        . bytestringDigest
        . sha256 . L8.pack . show $ c
  return (p, mintTCB $ toComponent p)