{-# LANGUAGE Trustworthy #-}

-- | Exports 'Data.UUID.V4' to LIO code.  

module LIO.Data.UUID.V4 (nextRandom) where

import LIO
import LIO.TCB (ioTCB)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as IO

-- | Generate a random UUID.
nextRandom :: Label l => LIO l UUID
nextRandom = ioTCB IO.nextRandom
