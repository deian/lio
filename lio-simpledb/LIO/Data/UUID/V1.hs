{-# LANGUAGE Trustworthy #-}

-- | Exports 'Data.UUID.V1' to LIO code.  

module LIO.Data.UUID.V1 (nextUUID) where

import LIO
import LIO.TCB (ioTCB)
import Data.UUID (UUID)
import qualified Data.UUID.V1 as IO

-- | Returns a new UUID derived from the local hardware MAC
-- address and the current system time.
-- Is generated according to the Version 1 UUID sepcified in
-- RFC 4122.
--
-- Returns 'Nothing' if you request UUIDs too quickly.
nextUUID :: Label l => LIO l (Maybe UUID)
nextUUID = ioTCB IO.nextUUID
