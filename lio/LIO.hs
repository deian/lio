{-# LANGUAGE Safe #-}
{- |

This is the main module to be included by code using the Labeled IO
(LIO) library. This module exports the core library (documented in
"LIO.Core"), with support for labels and privileges (documented in
"LIO.Privs") and labeled values (documented in "LIO.Labeled").

Certain symbols in the lio library, particularly those in
"LIO.Exception", use the same names as their 'IO' equivalents in the
system libraries.  Hence main modules that mostly include 'IO' code
and only need to invoke 'LIO' code should import "LIO.Run" (or
"LIO.DCLabel") to avoid polluting their namespaces.

Most code will need to use a particular label format, which needs to
be imported separately.  For instance:

@
 import "LIO"
 -- Import your favorite label format:
 import "LIO.DCLabel"
@

WARNING:  For security, untrusted code must always be compiled with
the @-XSafe@ and @-fpackage-trust@ /SafeHaskell/ flags. See
<http://hackage.haskell.org/trac/ghc/wiki/SafeHaskell> for more
details on the guarantees provided by SafeHaskell.

-}

module LIO ( 
    module LIO.Core
  , module LIO.Delegate
  , module LIO.Exception
  , module LIO.Label
  , module LIO.Labeled
  ) where

import safe LIO.Core
import safe LIO.Delegate
import safe LIO.Exception
import safe LIO.Label
import safe LIO.Labeled
