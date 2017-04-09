{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Exponentail 'Ring' and 'Field'
module NumHask.Algebra.Exponential (
    -- * Exponential
    ExpRing(..)
  , (^)
  , ExpField(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float)
import NumHask.Algebra.Field
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Additive
import NumHask.Algebra.Ring

-- | ExpRing
class Ring a => ExpRing a where
    logBase :: a -> a -> a
    (**) :: a -> a -> a

-- | (^)
(^) :: ExpRing a => a -> a -> a
(^) = (**)

instance ExpRing Double where
    logBase = P.logBase
    (**) = (P.**)
instance ExpRing Float where
    logBase = P.logBase
    (**) = (P.**)

-- | ExpField
class ( Field a
      , ExpRing a ) =>
      ExpField a where
    sqrt :: a -> a
    sqrt a = a**(one/(one+one))

    exp :: a -> a
    log :: a -> a

instance ExpField Double where
    exp = P.exp
    log = P.log

instance ExpField Float where
    exp = P.exp
    log = P.log

