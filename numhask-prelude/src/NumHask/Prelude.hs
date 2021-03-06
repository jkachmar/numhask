{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Combines 'Protolude' and 'numhask'.
module NumHask.Prelude
  ( -- * NumHask
    -- $instances

    module NumHask.Algebra.Abstract.Action,
    module NumHask.Algebra.Abstract.Additive,
    module NumHask.Algebra.Abstract.Field,
    module NumHask.Algebra.Abstract.Group,
    module NumHask.Algebra.Abstract.Lattice,
    module NumHask.Algebra.Abstract.Module,
    module NumHask.Algebra.Abstract.Multiplicative,
    module NumHask.Algebra.Abstract.Ring,
    module NumHask.Algebra.Linear.Hadamard,
    module NumHask.Analysis.Metric,
    module NumHask.Data.Complex,
    module NumHask.Data.Integral,
    module NumHask.Data.LogField,
    module NumHask.Data.Rational,
    module NumHask.Data.Pair,
    module NumHask.Data.Positive,
    Natural (..),
    module NumHask.Exception,

    -- * Backend
    -- $backend

    id,
    module Protolude,
    -- | Using different types for numbers requires RebindableSyntax.  This then removes all sorts of base-level stuff that has to be put back in.
    fromString,
    fail,
    ifThenElse,
    fromList,
    fromListN,
  )
where

import Control.Monad (fail)
import Data.String
import GHC.Exts
import GHC.Natural (Natural (..))
import NumHask.Algebra.Abstract.Action
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Module
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Linear.Hadamard
import NumHask.Analysis.Metric
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.LogField
import NumHask.Data.Pair
import NumHask.Data.Positive
import NumHask.Data.Rational
import NumHask.Exception
import Protolude hiding ((*), (**), (+), (-), (/), Complex (..), Integral (..), Product (..), Ratio, Rep, Semiring (..), Sum (..), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cis, cos, cosh, even, exp, floor, fromInteger, fromIntegral, fromRational, gcd, imagPart, infinity, log, logBase, magnitude, mkPolar, negate, odd, phase, pi, polar, product, properFraction, realPart, recip, reduce, round, sin, sinh, sqrt, subtract, sum, tan, tanh, toInteger, toRational, trans, truncate, zero)
import Control.Category (id)

-- $backend
-- NumHask imports Protolude as a starting prelude.
--
-- In addition, 'id' is imported (protolude uses 'identity')

-- $instances
-- NumHask replaces much of the 'Num' and 'Real' heirarchies in protolude & base.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool', 'Complex' and 'Natural'are supplied.

-- | rebindable syntax splats this, and I'm not sure where it exists in GHC land
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
