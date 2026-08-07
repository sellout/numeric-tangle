{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- @since 0.0.1
module Numeric.Ration
  ( Ration,
    Result,
    rationalize,
    (%),
  )
where

import "base" Data.Int (Int)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Ratio (Ratio, Rational)
import "base" Data.Ratio qualified as Ratio ((%))
import "base" Foreign.C.Types (CDouble, CFloat)
import "base" Numeric.Natural (Natural)
import "base" Prelude (Double, Float, Integer, Integral, Real, toRational)
import "base" Prelude qualified as Base ((/))

-- | To approximate a numerical tower, this provides an operator that always
--   results in an exact ratio, regardless of what `Real` it’s applied to.
--
-- @since 0.0.1
type Ration :: Type -> Constraint
class (Real n) => Ration n where
  -- | The exact ratio that dividing two @n@ produces. For an `Integral` type
  --   this is generally @`Ratio` n@, but types that can’t be the components of
  --   a `Ratio` (`Float`, say) widen to some other exact representation.
  --
  -- @since 0.0.1
  type Result n

  -- | Divide two values, producing an exact ratio rather than approximating
  --   the result in the type of the arguments.
  --
  -- @since 0.0.1
  (%) :: n -> n -> Result n

infixl 7 %

-- | Lifts an integral value into the corresponding ratio, by giving it a
--   denominator of 1.
--
-- @since 0.0.1
rationalize :: (Integral n, Ration n) => n -> Result n
rationalize = (% 1)

-- |
--
-- @since 0.0.1
instance Ration Int where
  type Result Int = Ratio Int
  (%) = (Ratio.%)

-- |
--
-- @since 0.0.1
instance Ration Integer where
  type Result Integer = Ratio Integer
  (%) = (Ratio.%)

-- |
--
-- @since 0.0.1
instance Ration Natural where
  type Result Natural = Ratio Natural
  (%) = (Ratio.%)

-- |
--
-- @since 0.0.1
instance (Integral n) => Ration (Ratio n) where
  type Result (Ratio n) = Ratio n
  (%) = (Base./)

-- |
--
-- @since 0.0.1
instance Ration CDouble where
  type Result CDouble = Rational
  a % b = toRational a Base./ toRational b

-- |
--
-- @since 0.0.1
instance Ration CFloat where
  type Result CFloat = Rational
  a % b = toRational a Base./ toRational b

-- |
--
-- @since 0.0.1
instance Ration Double where
  type Result Double = Rational
  a % b = toRational a Base./ toRational b

-- |
--
-- @since 0.0.1
instance Ration Float where
  type Result Float = Rational
  a % b = toRational a Base./ toRational b
