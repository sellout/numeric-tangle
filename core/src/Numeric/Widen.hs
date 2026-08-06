{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- @since 0.0.1
module Numeric.Widen
  ( Widen,
    Failure (Overflow, Interstitial, Underflow),
    narrow,
    widen,
  )
where

import "base" Control.Applicative (Applicative, pure)
import "base" Control.Category ((.))
import "base" Data.Complex (Complex ((:+)), imagPart, realPart)
import "base" Data.Either (Either)
import "base" Data.Fixed (Fixed, HasResolution)
import "base" Data.Function (const)
import "base" Data.Functor.Const (Const (Const), getConst)
import "base" Data.Functor.Contravariant (Op (Op))
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.Int (Int, Int16, Int32, Int64, Int8)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid
  ( Alt (Alt),
    Ap,
    Product (Product),
    Sum (Sum),
    getAlt,
    getProduct,
    getSum,
  )
import "base" Data.Ord (Down (Down), getDown)
import "base" Data.Ratio (Ratio, Rational, denominator, numerator, (%))
import "base" Data.Semigroup (Max (Max), Min (Min), getMax, getMin)
import "base" Data.Word (Word, Word16, Word32, Word64, Word8)
import "base" Foreign.C.Types
  ( CBool,
    CChar,
    CDouble,
    CFloat,
    CSChar,
    CShort,
    CSigAtomic,
    CUChar,
    CUShort,
    CWchar,
  )
import "base" Numeric.Natural (Natural)
import "base" Prelude
  ( Double,
    Float,
    Integer,
    Integral,
    -- Num,
    fromIntegral,
    realToFrac,
    toRational,
    undefined,
  )

-- | The ways in which `narrow` can fail to represent a value in the narrower
--   type.
--
-- @since 0.0.1
type Failure :: Type
data Failure
  = -- | The value is greater than the largest value of the destination type.
    Overflow
  | -- | This indicates that narrowing a type resulted in a value that is
    --   between valid values of the destination type, so precision would be
    --   lost.
    Interstitial
  | -- | The value is less than the smallest value of the destination type.
    Underflow

-- | This widens a numeric type to one that contains a strict superset of the
--   values (that is, you can not widen a type to itself). It also only widens
--   to “neighboring types”, so you need to step through `widen` multiple times
--   to traverse the tangle.
--
--  __NB__: This offers a `Prism`, which is useful for testing round-trips.
--
-- @since 0.0.1
type Widen :: Type -> Type -> Constraint
class Widen from to where
  -- | This may use methods like `fromIntegral`, but it is recommended to use
  --   more direct implementations when available.
  --
  -- @since 0.0.1
  widen :: from -> to

  -- | The partial inverse of `widen`, returning a `Failure` when the value
  --   can’t be represented in the narrower type.
  --
  -- @since 0.0.1
  narrow :: to -> Either Failure from
  -- __FIXME__: Remove this default impl.
  narrow = undefined

-- * from `CBool`

-- |
--
-- @since 0.0.1
instance Widen CBool CChar where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CBool CSigAtomic where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CBool CWchar where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CBool Int8 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CBool Word8 where
  widen = fromIntegral

-- * from `Int8`

-- |
--
-- @since 0.0.1
instance Widen Int8 CSChar where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int8 Int16 where
  widen = fromIntegral

-- * from `Word8`

-- |
--
-- @since 0.0.1
instance Widen Word8 CUChar where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Word8 Int16 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Word8 Word16 where
  widen = fromIntegral

-- * from `CSChar`

-- |
--
-- @since 0.0.1
instance Widen CSChar CShort where
  widen = fromIntegral

-- * from `CUChar`

-- |
--
-- @since 0.0.1
instance Widen CUChar CShort where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CUChar CUShort where
  widen = fromIntegral

-- * from `Word16`

-- |
--
-- @since 0.0.1
instance Widen Word16 Word32 where
  widen = fromIntegral

-- * non-exhaustive

-- |
--
-- @since 0.0.1
instance Widen Int16 Int32 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int32 Int64 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int64 Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Natural Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen CChar Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Word32 Word64 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Word64 Natural where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Float Double where
  widen = realToFrac

-- |
--
-- @since 0.0.1
instance Widen CFloat CDouble where
  widen = realToFrac

-- |
--
-- @since 0.0.1
instance Widen Double Rational where
  widen = toRational

-- |
--
-- @since 0.0.1
instance Widen CDouble Rational where
  widen = toRational

-- `Int` is underspecifed, so we need to treat it as at least a 30-bit number,
-- but it may be larger.

-- |
--
-- @since 0.0.1
instance Widen Int16 Int where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int Integer where
  widen = fromIntegral

-- `Word` is underspecifed, so we need to treat it as at least a 30-bit number,
-- but it may be larger.

-- |
--
-- @since 0.0.1
instance Widen Word16 Word where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Word Natural where
  widen = fromIntegral

-- general instances

-- |
--
-- @since 0.0.1
instance Widen (Alt f a) (f a) where
  widen = getAlt

-- |
--
-- @since 0.0.1
instance Widen (f a) (Alt f a) where
  widen = Alt

-- |
--
-- @since 0.0.1
instance (Applicative f) => Widen a (Ap f a) where
  widen = pure

-- instance (Num a) => Widen a (Complex a) where
--   widen a = a :+ 0

-- instance {-# OVERLAPPABLE #-} (Widen a b, Num b) => Widen a (Complex b) where
--   widen a = widen a :+ 0

-- |
--
-- @since 0.0.1
instance (Widen a b) => Widen (Complex a) (Complex b) where
  widen a = widen (realPart a) :+ widen (imagPart a)

-- |
--
-- @since 0.0.1
instance Widen (Const a b) a where
  widen = getConst

-- |
--
-- @since 0.0.1
instance Widen a (Const a b) where
  widen = Const

-- |
--
-- @since 0.0.1
instance Widen (Down a) a where
  widen = getDown

-- |
--
-- @since 0.0.1
instance Widen a (Down a) where
  widen = Down

-- |
--
-- @since 0.0.1
instance (HasResolution n) => Widen (Fixed n) Rational where
  widen = toRational

-- |
--
-- @since 0.0.1
instance Widen (Identity a) a where
  widen = runIdentity

-- |
--
-- @since 0.0.1
instance Widen a (Identity a) where
  widen = Identity

-- |
--
-- @since 0.0.1
instance Widen (Max a) a where
  widen = getMax

-- |
--
-- @since 0.0.1
instance Widen a (Max a) where
  widen = Max

-- |
--
-- @since 0.0.1
instance Widen (Min a) a where
  widen = getMin

-- |
--
-- @since 0.0.1
instance Widen a (Min a) where
  widen = Min

-- |
--
-- @since 0.0.1
instance Widen a (Op a b) where
  widen = Op . const

-- |
--
-- @since 0.0.1
instance Widen (Product a) a where
  widen = getProduct

-- |
--
-- @since 0.0.1
instance Widen a (Product a) where
  widen = Product

-- |
--
-- @since 0.0.1
instance Widen (Sum a) a where
  widen = getSum

-- |
--
-- @since 0.0.1
instance Widen a (Sum a) where
  widen = Sum

-- instance (Integral a) => Widen a (Ratio a) where
--   widen a = a % 1

-- instance {-# OVERLAPPABLE #-} (Widen a b, Integral b) => Widen a (Ratio b) where
--   widen a = widen a % 1

-- |
--
-- @since 0.0.1
instance (Widen a b, Integral b) => Widen (Ratio a) (Ratio b) where
  widen a = widen (numerator a) % widen (denominator a)

-- transitive instances

-- |
--
-- @since 0.0.1
instance Widen Int8 Int where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int8 Int32 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int8 Int64 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int8 Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int16 Int64 where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int16 Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Int32 Integer where
  widen = fromIntegral

-- |
--
-- @since 0.0.1
instance Widen Float Rational where
  widen = toRational

-- |
--
-- @since 0.0.1
instance Widen CFloat Rational where
  widen = toRational
