{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region
    ( {-|
      Note that this module re-exports the @Control.Monad.Trans.Region@ module
      from the @regions@ package which allows you to run regions using 'runRegionT'
      and duplicate a 'RegionalPtr' to a parent region using 'dup'.
      -}
      module Control.Monad.Trans.Region

      -- * Regional pointers
    , RegionalPtr

      -- * Null pointers
    , nullPtr, NullPtr

      -- * Class of pointers
    , Pointer(mapPointer)
    , AllocatedPointer

      -- *  Pure functions on pointers
    , castPtr
    , alignPtr
    , plusPtr
    , minusPtr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Int ( Int )
import qualified Foreign.Ptr as FP ( castPtr, plusPtr, alignPtr, minusPtr )

-- from regions:
import Control.Monad.Trans.Region -- (re-exported entirely)

-- from ourselves:
import Foreign.Ptr.Region.Internal ( RegionalPtr
                                   , NullPtr, nullPtr
                                   , Pointer(unsafePtr, mapPointer)
                                   , AllocatedPointer
                                   )


--------------------------------------------------------------------------------
-- * Pure functions on regional pointers
--------------------------------------------------------------------------------

-- | The @castPtr@ function casts a pointer from one type to another.
--
-- Wraps: @Foreign.Ptr.@'FP.castPtr'
castPtr :: (Pointer pointer) => pointer a r -> pointer b r
castPtr = mapPointer FP.castPtr

-- | Given an arbitrary address and an alignment constraint, @alignPtr@ yields
-- the next higher address that fulfills the alignment constraint. An alignment
-- constraint @x@ is fulfilled by any address divisible by @x@.  This operation
-- is idempotent.
--
-- Wraps: @Foreign.Ptr.@'FP.alignPtr'
alignPtr :: (AllocatedPointer pointer) => pointer a r -> Int -> pointer a r
alignPtr pointer n = mapPointer (\ptr -> FP.alignPtr ptr n) pointer

-- | Advances the given address by the given offset in bytes.
--
-- Wraps: @Foreign.Ptr.@'FP.plusPtr'
plusPtr :: (AllocatedPointer pointer) => pointer a r -> Int -> pointer b r
plusPtr pointer n = mapPointer (\ptr -> FP.plusPtr ptr n) pointer

-- | Computes the offset required to get from the second to the first
-- argument. We have
--
-- > p2 == p1 `plusPtr` (p2 `minusPtr` p1)
--
-- Wraps: @Foreign.Ptr.@'FP.minusPtr'
minusPtr :: (AllocatedPointer pointer) => pointer a r1 -> pointer b r2 -> Int
minusPtr pointer1 pointer2 = FP.minusPtr (unsafePtr pointer1)
                                         (unsafePtr pointer2)
