{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region
    ( -- * Regional pointers
      RegionalPtr

      {-| Note that this module re-exports the @Control.Monad.Trans.Region@
      module from the @regions@ package which allows you to:

      * Run a region using 'runRegionT'.

      * Concurrently run a region inside another region using 'forkTopRegion'.

      * Duplicate a 'RegionalPtr' to a parent region using 'dup'.
      -}
    , module Control.Monad.Trans.Region

      -- * Constructing regional pointers
    , regionalPtr
    , nullPtr

      -- *  Pure functions on regional pointers
    , mapRegionalPtr

    , castPtr
    , plusPtr
    , alignPtr
    , minusPtr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                     ( return, (>>=), fail )
import Data.Function                     ( ($) )
import Data.Int                          ( Int )
import Data.Maybe                        ( Maybe(Nothing, Just) )
import           Foreign.Ptr             ( Ptr )
import qualified Foreign.Ptr as FP       ( nullPtr
                                         , castPtr, plusPtr, alignPtr, minusPtr
                                         )

-- from transformers:
import Control.Monad.IO.Class            ( MonadIO )

-- from regions:
import Control.Monad.Trans.Region     -- (re-exported entirely)
import Control.Monad.Trans.Region.OnExit ( CloseAction, onExit )

-- from ourselves:
import Foreign.Ptr.Region.Internal       ( RegionalPtr(RegionalPtr) )
import Foreign.Ptr.Region.Unsafe         ( unsafePtr )


--------------------------------------------------------------------------------
-- * Constructing regional pointers
--------------------------------------------------------------------------------

-- | Construct a regional pointer from a native pointer and an @IO@ computation
-- that frees the pointer which is executed when the region exits.
regionalPtr ∷ MonadIO pr
            ⇒ Ptr α
            → CloseAction
            → RegionT s pr (RegionalPtr α (RegionT s pr))
regionalPtr ptr freePtr = do ch ← onExit freePtr
                             return $ RegionalPtr ptr $ Just ch

-- | The constant @nullPtr@ contains a distinguished value of 'RegionalPtr'
-- that is not associated with a valid memory location.
nullPtr ∷ RegionalPtr α r
nullPtr = RegionalPtr FP.nullPtr Nothing


--------------------------------------------------------------------------------
-- * Pure functions on regional pointers
--------------------------------------------------------------------------------

-- | Apply a /pure/ function to the inner pointer of a regional pointer.
mapRegionalPtr ∷ (Ptr α → Ptr β) → (RegionalPtr α r → RegionalPtr β r)
mapRegionalPtr f = \(RegionalPtr ptr ch) → RegionalPtr (f ptr) ch

-- | The @castPtr@ function casts a pointer from one type to another.
--
-- Wraps: @Foreign.Ptr.@'FP.castPtr'
castPtr ∷ RegionalPtr α r → RegionalPtr β r
castPtr = mapRegionalPtr FP.castPtr

-- | Advances the given address by the given offset in bytes.
--
-- Wraps: @Foreign.Ptr.@'FP.plusPtr'
plusPtr ∷ RegionalPtr α r → Int → RegionalPtr β r
plusPtr rp n = mapRegionalPtr (\p → FP.plusPtr p n) rp

-- | Given an arbitrary address and an alignment constraint, @alignPtr@ yields
-- the next higher address that fulfills the alignment constraint. An alignment
-- constraint @x@ is fulfilled by any address divisible by @x@.  This operation
-- is idempotent.
--
-- Wraps: @Foreign.Ptr.@'FP.alignPtr'
alignPtr ∷ RegionalPtr α r → Int → RegionalPtr α r
alignPtr rp n = mapRegionalPtr (\p → FP.alignPtr p n) rp

-- | Computes the offset required to get from the second to the first
-- argument. We have
--
-- > p2 == p1 `plusPtr` (p2 `minusPtr` p1)
--
-- Wraps: @Foreign.Ptr.@'FP.minusPtr'
minusPtr ∷ RegionalPtr α r1 → RegionalPtr β r2 → Int
minusPtr rp1 rp2 = FP.minusPtr (unsafePtr rp1) (unsafePtr rp2)


-- The End ---------------------------------------------------------------------
