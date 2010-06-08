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
    ( -- * Memory as a scarce resource
      RegionalPtr

      {-| Note that this module re-exports the @Control.Monad.Trans.Region@
      module from the @regions@ package which allows you to:

      * Run a region using 'runRegionT'.

      * Concurrently run a region inside another region using 'forkTopRegion'.

       * Duplicate a 'RegionalPtr' to a parent region using 'dup'.
      -}
    , module Control.Monad.Trans.Region

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
import Data.Int                    ( Int )
import           Foreign.Ptr       ( Ptr )
import qualified Foreign.Ptr as FP ( castPtr, plusPtr, alignPtr, minusPtr )

-- from regions:
import Control.Monad.Trans.Region -- (re-exported entirely)

-- from ourselves:
import Foreign.Ptr.Region.Internal ( RegionalPtr(RegionalPtr) )
import Foreign.Ptr.Region.Unsafe   ( unsafePtr )


--------------------------------------------------------------------------------
-- Pure functions on regional pointers
--------------------------------------------------------------------------------

mapRegionalPtr ∷ (Ptr α → Ptr β) → (RegionalPtr α r → RegionalPtr β r)
mapRegionalPtr f (RegionalPtr ptr ch) = RegionalPtr (f ptr) ch


-- | Wraps: @Foreign.Ptr.@'FP.castPtr'
castPtr ∷ RegionalPtr α r → RegionalPtr β r
castPtr = mapRegionalPtr FP.castPtr

-- | Wraps: @Foreign.Ptr.@'FP.plusPtr'
plusPtr ∷ RegionalPtr α r → Int → RegionalPtr β r
plusPtr rp n = mapRegionalPtr (\p → FP.plusPtr p n) rp

-- | Wraps: @Foreign.Ptr.@'FP.alignPtr'
alignPtr ∷ RegionalPtr α r → Int → RegionalPtr α r
alignPtr rp n = mapRegionalPtr (\p → FP.alignPtr p n) rp

-- | Wraps: @Foreign.Ptr.@'FP.minusPtr'
minusPtr ∷ RegionalPtr α r → RegionalPtr β r → Int
minusPtr rp1 rp2 = FP.minusPtr (unsafePtr rp1) (unsafePtr rp2)


-- The End ---------------------------------------------------------------------
