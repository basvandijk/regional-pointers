{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , RankNTypes
           , ScopedTypeVariables
           , CPP
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Alloc.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Alloc.Region
    ( -- * Local allocation
      alloca
    , allocaBytes

      -- * Dynamic allocation
    , malloc
    , mallocBytes

      -- | /TODO:/ Define and export: @realloc@ and @reallocBytes@.
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                          ( (>>=) )
import Data.Function                          ( ($) )
import Data.Int                               ( Int )
import Foreign.Storable                       ( Storable, sizeOf )

import Foreign.Marshal.Alloc                  ( free )

import qualified Foreign.Marshal.Alloc as FMA ( mallocBytes )

#ifdef __HADDOCK__
import qualified Foreign.Marshal.Alloc as FMA ( alloca, allocaBytes, malloc )
#endif

#if __GLASGOW_HASKELL__ < 701
import Control.Monad                          ( fail )
#endif

-- from base-unicode-symbols:
import Prelude.Unicode                        ( (⊥) )

-- from transformers:
import Control.Monad.IO.Class                 ( liftIO )

-- from monad-peel:
import Control.Monad.IO.Peel                  ( MonadPeelIO )
import Control.Exception.Peel                 ( block )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT, runRegionT )

-- from ourselves:
import Foreign.Ptr.Region                     ( RegionalPtr  )
import Foreign.Ptr.Region.Unsafe              ( unsafeRegionalPtr )


--------------------------------------------------------------------------------
-- * Local allocation
--------------------------------------------------------------------------------

{-| Convenience function which allocates sufficient memory to hold values of
type @&#945;@, applies the given continuation function to the resulting regional
pointer and runs the resulting region.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.alloca'@.

Note that: @alloca = 'allocaBytes' $ 'sizeOf' (undefined :: &#945;)@
-}
alloca ∷ ∀ α pr β. (Storable α, MonadPeelIO pr)
       ⇒ (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
       → pr β
alloca = allocaBytes $ sizeOf ((⊥) ∷ α)

{-| Convenience function which allocates the given number of bytes, applies the
given continuation function to the resulting regional pointer and runs the
resulting region.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.allocaBytes'@.

Note that: @allocaBytes size f = 'runRegionT' $ 'mallocBytes' size >>= f@
-}
allocaBytes ∷ ∀ α pr β. MonadPeelIO pr
            ⇒ Int
            → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
            → pr β
allocaBytes size f = runRegionT $ mallocBytes size >>= f


--------------------------------------------------------------------------------
-- * Dynamic allocation
--------------------------------------------------------------------------------

{-| Convenience function which allocates sufficient memory to hold values of
type @&#945;@ and returns a regional pointer to them.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.malloc'@.

Note that: @malloc = 'mallocBytes' $ 'sizeOf' (undefined :: &#945;)@
-}
malloc ∷ ∀ α pr s. (Storable α, MonadPeelIO pr)
       ⇒ RegionT s pr (RegionalPtr α (RegionT s pr))
malloc = mallocBytes $ sizeOf ((⊥) ∷ α)

{-| Allocates the given number of bytes and returns a regional pointer to them.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.mallocBytes'@.
-}
mallocBytes ∷ MonadPeelIO pr
            ⇒ Int
            → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocBytes size = block $ do
                     ptr ← liftIO $ FMA.mallocBytes size
                     unsafeRegionalPtr ptr $ free ptr

-- TODO:
-- realloc ∷ (Storable β, pr `AncestorRegion` cr, MonadIO cr)
--         ⇒ RegionalPtr α pr → cr (RegionalPtr β pr)
-- realloc = ...
-- reallocBytes ∷ (pr `AncestorRegion` cr, MonadIO cr)
--              ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)
-- reallocBytes = ...


-- The End ---------------------------------------------------------------------
