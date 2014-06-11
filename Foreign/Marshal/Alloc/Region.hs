{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , RankNTypes
           , ScopedTypeVariables
           , FlexibleContexts
           , CPP
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Alloc.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Alloc.Region
    ( -- * Local allocation
      LocalPtr
    , alloca
    , allocaBytes
#if MIN_VERSION_base(4,3,0)
    , allocaBytesAligned
#endif
      -- * Dynamic allocation
    , malloc
    , mallocBytes
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Int                               ( Int )
import Foreign.Storable                       ( Storable )
import Prelude                                ( IO )

import qualified Foreign.Marshal.Alloc as FMA ( alloca, allocaBytes
                                              , malloc, mallocBytes
                                              )
#if MIN_VERSION_base(4,3,0)
import qualified Foreign.Marshal.Alloc as FMA ( allocaBytesAligned )
#endif

#if __HADDOCK__
import Foreign.Storable ( sizeOf )
#endif

-- from regions:
import Control.Monad.Trans.Region ( RegionT, RegionBaseControl, LocalRegion, Local )

-- from ourselves:
import Foreign.Ptr.Region          ( RegionalPtr,  )
import Foreign.Ptr.Region.Internal ( LocalPtr )
import Foreign.Ptr.Region.Unsafe   ( wrapAlloca, wrapMalloc )


--------------------------------------------------------------------------------
-- * Local allocation
--------------------------------------------------------------------------------

{-|
@'alloca' f@ executes the computation @f@, passing as argument a pointer to
a temporarily allocated block of memory sufficient to hold values of type @&#945;@.

The memory is freed when @f@ terminates (either normally or via an exception).

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.alloca'@.
-}
alloca ∷ (Storable α, RegionBaseControl IO pr)
       ⇒ (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
       → RegionT s pr β
alloca = wrapAlloca FMA.alloca

{-|
@'allocaBytes' n f@ executes the computation @f@, passing as argument a
pointer to a temporarily allocated block of memory of @n@ bytes.
The block of memory is sufficiently aligned for any of the basic foreign types
that fits into a memory block of the allocated size.

The memory is freed when @f@ terminates (either normally or via an exception).

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.allocaBytes'@.
-}
allocaBytes ∷ RegionBaseControl IO pr
            ⇒ Int
            → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
            → RegionT s pr β
allocaBytes size = wrapAlloca (FMA.allocaBytes size)

#if MIN_VERSION_base(4,3,0)
-- | This should provide a safer replacement for:
-- @Foreign.Marshal.Alloc.'FMA.allocaBytesAligned'@.
allocaBytesAligned ∷
    RegionBaseControl IO pr
  ⇒ Int → Int
  → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
  → RegionT s pr β
allocaBytesAligned size align = wrapAlloca (FMA.allocaBytesAligned size align)
#endif


--------------------------------------------------------------------------------
-- * Dynamic allocation
--------------------------------------------------------------------------------

{-|
Allocate a block of memory that is sufficient to hold values of type @&#945;@.

Note that: @malloc = 'mallocBytes' $ 'sizeOf' (undefined :: &#945;)@

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.malloc'@.
-}
malloc ∷ ∀ α pr s. (Storable α, RegionBaseControl IO pr)
       ⇒ RegionT s pr (RegionalPtr α (RegionT s pr))
malloc = wrapMalloc FMA.malloc

{-|
Allocate a block of memory of the given number of bytes.
The block of memory is sufficiently aligned for any of the basic foreign types
that fits into a memory block of the allocated size.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.mallocBytes'@.
-}
mallocBytes ∷ RegionBaseControl IO pr
            ⇒ Int
            → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocBytes size = wrapMalloc (FMA.mallocBytes size)

-- TODO:
-- realloc ∷ (Storable β, pr `AncestorRegion` cr, MonadIO cr)
--         ⇒ RegionalPtr α pr → cr (RegionalPtr β pr)
-- realloc = ...
-- reallocBytes ∷ (pr `AncestorRegion` cr, MonadIO cr)
--              ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)
-- reallocBytes = ...


-- The End ---------------------------------------------------------------------
