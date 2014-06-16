{-# LANGUAGE NoImplicitPrelude
           , RankNTypes
           , ScopedTypeVariables
           , FlexibleContexts
           , CPP
           , TypeFamilies #-}

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
    , allocaBytesAligned
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
import System.IO                              ( IO )

import qualified Foreign.Marshal.Alloc as FMA ( alloca, allocaBytes
                                              , malloc, mallocBytes
                                              , allocaBytesAligned
                                              )

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
a temporarily allocated block of memory sufficient to hold values of type @a@.

The memory is freed when @f@ terminates (either normally or via an exception).

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.alloca'@.
-}
alloca :: (RegionBaseControl IO pr, Storable a)
       => (forall sl. LocalPtr a (LocalRegion sl s)
          -> RegionT (Local s) pr b)
       -> RegionT s pr b
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
allocaBytes :: (RegionBaseControl IO pr)
            => Int
            -> (forall sl. LocalPtr a (LocalRegion sl s)
               -> RegionT (Local s) pr b)
            -> RegionT s pr b
allocaBytes size = wrapAlloca (FMA.allocaBytes size)

-- | This should provide a safer replacement for:
-- @Foreign.Marshal.Alloc.'FMA.allocaBytesAligned'@.
allocaBytesAligned
  :: (RegionBaseControl IO pr)
  => Int -> Int
  -> (forall sl. LocalPtr a (LocalRegion sl s)
     -> RegionT (Local s) pr b)
  -> RegionT s pr b
allocaBytesAligned size align = wrapAlloca (FMA.allocaBytesAligned size align)


--------------------------------------------------------------------------------
-- * Dynamic allocation
--------------------------------------------------------------------------------

{-|
Allocate a block of memory that is sufficient to hold values of type @a@.

Note that: @malloc = 'mallocBytes' $ 'sizeOf' (undefined :: a)@

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.malloc'@.
-}
malloc :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
       => region (RegionalPtr a region)
malloc = wrapMalloc FMA.malloc

{-|
Allocate a block of memory of the given number of bytes.
The block of memory is sufficiently aligned for any of the basic foreign types
that fits into a memory block of the allocated size.

This should provide a safer replacement for:
@Foreign.Marshal.Alloc.'FMA.mallocBytes'@.
-}
mallocBytes :: (region ~ RegionT s pr, RegionBaseControl IO pr)
            => Int
            -> region (RegionalPtr a region)
mallocBytes size = wrapMalloc (FMA.mallocBytes size)

-- TODO:
-- realloc :: (Storable b, pr `AncestorRegion` cr, MonadBase IO cr)
--         => RegionalPtr a pr -> cr (RegionalPtr b pr)
-- realloc = ...
-- reallocBytes :: (pr `AncestorRegion` cr, MonadBase IO cr)
--              => RegionalPtr a pr -> Int -> cr (RegionalPtr a pr)
-- reallocBytes = ...
