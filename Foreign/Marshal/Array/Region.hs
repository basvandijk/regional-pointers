{-# LANGUAGE NoImplicitPrelude
           , CPP
           , RankNTypes
           , FlexibleContexts
           , ImpredicativeTypes
           , TypeFamilies
           , TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Array.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Array.Region
    ( -- * Allocation
      mallocArray, mallocArray0
    , allocaArray, allocaArray0

      -- * Marshalling
    , peekArray, peekArray0
    , pokeArray, pokeArray0

      -- * Combined allocation and marshalling
    , newArray,     newArray0
    , withArray,    withArray0
    , withArrayLen, withArrayLen0

      -- * Copying
    , copyArray, moveArray

      -- * Finding the length
    , lengthArray0

      -- * Indexing
    , advancePtr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Category                       ( (.) )
import Data.Function                          ( ($) )
import Data.Int                               ( Int )
import Data.Eq                                ( Eq )
import Foreign.Storable                       ( Storable )
import qualified Foreign.Marshal.Array as FMA ( mallocArray,  mallocArray0

                                              , allocaArray,  allocaArray0

                                              , peekArray,    peekArray0
                                              , pokeArray,    pokeArray0

                                              , newArray,     newArray0
                                              , withArray,    withArray0

                                              , withArrayLen, withArrayLen0

                                              , copyArray, moveArray

                                              , lengthArray0
                                              , advancePtr
                                              )
import System.IO                              ( IO )

-- from transformers-base:
import Control.Monad.Base                     ( MonadBase, liftBase )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT
                                              , RegionBaseControl
                                              , AncestorRegion
                                              , LocalRegion, Local
                                              )

-- from ourselves:
import Foreign.Ptr.Region                     ( AllocatedPointer, mapPointer
                                              , RegionalPtr
                                              )
import Foreign.Marshal.Alloc.Region           ( LocalPtr )
import Foreign.Ptr.Region.Unsafe              ( unsafePtr
                                              , unsafeWrap2, unsafeWrap2flp
                                              , wrapAlloca,  wrapAlloca2
                                              , wrapMalloc
                                              )

#ifdef __HADDOCK__
import Foreign.Marshal.Alloc.Region           ( malloc, alloca )
import Foreign.Marshal.Utils.Region           ( new, with )
#endif


--------------------------------------------------------------------------------
-- * Allocation
--------------------------------------------------------------------------------

-- | Allocate storage for the given number of elements of a storable type.
--
-- Like 'malloc', but for multiple elements.
mallocArray :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
            => Int -> region (RegionalPtr a region)
mallocArray = wrapMalloc . FMA.mallocArray

-- | Like 'mallocArray', but add an extra position to hold a special termination
-- element.
mallocArray0 :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
             => Int -> region (RegionalPtr a region)
mallocArray0 = wrapMalloc . FMA.mallocArray0

-- | Temporarily allocate space for the given number of elements (like 'alloca',
-- but for multiple elements).
allocaArray :: (Storable a, RegionBaseControl IO pr)
            => Int
            -> (forall sl. LocalPtr a (LocalRegion sl s)
               -> RegionT (Local s) pr b)
            -> RegionT s pr b
allocaArray = wrapAlloca . FMA.allocaArray

-- | Like 'allocaArray', but add an extra position to hold a special termination
-- element.
allocaArray0 :: (Storable a, RegionBaseControl IO pr)
             => Int
             -> (forall sl. LocalPtr a (LocalRegion sl s)
                -> RegionT (Local s) pr b)
             -> RegionT s pr b
allocaArray0 = wrapAlloca . FMA.allocaArray0

-- TODO:
-- reallocArray  :: Storable a => RegionalPtr a pr -> Int -> cr (RegionalPtr a pr)
-- reallocArray0 :: Storable a => RegionalPtr a pr -> Int -> cr (RegionalPtr a pr)


--------------------------------------------------------------------------------
-- * Marshalling
--------------------------------------------------------------------------------

-- | Convert an array of given length into a Haskell list.
--
-- (This version traverses the array backwards using an accumulating parameter,
-- which uses constant stack space. The previous version using @mapM@ needed
-- linear stack space.)
--
-- Wraps: @Foreign.Marshal.Array.'FMA.peekArray'@.
peekArray :: ( AllocatedPointer pointer, Storable a
             , pr `AncestorRegion` cr, MonadBase IO cr
             )
          => Int -> pointer a pr -> cr [a]
peekArray =  unsafeWrap2flp FMA.peekArray

-- | Convert an array terminated by the given end marker into a Haskell list.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.peekArray0'@.
peekArray0 :: ( AllocatedPointer pointer, Storable a, Eq a
              , pr `AncestorRegion` cr, MonadBase IO cr
              )
           => a -> pointer a pr -> cr [a]
peekArray0 = unsafeWrap2flp FMA.peekArray0

-- | Write the list elements consecutive into memory.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray'@.
pokeArray :: ( AllocatedPointer pointer, Storable a
             , pr `AncestorRegion` cr, MonadBase IO cr
             )
          => pointer a pr -> [a] -> cr ()
pokeArray = unsafeWrap2 FMA.pokeArray

-- | Write the list elements consecutive into memory and terminate them with the
-- given marker element.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray0'@.
pokeArray0 :: ( AllocatedPointer pointer, Storable a
              , pr `AncestorRegion` cr, MonadBase IO cr
              )
           => a -> pointer a pr -> [a] -> cr ()
pokeArray0 m rp xs = liftBase $ FMA.pokeArray0 m (unsafePtr rp) xs


--------------------------------------------------------------------------------
-- * Combined allocation and marshalling
--------------------------------------------------------------------------------

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values.
--
-- Like 'new', but for multiple elements.
newArray :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
         => [a] -> region (RegionalPtr a region)
newArray = wrapMalloc . FMA.newArray

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end marker.
newArray0 :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
          => a -> [a] -> region (RegionalPtr a region)
newArray0 marker vals = wrapMalloc (FMA.newArray0 marker vals)

-- | Temporarily store a list of storable values in memory.
--
-- Like 'with', but for multiple elements.
withArray :: (Storable a, RegionBaseControl IO pr)
          => [a]
          -> (forall sl. LocalPtr a (LocalRegion sl s)
             -> RegionT (Local s) pr b)
          -> RegionT s pr b
withArray = wrapAlloca . FMA.withArray

-- | Like 'withArray', but a terminator indicates where the array ends.
withArray0 :: (Storable a, RegionBaseControl IO pr)
           => a
           -> [a]
           -> (forall sl. LocalPtr a (LocalRegion sl s)
              -> RegionT (Local s) pr b)
           -> RegionT s pr b
withArray0 marker vals = wrapAlloca (FMA.withArray0 marker vals)

-- | Like 'withArray', but the action gets the number of values as an additional
-- parameter.
withArrayLen
  :: (Storable a, RegionBaseControl IO pr)
  => [a]
  -> (forall sl. Int -> LocalPtr a (LocalRegion sl s)
     -> RegionT (Local s) pr b)
  -> RegionT s pr b
withArrayLen = wrapAlloca2 . FMA.withArrayLen

-- | Like 'withArrayLen', but a terminator indicates where the array ends.
withArrayLen0
  :: (Storable a, RegionBaseControl IO pr)
  => a
  -> [a]
  -> (forall sl. Int -> LocalPtr a (LocalRegion sl s)
     -> RegionT (Local s) pr b)
  -> RegionT s pr b
withArrayLen0 marker vals = wrapAlloca2 (FMA.withArrayLen0 marker vals)


--------------------------------------------------------------------------------
-- * Copying
--------------------------------------------------------------------------------

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may /not/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.copyArray'@.
copyArray :: ( AllocatedPointer pointer1
             , AllocatedPointer pointer2
             , Storable a
             , pr1 `AncestorRegion` cr
             , pr2 `AncestorRegion` cr
             , MonadBase IO cr
             )
          => pointer1 a pr1 -- ^ Destination
          -> pointer2 a pr2 -- ^ Source
          -> Int            -- ^ Number of /elements/ to copy.
          -> cr ()
copyArray pointer1 pointer2 = liftBase . FMA.copyArray (unsafePtr pointer1)
                                                       (unsafePtr pointer2)

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas /may/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.moveArray'@.
moveArray :: ( AllocatedPointer pointer1
             , AllocatedPointer pointer2
             , Storable a
             , pr1 `AncestorRegion` cr
             , pr2 `AncestorRegion` cr
             , MonadBase IO cr
             )
          => pointer1 a pr1 -- ^ Destination
          -> pointer2 a pr2 -- ^ Source
          -> Int            -- ^ Number of /elements/ to move.
          -> cr ()
moveArray pointer1 pointer2 = liftBase . FMA.moveArray (unsafePtr pointer1)
                                                       (unsafePtr pointer2)


--------------------------------------------------------------------------------
-- * Finding the length
--------------------------------------------------------------------------------

-- | Return the number of elements in an array, excluding the terminator.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.lengthArray0'@.
lengthArray0 :: ( AllocatedPointer pointer, Storable a, Eq a
                , pr `AncestorRegion` cr, MonadBase IO cr
                )
             => a -> pointer a pr -> cr Int
lengthArray0 = unsafeWrap2flp FMA.lengthArray0


--------------------------------------------------------------------------------
-- * Indexing
--------------------------------------------------------------------------------

-- | Advance a pointer into an array by the given number of elements.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.advancePtr'@.
advancePtr :: (AllocatedPointer pointer, Storable a)
           => pointer a pr -> Int -> pointer a pr
advancePtr pointer i = mapPointer (\ptr -> FMA.advancePtr ptr i) pointer
