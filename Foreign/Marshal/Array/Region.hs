{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , CPP
           , RankNTypes
  #-}

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

-- from base-unicode-symbols:
import Data.Function.Unicode                  ( (∘) )

-- from transformers:
import Control.Monad.IO.Class                 ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT
                                              , RegionControlIO
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
mallocArray ∷ (Storable α, RegionControlIO pr)
            ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray = wrapMalloc ∘ FMA.mallocArray

-- | Like 'mallocArray', but add an extra position to hold a special termination
-- element.
mallocArray0 ∷ (Storable α, RegionControlIO pr)
             ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray0 = wrapMalloc ∘ FMA.mallocArray0

-- | Temporarily allocate space for the given number of elements (like 'alloca',
-- but for multiple elements).
allocaArray ∷ (Storable α, RegionControlIO pr)
            ⇒ Int
            → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
            → RegionT s pr β
allocaArray = wrapAlloca ∘ FMA.allocaArray

-- | Like 'allocaArray', but add an extra position to hold a special termination
-- element.
allocaArray0 ∷ (Storable α, RegionControlIO pr)
             ⇒ Int
             → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
             → RegionT s pr β
allocaArray0 = wrapAlloca ∘ FMA.allocaArray0

-- TODO:
-- reallocArray  ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)
-- reallocArray0 ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)


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
peekArray ∷ ( AllocatedPointer pointer, Storable α
            , pr `AncestorRegion` cr, MonadIO cr
            )
          ⇒ Int → pointer α pr → cr [α]
peekArray =  unsafeWrap2flp FMA.peekArray

-- | Convert an array terminated by the given end marker into a Haskell list.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.peekArray0'@.
peekArray0 ∷ ( AllocatedPointer pointer, Storable α, Eq α
             , pr `AncestorRegion` cr, MonadIO cr
             )
           ⇒ α → pointer α pr → cr [α]
peekArray0 = unsafeWrap2flp FMA.peekArray0

-- | Write the list elements consecutive into memory.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray'@.
pokeArray ∷ ( AllocatedPointer pointer, Storable α
            , pr `AncestorRegion` cr, MonadIO cr
            )
          ⇒ pointer α pr → [α] → cr ()
pokeArray = unsafeWrap2 FMA.pokeArray

-- | Write the list elements consecutive into memory and terminate them with the
-- given marker element.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray0'@.
pokeArray0 ∷ ( AllocatedPointer pointer, Storable α
             , pr `AncestorRegion` cr, MonadIO cr
             )
           ⇒ α → pointer α pr → [α] → cr ()
pokeArray0 m rp xs = liftIO $ FMA.pokeArray0 m (unsafePtr rp) xs


--------------------------------------------------------------------------------
-- * Combined allocation and marshalling
--------------------------------------------------------------------------------

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values.
--
-- Like 'new', but for multiple elements.
newArray ∷ (Storable α, RegionControlIO pr)
         ⇒ [α] → RegionT s pr (RegionalPtr α (RegionT s pr ))
newArray = wrapMalloc ∘ FMA.newArray

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end marker.
newArray0 ∷ (Storable α, RegionControlIO pr)
          ⇒ α → [α] → RegionT s pr (RegionalPtr α (RegionT s pr))
newArray0 marker vals = wrapMalloc (FMA.newArray0 marker vals)

-- | Temporarily store a list of storable values in memory.
--
-- Like 'with', but for multiple elements.
withArray ∷ (Storable α, RegionControlIO pr)
          ⇒ [α]
          → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
          → RegionT s pr β
withArray = wrapAlloca ∘ FMA.withArray

-- | Like 'withArray', but a terminator indicates where the array ends.
withArray0 ∷ (Storable α, RegionControlIO pr)
           ⇒ α
           → [α]
           → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
           → RegionT s pr β
withArray0 marker vals = wrapAlloca (FMA.withArray0 marker vals)

-- | Like 'withArray', but the action gets the number of values as an additional
-- parameter.
withArrayLen ∷
    (Storable α, RegionControlIO pr)
  ⇒ [α]
  → (∀ sl. Int → LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
  → RegionT s pr β
withArrayLen = wrapAlloca2 ∘ FMA.withArrayLen

-- | Like 'withArrayLen', but a terminator indicates where the array ends.
withArrayLen0 ∷
    (Storable α, RegionControlIO pr)
  ⇒ α
  → [α]
  → (∀ sl. Int → LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
  → RegionT s pr β
withArrayLen0 marker vals = wrapAlloca2 (FMA.withArrayLen0 marker vals)


--------------------------------------------------------------------------------
-- * Copying
--------------------------------------------------------------------------------

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may /not/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.copyArray'@.
copyArray ∷ ( AllocatedPointer pointer1
            , AllocatedPointer pointer2
            , Storable α
            , pr1 `AncestorRegion` cr
            , pr2 `AncestorRegion` cr
            , MonadIO cr
            )
          ⇒ pointer1 α pr1 -- ^ Destination
          → pointer2 α pr2 -- ^ Source
          → Int            -- ^ Number of /elements/ to copy.
          → cr ()
copyArray pointer1 pointer2 = liftIO ∘ FMA.copyArray (unsafePtr pointer1)
                                                     (unsafePtr pointer2)

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas /may/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.moveArray'@.
moveArray ∷ ( AllocatedPointer pointer1
            , AllocatedPointer pointer2
            , Storable α
            , pr1 `AncestorRegion` cr
            , pr2 `AncestorRegion` cr
            , MonadIO cr
            )
          ⇒ pointer1 α pr1 -- ^ Destination
          → pointer2 α pr2 -- ^ Source
          → Int            -- ^ Number of /elements/ to move.
          → cr ()
moveArray pointer1 pointer2 = liftIO ∘ FMA.moveArray (unsafePtr pointer1)
                                                     (unsafePtr pointer2)


--------------------------------------------------------------------------------
-- * Finding the length
--------------------------------------------------------------------------------

-- | Return the number of elements in an array, excluding the terminator.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.lengthArray0'@.
lengthArray0 ∷ ( AllocatedPointer pointer, Storable α, Eq α
               , pr `AncestorRegion` cr, MonadIO cr
               )
             ⇒ α → pointer α pr → cr Int
lengthArray0 = unsafeWrap2flp FMA.lengthArray0


--------------------------------------------------------------------------------
-- * Indexing
--------------------------------------------------------------------------------

-- | Advance a pointer into an array by the given number of elements.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.advancePtr'@.
advancePtr ∷ (AllocatedPointer pointer, Storable α)
           ⇒ pointer α pr → Int → pointer α pr
advancePtr pointer i = mapPointer (\ptr → FMA.advancePtr ptr i) pointer


-- The End ---------------------------------------------------------------------
