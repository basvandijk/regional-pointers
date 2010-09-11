{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , CPP
           , RankNTypes
           , ScopedTypeVariables
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Array.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Array.Region
    ( -- * Allocation
      mallocArray
    , mallocArray0
    , allocaArray
    , allocaArray0

    -- | /TODO:/ Define and export @reallocArray@ and @reallocArray0@

      -- * Marshalling
    , peekArray
    , peekArray0
    , pokeArray
    , pokeArray0

      -- * Combined allocation and marshalling
    , newArray
    , newArray0
    , withArray
    , withArray0
    , withArrayLen
    , withArrayLen0

      -- * Copying
    , copyArray
    , moveArray

      -- * Finding the length
    , lengthArray0

      -- * Indexing
    , advancePtr
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude                                ( (*), succ )
import Data.Function                          ( ($), flip, const )
import Data.Int                               ( Int )
import Data.List                              ( length )
import Data.Eq                                ( Eq )
import Control.Monad                          ( return, (>>=), fail, (>>) )
import System.IO                              ( IO )
import Foreign.Ptr                            ( Ptr )
import Foreign.Storable                       ( Storable, sizeOf )
import qualified Foreign.Marshal.Array as FMA ( peekArray
                                              , peekArray0
                                              , pokeArray
                                              , pokeArray0
                                              , copyArray
                                              , moveArray
                                              , lengthArray0
                                              , advancePtr
                                              )
-- from base-unicode-symbols:
import Data.Function.Unicode                  ( (∘) )
import Prelude.Unicode                        ( (⊥) )

-- from transformers:
import Control.Monad.IO.Class                 ( MonadIO, liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO                  ( MonadCatchIO )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT , ParentOf )

-- from ourselves:
import Foreign.Ptr.Region                     ( RegionalPtr, mapRegionalPtr )
import Foreign.Ptr.Region.Unsafe              ( unsafePtr, unsafeWrap2 )
import Foreign.Marshal.Alloc.Region           ( mallocBytes, allocaBytes )

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
mallocArray ∷ ∀ α s pr. (Storable α, MonadCatchIO pr)
            ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray size = mallocBytes $ size * sizeOf ((⊥) ∷ α)

-- | Like 'mallocArray', but add an extra position to hold a special termination
-- element.
mallocArray0 ∷ (Storable α, MonadCatchIO pr)
             ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray0 = mallocArray ∘ succ

-- | Temporarily allocate space for the given number of elements (like 'alloca',
-- but for multiple elements).
allocaArray ∷ ∀ α pr β. (Storable α, MonadCatchIO pr)
            ⇒ Int
            → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
            → pr β
allocaArray size = allocaBytes $ size * sizeOf ((⊥) ∷ α)

-- | Like 'allocaArray', but add an extra position to hold a special termination
-- element.
allocaArray0 ∷ ∀ α pr β. (Storable α, MonadCatchIO pr)
             ⇒ Int
             → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
             → pr β
allocaArray0 = allocaArray ∘ succ

-- TODO:
-- reallocArray  ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)
-- reallocArray0 ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)


--------------------------------------------------------------------------------
-- * Marshalling
--------------------------------------------------------------------------------

unsafeWrap2flp ∷ MonadIO m
               ⇒ (γ → Ptr α → IO β)
               → (γ → RegionalPtr α r → m β)
unsafeWrap2flp = flip ∘ unsafeWrap2 ∘ flip

-- | Convert an array of given length into a Haskell list.
--
-- (This version traverses the array backwards using an accumulating parameter,
-- which uses constant stack space. The previous version using @mapM@ needed
-- linear stack space.)
--
-- Wraps: @Foreign.Marshal.Array.'FMA.peekArray'@.
peekArray ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
          ⇒ Int → RegionalPtr α pr → cr [α]
peekArray =  unsafeWrap2flp FMA.peekArray

-- | Convert an array terminated by the given end marker into a Haskell list.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.peekArray0'@.
peekArray0 ∷ (Storable α, Eq α, pr `ParentOf` cr, MonadIO cr)
           ⇒ α → RegionalPtr α pr → cr [α]
peekArray0 = unsafeWrap2flp FMA.peekArray0

-- | Write the list elements consecutive into memory.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray'@.
pokeArray ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
          ⇒ RegionalPtr α pr → [α] → cr ()
pokeArray = unsafeWrap2 FMA.pokeArray

-- | Write the list elements consecutive into memory and terminate them with the
-- given marker element.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.pokeArray0'@.
pokeArray0 ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
           ⇒ α → RegionalPtr α pr → [α] → cr ()
pokeArray0 m rp xs = liftIO $ FMA.pokeArray0 m (unsafePtr rp) xs


--------------------------------------------------------------------------------
-- * Combined allocation and marshalling
--------------------------------------------------------------------------------

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values.
--
-- Like 'new', but for multiple elements.
newArray ∷ (Storable α, MonadCatchIO pr)
         ⇒ [α] → RegionT s pr (RegionalPtr α (RegionT s pr ))
newArray vals  = do
  ptr ← mallocArray $ length vals
  pokeArray ptr vals
  return ptr

-- | Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end marker.
newArray0 ∷ (Storable α, MonadCatchIO pr)
          ⇒ α → [α] → RegionT s pr (RegionalPtr α (RegionT s pr))
newArray0 marker vals  = do
  ptr ← mallocArray0 $ length vals
  pokeArray0 marker ptr vals
  return ptr

-- | Temporarily store a list of storable values in memory.
--
-- Like 'with', but for multiple elements.
withArray ∷ (Storable α, MonadCatchIO pr)
          ⇒ [α]
          → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
          → pr β
withArray vals = withArrayLen vals ∘ const

-- | Like 'withArray', but a terminator indicates where the array ends.
withArray0 ∷ (Storable α, MonadCatchIO pr)
           ⇒ α
           → [α]
           → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
           → pr β
withArray0 marker vals = withArrayLen0 marker vals ∘ const

-- | Like 'withArray', but the action gets the number of values as an additional
-- parameter.
withArrayLen ∷ (Storable α, MonadCatchIO pr)
            ⇒ [α]
            → (∀ s. Int → RegionalPtr α (RegionT s pr) → RegionT s pr β)
            → pr β
withArrayLen vals f =
  allocaArray len $ \ptr → do
    pokeArray ptr vals
    res ← f len ptr
    return res
  where
    len = length vals

-- | Like 'withArrayLen', but a terminator indicates where the array ends.
withArrayLen0 ∷ (Storable α, MonadCatchIO pr)
              ⇒ α
              → [α]
              → (∀ s. Int → RegionalPtr α (RegionT s pr) → RegionT s pr β)
              → pr β
withArrayLen0 marker vals f =
  allocaArray0 len $ \ptr → do
    pokeArray0 marker ptr vals
    res ← f len ptr
    return res
  where
    len = length vals


--------------------------------------------------------------------------------
-- * Copying
--------------------------------------------------------------------------------

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may /not/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.copyArray'@.
copyArray ∷ ( Storable α
            , pr1 `ParentOf` cr
            , pr2 `ParentOf` cr
            , MonadIO cr
            )
          ⇒ RegionalPtr α pr1 -- ^ Destination
          → RegionalPtr α pr2 -- ^ Source
          → Int               -- ^ Number of /elements/ to copy.
          → cr ()
copyArray rPtr1 rPtr2 = liftIO ∘ FMA.copyArray (unsafePtr rPtr1) (unsafePtr rPtr2)

-- | Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas /may/ overlap.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.moveArray'@.
moveArray ∷ ( Storable α
            , pr1 `ParentOf` cr
            , pr2 `ParentOf` cr
            , MonadIO cr
            )
          ⇒ RegionalPtr α pr1 -- ^ Destination
          → RegionalPtr α pr1 -- ^ Source
          → Int               -- ^ Number of /elements/ to move.
          → cr ()
moveArray rPtr1 rPtr2 = liftIO ∘ FMA.moveArray (unsafePtr rPtr1) (unsafePtr rPtr2)


--------------------------------------------------------------------------------
-- * Finding the length
--------------------------------------------------------------------------------

-- | Return the number of elements in an array, excluding the terminator.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.lengthArray0'@.
lengthArray0 ∷ (Storable α, Eq α, pr `ParentOf` cr, MonadIO cr)
             ⇒ α → RegionalPtr α pr → cr Int
lengthArray0 = unsafeWrap2flp FMA.lengthArray0


--------------------------------------------------------------------------------
-- * Indexing
--------------------------------------------------------------------------------

-- | Advance a pointer into an array by the given number of elements.
--
-- Wraps: @Foreign.Marshal.Array.'FMA.advancePtr'@.
advancePtr ∷ Storable α ⇒ RegionalPtr α pr → Int → RegionalPtr α pr
advancePtr rp i = mapRegionalPtr (\p → FMA.advancePtr p i) rp


-- The End ---------------------------------------------------------------------
