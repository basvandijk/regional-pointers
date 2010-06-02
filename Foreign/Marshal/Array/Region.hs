{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
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
import Prelude                                ( undefined, (*), succ )
import Data.Function                          ( ($), flip, const )
import Data.Int                               ( Int )
import Data.List                              ( length )
import Data.Eq                                ( Eq )
import Control.Monad                          ( return, (>>=), fail
                                              , (>>)
                                              )
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


--------------------------------------------------------------------------------
-- Allocation
--------------------------------------------------------------------------------

mallocArray ∷ ∀ α s pr. (Storable α, MonadCatchIO pr)
            ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray size = mallocBytes $ size * sizeOf (undefined ∷ α)

mallocArray0 ∷ (Storable α, MonadCatchIO pr)
             ⇒ Int → RegionT s pr (RegionalPtr α (RegionT s pr))
mallocArray0 = mallocArray ∘ succ

allocaArray ∷ ∀ α pr β. (Storable α, MonadCatchIO pr)
            ⇒ Int
            → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
            → pr β
allocaArray size = allocaBytes $ size * sizeOf (undefined ∷ α)

allocaArray0 ∷ ∀ α pr β. (Storable α, MonadCatchIO pr)
             ⇒ Int
             → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
             → pr β
allocaArray0 = allocaArray ∘ succ

-- TODO:
-- reallocArray ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)
-- reallocArray0 ∷ Storable α ⇒ RegionalPtr α pr → Int → cr (RegionalPtr α pr)


--------------------------------------------------------------------------------
-- Marshalling
--------------------------------------------------------------------------------

unsafeWrap2flp ∷ MonadIO m
               ⇒ (γ → Ptr α → IO β)
               → (γ → RegionalPtr α r → m β)
unsafeWrap2flp = flip ∘ unsafeWrap2 ∘ flip

-- | Wraps: @Foreign.Marshal.Array.@'FMA.peekArray'.
peekArray ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
          ⇒ Int → RegionalPtr α pr → cr [α]
peekArray =  unsafeWrap2flp FMA.peekArray

-- | Wraps: @Foreign.Marshal.Array.@'FMA.peekArray0'.
peekArray0 ∷ (Storable α, Eq α, pr `ParentOf` cr, MonadIO cr)
           ⇒ α → RegionalPtr α pr → cr [α]
peekArray0 = unsafeWrap2flp FMA.peekArray0

-- | Wraps: @Foreign.Marshal.Array.@'FMA.pokeArray'.
pokeArray ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
          ⇒ RegionalPtr α pr → [α] → cr ()
pokeArray = unsafeWrap2 FMA.pokeArray

-- | Wraps: @Foreign.Marshal.Array.@'FMA.pokeArray0'.
pokeArray0 ∷ (Storable α, pr `ParentOf` cr, MonadIO cr)
           ⇒ α → RegionalPtr α pr → [α] → cr ()
pokeArray0 m rp xs = liftIO $ FMA.pokeArray0 m (unsafePtr rp) xs


--------------------------------------------------------------------------------
-- Combined allocation and marshalling
--------------------------------------------------------------------------------

newArray ∷ (Storable α, MonadCatchIO pr)
         ⇒ [α] → RegionT s pr (RegionalPtr α (RegionT s pr ))
newArray vals  = do
  ptr ← mallocArray $ length vals
  pokeArray ptr vals
  return ptr

newArray0 ∷ (Storable α, MonadCatchIO pr)
          ⇒ α → [α] → RegionT s pr (RegionalPtr α (RegionT s pr))
newArray0 marker vals  = do
  ptr ← mallocArray0 $ length vals
  pokeArray0 marker ptr vals
  return ptr

withArray ∷ (Storable α, MonadCatchIO pr)
          ⇒ [α]
          → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
          → pr β
withArray vals = withArrayLen vals ∘ const

withArray0 ∷ (Storable α, MonadCatchIO pr)
           ⇒ α
           → [α]
           → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β)
           → pr β
withArray0 marker vals = withArrayLen0 marker vals ∘ const

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
-- Copying
--------------------------------------------------------------------------------

-- | Wraps: @Foreign.Marshal.Array.@'FMA.copyArray'.
copyArray ∷ (Storable α, pr1 `ParentOf` cr, pr2 `ParentOf` cr, MonadIO cr)
          ⇒ RegionalPtr α pr1 → RegionalPtr α pr2 → Int → cr ()
copyArray rp1 rp2 = liftIO ∘ FMA.copyArray (unsafePtr rp1) (unsafePtr rp2)

-- | Wraps: @Foreign.Marshal.Array.@'FMA.moveArray'.
moveArray ∷ (Storable α, pr1 `ParentOf` cr, pr2 `ParentOf` cr, MonadIO cr)
          ⇒ RegionalPtr α pr1 → RegionalPtr α pr1 → Int → cr ()
moveArray rp1 rp2 = liftIO ∘ FMA.moveArray (unsafePtr rp1) (unsafePtr rp2)


--------------------------------------------------------------------------------
-- Finding the length
--------------------------------------------------------------------------------

-- | Wraps: @Foreign.Marshal.Array.@'FMA.lengthArray0'.
lengthArray0 ∷ (Storable α, Eq α, pr `ParentOf` cr, MonadIO cr)
             ⇒ α → RegionalPtr α pr → cr Int
lengthArray0 = unsafeWrap2flp FMA.lengthArray0


--------------------------------------------------------------------------------
-- Indexing
--------------------------------------------------------------------------------

-- | Wraps: @Foreign.Marshal.Array.@'FMA.advancePtr'.
advancePtr ∷ Storable α ⇒ RegionalPtr α pr → Int → RegionalPtr α pr
advancePtr rp i = mapRegionalPtr (\p → FMA.advancePtr p i) rp


-- The End ---------------------------------------------------------------------
