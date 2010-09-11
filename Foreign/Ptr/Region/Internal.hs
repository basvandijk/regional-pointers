{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region.Internal
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region.Internal
    ( -- * Regional pointers
      RegionalPtr(RegionalPtr)

      -- * Unsafely constructing regional pointers
    , unsafeRegionalPtr
    , unsafePureRegionalPtr

      -- * Unsafe utility functions for lifting operations on @Ptrs@ to @RegionalPtrs@
    , unsafePtr
    , unsafeWrap, unsafeWrap2, unsafeWrap3
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( return, (>>=), fail )
import Data.Function ( ($) )
import Data.Maybe    ( Maybe(Nothing, Just) )
import System.IO     ( IO )
import Foreign.Ptr   ( Ptr )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region.OnExit ( CloseHandle, CloseAction, onExit )
import Control.Monad.Trans.Region        ( RegionT, Dup(dup) )


--------------------------------------------------------------------------------
-- * Regional pointers
--------------------------------------------------------------------------------

-- | A regional handle to memory. This should provide a safer replacement for
-- @Foreign.Ptr.'Ptr'@
data RegionalPtr α (r ∷ * → *) = RegionalPtr !(Ptr α) !(Maybe (CloseHandle r))

instance Dup (RegionalPtr α) where
    dup (RegionalPtr ptr Nothing)   = return $ RegionalPtr ptr Nothing
    dup (RegionalPtr ptr (Just ch)) = do ch' ← dup ch
                                         return $ RegionalPtr ptr $ Just ch'

--------------------------------------------------------------------------------
-- * Constructing regional pointers
--------------------------------------------------------------------------------

-- | Construct a regional pointer from a native pointer
-- and an @IO@ computation that finalizes the pointer (like @free ptr@)
-- which is executed when the region exits.
--
-- This function is considered unsafe because this library can't guarantee that
-- the finalizer will actually finalize the pointer (suppose having @return ()@
-- as the finalizer). You have to verify the correct finalisation yourself.
unsafeRegionalPtr ∷ MonadIO pr
                  ⇒ Ptr α
                  → CloseAction
                  → RegionT s pr (RegionalPtr α (RegionT s pr))
unsafeRegionalPtr ptr finalize = do ch ← onExit finalize
                                    return $ RegionalPtr ptr $ Just ch

-- | Construct a regional pointer from a native pointer
-- without registering a finalizer like @free ptr@.
--
-- This function is considered unsafe because this library can't guarantee the
-- finalisation of the pointer, you have to do that yourself.
unsafePureRegionalPtr ∷ Ptr α → RegionalPtr α r
unsafePureRegionalPtr ptr = RegionalPtr ptr Nothing


--------------------------------------------------------------------------------
-- * Utility functions for lifting operations on Ptrs to RegionalPtrs
--------------------------------------------------------------------------------

unsafePtr ∷ RegionalPtr α r → Ptr α
unsafePtr (RegionalPtr ptr _) = ptr

unsafeWrap ∷ MonadIO m
           ⇒ (Ptr α → IO β)
           → (RegionalPtr α r → m β)
unsafeWrap f rPtr = liftIO $ f (unsafePtr rPtr)

unsafeWrap2 ∷ MonadIO m
            ⇒ (Ptr α → γ → IO β)
            → (RegionalPtr α r → γ → m β)
unsafeWrap2 f rPtr x = liftIO $ f (unsafePtr rPtr) x

unsafeWrap3 ∷ MonadIO m
            ⇒ (Ptr α → γ → δ → IO β)
            → (RegionalPtr α r → γ → δ → m β)
unsafeWrap3 f rPtr x y = liftIO $ f (unsafePtr rPtr) x y


-- The End ---------------------------------------------------------------------
