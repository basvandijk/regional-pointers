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
    ( -- * Memory as a scarce resource
      RegionalPtr(RegionalPtr)

      -- * Utility functions for lifting operations on Ptrs to RegionalPtrs
    , unsafePtr
    , unsafeWrap, unsafeWrap2, unsafeWrap3
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad ( liftM )
import Data.Function ( ($) )
import System.IO     ( IO )
import Foreign.Ptr   ( Ptr )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region.Close ( CloseHandle )
import Control.Monad.Trans.Region       ( Dup(dup) )

--------------------------------------------------------------------------------
-- Memory as a scarce resource
--------------------------------------------------------------------------------

-- | A regional handle to memory. This should provide a safer replacement for
-- @Foreign.Ptr.'Ptr'@
data RegionalPtr α (r ∷ * → *) = RegionalPtr (Ptr α) (CloseHandle r)

instance Dup (RegionalPtr α) where
    dup (RegionalPtr ptr ch) = liftM (RegionalPtr ptr) $ dup ch


--------------------------------------------------------------------------------
-- Utility functions for lifting operations on Ptrs to RegionalPtrs
--------------------------------------------------------------------------------

unsafePtr ∷ RegionalPtr α r → Ptr α
unsafePtr (RegionalPtr ptr _) = ptr

unsafeWrap ∷ MonadIO m
           ⇒ (Ptr α → IO β)
           → (RegionalPtr α r → m β)
unsafeWrap f rp = liftIO $ f $ unsafePtr rp

unsafeWrap2 ∷ MonadIO m
            ⇒ (Ptr α → γ → IO β)
            → (RegionalPtr α r → γ → m β)
unsafeWrap2 f rp x = liftIO $ f (unsafePtr rp) x

unsafeWrap3 ∷ MonadIO m
            ⇒ (Ptr α → γ → δ → IO β)
            → (RegionalPtr α r → γ → δ → m β)
unsafeWrap3 f rp x y = liftIO $ f (unsafePtr rp) x y


-- The End ---------------------------------------------------------------------
