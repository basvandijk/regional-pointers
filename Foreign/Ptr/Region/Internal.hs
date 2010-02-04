{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , TypeFamilies
           , CPP
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
      Memory(..)
    , RegionalPtr

      -- * Utility functions for lifting operations on Ptrs to RegionalPtrs
    , mapRegionalPtr
    , unsafePtr
    , unsafeWrap, unsafeWrap2, unsafeWrap3
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function                          ( ($) )
import Data.Int                               ( Int )
import Control.Monad                          ( liftM )
import System.IO                              ( IO )
import Foreign.Ptr                            ( Ptr )
import qualified Foreign.Marshal.Alloc as FMA ( mallocBytes, free )

-- from base-unicode-symbols:
import Data.Function.Unicode                  ( (∘) )

-- from transformers:
import Control.Monad.Trans                    ( MonadIO, liftIO )

-- from regions:
import Control.Resource                       ( Resource
                                              , Handle
                                              , openResource
                                              , closeResource
                                              )
import Control.Monad.Trans.Region             ( RegionalHandle )
import Control.Monad.Trans.Region.Unsafe      ( internalHandle
                                              , mapInternalHandle
                                              )
#ifdef __HADDOCK__
import Control.Monad.Trans.Region ( open )
#endif


--------------------------------------------------------------------------------
-- Memory as a scarce resource
--------------------------------------------------------------------------------

{-| Represents memory of 'size' number of bytes which may be marshalled to or
from Haskell values of type @&#945;@. Before you can use the memory you have to
allocate it using 'open'.
-}
newtype Memory α = Memory { size ∷ Int }

instance Resource (Memory α) where
    newtype Handle (Memory α) = Pointer { ptr ∷ Ptr α }

    openResource  = liftM Pointer ∘ FMA.mallocBytes ∘ size
    closeResource = FMA.free ∘ ptr

-- | Handy type synonym for a regional handle to memory. This should provide a
-- safer replacement for @Foreign.Ptr.@'Ptr'
type RegionalPtr α r = RegionalHandle (Memory α) r


--------------------------------------------------------------------------------
-- Utility functions for lifting operations on Ptrs to RegionalPtrs
--------------------------------------------------------------------------------

mapRegionalPtr ∷ (Ptr α → Ptr β) → (RegionalPtr α r → RegionalPtr β r)
mapRegionalPtr f = mapInternalHandle $ Pointer ∘ f ∘ ptr

unsafePtr ∷ RegionalPtr α r → Ptr α
unsafePtr = ptr ∘ internalHandle

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
