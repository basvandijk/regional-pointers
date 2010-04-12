{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , TypeFamilies
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.StablePtr.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.StablePtr.Region
       ( Stable(..)
       , RegionalStablePtr
       , newStablePtr
       , deRefStablePtr
       , castStablePtrToPtr
       -- TODO: , castPtrToStablePtr
       ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                     ( liftM )
import Foreign.Ptr                       ( Ptr )
import qualified Foreign.StablePtr as FS ( StablePtr
                                         , newStablePtr
                                         , freeStablePtr
                                         , deRefStablePtr
                                         , castStablePtrToPtr
                                         , -- TODO: castPtrToStablePtr
                                         )

-- from base-unicode-symbols:
import Data.Function.Unicode             ( (∘) )

-- from transformers:
import Control.Monad.IO.Class            ( MonadIO, liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO             ( MonadCatchIO )

-- from regions:
import Control.Resource                  ( Resource
                                         , Handle
                                         , openResource
                                         , closeResource
                                         )
import Control.Monad.Trans.Region        ( RegionT
                                         , RegionalHandle
                                         , open
                                         , ParentOf
                                         )
import Control.Monad.Trans.Region.Unsafe ( internalHandle )


--------------------------------------------------------------------------------
-- Stable pointers as scarce resources
--------------------------------------------------------------------------------

data Stable α = Stable { unStable ∷ α }

instance Resource (Stable α) where
    newtype Handle (Stable α) = StablePointer
        { stablePtr ∷ FS.StablePtr α }

    openResource  = liftM StablePointer ∘ FS.newStablePtr ∘ unStable
    closeResource = FS.freeStablePtr ∘ stablePtr

type RegionalStablePtr α r = RegionalHandle (Stable α) r

regularStablePtr ∷ RegionalStablePtr α r → FS.StablePtr α
regularStablePtr = stablePtr ∘ internalHandle

newStablePtr ∷ MonadCatchIO pr
             ⇒ α
             → RegionT s pr (RegionalStablePtr α (RegionT s pr))
newStablePtr = open ∘ Stable

deRefStablePtr ∷ (pr `ParentOf` cr, MonadIO cr)
               ⇒ RegionalStablePtr α pr → cr α
deRefStablePtr = liftIO ∘ FS.deRefStablePtr ∘ regularStablePtr

castStablePtrToPtr ∷ RegionalStablePtr α r → Ptr ()
castStablePtrToPtr = FS.castStablePtrToPtr ∘ regularStablePtr

-- TODO:
-- castPtrToStablePtr ∷ Ptr () → RegionalStablePtr α r
-- castPtrToStablePtr = ...


-- The End ---------------------------------------------------------------------
