{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Pool.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Pool.Region
    ( Pool(..)
    , newPool
    , withPool
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad                          ( liftM )
import qualified Foreign.Marshal.Pool as FMP

-- from base-unicode-symbols:
import Data.Function.Unicode                  ( (∘) )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO                  ( MonadCatchIO )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT
                                              , RegionalHandle
                                              , open, with
                                              )
import Control.Monad.Trans.Region.Unsafe      ( Resource
                                              , Handle
                                              , openResource
                                              , closeResource
                                              )

--------------------------------------------------------------------------------
-- Memory pools as scarce resources
--------------------------------------------------------------------------------

data Pool = Pool

instance Resource Pool where
    newtype Handle Pool = PoolHandle { pool ∷ FMP.Pool }

    openResource _ = liftM PoolHandle FMP.newPool
    closeResource  = FMP.freePool ∘ pool

type RegionalPoolHandle r = RegionalHandle Pool r

newPool ∷ MonadCatchIO pr
        ⇒ RegionT s pr (RegionalPoolHandle (RegionT s pr))
newPool = open Pool

withPool ∷ MonadCatchIO pr
         ⇒ (∀ s. RegionalPoolHandle (RegionT s pr) → RegionT s pr α)
         → pr α
withPool = with Pool


-- The End ---------------------------------------------------------------------
