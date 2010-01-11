{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Storable.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Storable.Region
    ( peekElemOff, pokeElemOff
    , peekByteOff, pokeByteOff
    , peek,        poke
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Int                         ( Int )
import Foreign.Storable                 ( Storable )
import qualified Foreign.Storable as FS ( peekElemOff, pokeElemOff
                                        , peekByteOff, pokeByteOff
                                        , peek,        poke
                                        )
-- from transformers:
import Control.Monad.Trans              ( MonadIO )

-- from regions:
import Control.Monad.Trans.Region       ( ParentOf )

-- from ourselves:
import Foreign.Ptr.Region               ( RegionalPtr )
import Foreign.Ptr.Region.Unsafe        ( wrap, wrap2, wrap3 )


--------------------------------------------------------------------------------
-- Storable methods lifted to a region
--------------------------------------------------------------------------------

peekElemOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr α pr → Int → cr α
peekElemOff = wrap2 FS.peekElemOff

pokeElemOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr α pr → Int → α → cr ()
pokeElemOff = wrap3 FS.pokeElemOff

peekByteOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr β pr → Int → cr α
peekByteOff = wrap2 FS.peekByteOff

pokeByteOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr β pr → Int → α → cr ()
pokeByteOff = wrap3 FS.pokeByteOff

peek ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
     ⇒ RegionalPtr α pr → cr α
peek = wrap FS.peek

poke ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
     ⇒ RegionalPtr α pr → α → cr ()
poke = wrap2 FS.poke


-- The End ---------------------------------------------------------------------
