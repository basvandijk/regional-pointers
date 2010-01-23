{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Storable.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Lifts methods of the 'Storable' type class from @Foreign.Storable@ to
-- regional pointers.
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
import Foreign.Ptr.Region.Unsafe        ( unsafeWrap, unsafeWrap2, unsafeWrap3 )


--------------------------------------------------------------------------------
-- Storable methods lifted to a region
--------------------------------------------------------------------------------

-- | Wraps: @Foreign.Storable.'FS.peekElemOff'@.
peekElemOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr α pr → Int → cr α
peekElemOff = unsafeWrap2 FS.peekElemOff

-- | Wraps: @Foreign.Storable.'FS.pokeElemOff'@.
pokeElemOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr α pr → Int → α → cr ()
pokeElemOff = unsafeWrap3 FS.pokeElemOff

-- | Wraps: @Foreign.Storable.'FS.peekByteOff'@.
peekByteOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr β pr → Int → cr α
peekByteOff = unsafeWrap2 FS.peekByteOff

-- | Wraps: @Foreign.Storable.'FS.pokeByteOff'@.
pokeByteOff ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
            ⇒ RegionalPtr β pr → Int → α → cr ()
pokeByteOff = unsafeWrap3 FS.pokeByteOff

-- | Wraps: @Foreign.Storable.'FS.peek'@.
peek ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
     ⇒ RegionalPtr α pr → cr α
peek = unsafeWrap FS.peek

-- | Wraps: @Foreign.Storable.'FS.poke'@.
poke ∷ (pr `ParentOf` cr, Storable α, MonadIO cr)
     ⇒ RegionalPtr α pr → α → cr ()
poke = unsafeWrap2 FS.poke


-- The End ---------------------------------------------------------------------
