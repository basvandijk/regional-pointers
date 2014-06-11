{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Storable.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
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
import Control.Monad.IO.Class           ( MonadIO )

-- from regions:
import Control.Monad.Trans.Region       ( AncestorRegion )

-- from ourselves:
import Foreign.Ptr.Region               ( AllocatedPointer )
import Foreign.Ptr.Region.Unsafe        ( unsafeWrap, unsafeWrap2, unsafeWrap3 )


--------------------------------------------------------------------------------
-- Storable methods lifted to a region
--------------------------------------------------------------------------------

-- | Read a value from a memory area regarded as an array of values of the same
-- kind. The first argument specifies the start address of the array and the
-- second the index into the array (the first element of the array has index
-- @0@). The following equality holds,
--
-- > peekElemOff addr idx = IOExts.fixIO $ \result ->
-- >   peek (addr `plusPtr` (idx * sizeOf result))
--
-- Note that this is only a specification, not necessarily the concrete
-- implementation of the function.
--
-- Wraps: @Foreign.Storable.'FS.peekElemOff'@.
peekElemOff ∷ ( AllocatedPointer pointer, Storable α
              , pr `AncestorRegion` cr, MonadIO cr
              )
            ⇒ pointer α pr → Int → cr α
peekElemOff = unsafeWrap2 FS.peekElemOff

-- | Write a value to a memory area regarded as an array of values of the same
-- kind.  The following equality holds:
--
-- > pokeElemOff addr idx x =
-- >   poke (addr `plusPtr` (idx * sizeOf x)) x
--
-- Wraps: @Foreign.Storable.'FS.pokeElemOff'@.
pokeElemOff ∷ ( AllocatedPointer pointer, Storable α
              , pr `AncestorRegion` cr, MonadIO cr
              )
            ⇒ pointer α pr → Int → α → cr ()
pokeElemOff = unsafeWrap3 FS.pokeElemOff

-- | Read a value from a memory location given by a base address and offset.
-- The following equality holds:
--
-- > peekByteOff addr off = peek (addr `plusPtr` off)
--
-- Wraps: @Foreign.Storable.'FS.peekByteOff'@.
peekByteOff ∷ ( AllocatedPointer pointer, Storable α
              , pr `AncestorRegion` cr, MonadIO cr
              )
            ⇒ pointer β pr → Int → cr α
peekByteOff = unsafeWrap2 FS.peekByteOff

-- | Write a value to a memory location given by a base address and offset.  The
-- following equality holds:
--
-- > pokeByteOff addr off x = poke (addr `plusPtr` off) x
--
-- Wraps: @Foreign.Storable.'FS.pokeByteOff'@.
pokeByteOff ∷ ( AllocatedPointer pointer, Storable α
              , pr `AncestorRegion` cr, MonadIO cr
              )
            ⇒ pointer β pr → Int → α → cr ()
pokeByteOff = unsafeWrap3 FS.pokeByteOff

-- | Read a value from the given memory location.
--
-- Note that the peek and poke functions might require properly aligned
-- addresses to function correctly.  This is architecture dependent; thus,
-- portable code should ensure that when peeking or poking values of some type
-- @a@, the alignment constraint for @a@, as given by the function 'alignment'
-- is fulfilled.
--
-- Wraps: @Foreign.Storable.'FS.peek'@.
peek ∷ ( AllocatedPointer pointer, Storable α
       , AncestorRegion pr cr, MonadIO cr
       )
     ⇒ pointer α pr → cr α
peek = unsafeWrap FS.peek

-- | Write the given value to the given memory location.  Alignment restrictions
-- might apply; see 'peek'.
--
-- Wraps: @Foreign.Storable.'FS.poke'@.
poke ∷ ( AllocatedPointer pointer, Storable α
       , AncestorRegion pr cr, MonadIO cr
       )
     ⇒ pointer α pr → α → cr ()
poke = unsafeWrap2 FS.poke


-- The End ---------------------------------------------------------------------
