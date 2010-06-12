{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , CPP
           , RankNTypes
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Utils.Region
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Marshal.Utils.Region
       ( -- * General marshalling utilities
         -- ** Combined allocation and marshalling
         with
       , new

         -- * Marshalling of Boolean values (non-zero corresponds to 'True')
       , FMU.fromBool
       , FMU.toBool

         -- ** Marshalling of Maybe values
         -- | /TODO:/ Define and export: @maybeNew@, @maybeWith@ and @maybePeek@.

         -- ** Marshalling lists of storable objects
         -- | /TODO:/ Define and export: @withMany@.

         -- ** Haskellish interface to memcpy and memmove
         -- | (argument order: destination, source)
       , copyBytes
       , moveBytes
       ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function                          ( ($) )
import Data.Int                               ( Int )
import Control.Monad                          ( return, (>>=), fail, (>>) )
import qualified Foreign.Marshal.Utils as FMU ( fromBool,  toBool
                                              , copyBytes, moveBytes
                                              )
import Foreign.Storable                       ( Storable )

#ifdef __HADDOCK__
import Foreign.Storable                       ( sizeOf )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode                  ( (∘) )

-- from transformers:
import Control.Monad.IO.Class                 ( MonadIO, liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO                  ( MonadCatchIO )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT, ParentOf )

-- from ourselves:
import Foreign.Ptr.Region                     ( RegionalPtr )
import Foreign.Ptr.Region.Unsafe              ( unsafePtr )
import Foreign.Marshal.Alloc.Region           ( alloca, malloc )
import Foreign.Storable.Region                ( poke )


--------------------------------------------------------------------------------
-- * General marshalling utilities
--------------------------------------------------------------------------------

-- ** Combined allocation and marshalling

-- | @'with' val f@ executes the computation @f@, passing as argument a regional
-- pointer to a temporarily allocated block of memory into which @val@ has been
-- marshalled (the combination of 'alloca' and 'poke').
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception).
with ∷ (Storable α, MonadCatchIO pr)
     ⇒ α → (∀ s. RegionalPtr α (RegionT s pr) → RegionT s pr β) → pr β
with val f = alloca $ \ptr → poke ptr val >> f ptr

-- | Allocate a block of memory and marshal a value into it (the combination of
-- 'malloc' and 'poke').  The size of the area allocated is determined by the
-- 'sizeOf' method from the instance of 'Storable' for the appropriate type.
new ∷ (Storable α, MonadCatchIO pr)
    ⇒ α → RegionT s pr (RegionalPtr α (RegionT s pr))
new val = do ptr ← malloc
             poke ptr val
             return ptr

-- TODO:
-- -- ** Marshalling of Maybe values
-- maybeNew
-- maybeWith
-- maybePeek

-- TODO
-- -- ** Marshalling lists of storable objects
-- withMany :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res

-- ** Haskellish interface to memcpy and memmove

-- | Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas may /not/ overlap
--
-- Wraps: @Foreign.Marshal.Utils.'FMU.copyBytes'@.
copyBytes ∷ ( pr1 `ParentOf` cr
            , pr2 `ParentOf` cr
            , MonadIO cr
            )
          ⇒ RegionalPtr α pr1 -- ^ Destination
          → RegionalPtr α pr2 -- ^ Source
          → Int               -- ^ Number of bytes to copy
          → cr ()
copyBytes rp1 rp2 = liftIO ∘ FMU.copyBytes (unsafePtr rp1) (unsafePtr rp2)

-- | Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas /may/ overlap
--
-- Wraps: @Foreign.Marshal.Utils.'FMU.moveBytes'@.
moveBytes ∷ ( pr1 `ParentOf` cr
            , pr2 `ParentOf` cr
            , MonadIO cr
            )
          ⇒ RegionalPtr α pr1 -- ^ Destination
          → RegionalPtr α pr2 -- ^ Source
          → Int               -- ^ Number of bytes to move
          → cr ()
moveBytes rp1 rp2 = liftIO ∘ FMU.moveBytes (unsafePtr rp1) (unsafePtr rp2)


-- The End ---------------------------------------------------------------------
