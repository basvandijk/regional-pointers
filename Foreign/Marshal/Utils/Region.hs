{-# LANGUAGE NoImplicitPrelude
           , CPP
           , RankNTypes
           , GADTs
           , KindSignatures
           , FlexibleContexts
           , ImpredicativeTypes
           , TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Utils.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
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

         -- ** Marshalling of @MaybePointer@ values
       , MaybePointer(..)
       , maybeNew
       , maybeWith
       , MaybePeek(maybePeek)

         -- ** Marshalling lists of storable objects
       , FMU.withMany

         -- ** Haskellish interface to memcpy and memmove
         -- | (argument order: destination, source)
       , copyBytes
       , moveBytes
       ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import System.IO                              ( IO )
import Control.Category                       ( (.) )
import qualified Foreign.Marshal.Utils as FMU ( with,      new
                                              , fromBool,  toBool
                                              , withMany
                                              , copyBytes, moveBytes
                                              )
import Foreign.Storable                       ( Storable )

#ifdef __HADDOCK__
import Foreign.Storable                       ( sizeOf )
import qualified Foreign.Marshal.Utils as FMU ( maybeNew, maybeWith, maybePeek )
#endif

import Data.Int                               ( Int )
import Data.Maybe                             ( Maybe(Nothing, Just) )
import Data.Functor                           ( (<$>) )
import Control.Applicative                    ( Applicative, pure )
import Control.Monad                          ( Monad, return )

-- from transformers-base:
import Control.Monad.Base                     ( MonadBase, liftBase )

-- from regions:
import Control.Monad.Trans.Region             ( RegionT
                                              , RegionBaseControl
                                              , AncestorRegion
                                              , RootRegion
                                              , LocalRegion, Local
                                              )

-- from ourselves:
import Foreign.Ptr.Region                     ( AllocatedPointer
                                              , RegionalPtr
                                              , NullPtr, nullPtr
                                              )
import Foreign.Marshal.Alloc.Region           ( LocalPtr )
import Foreign.Ptr.Region.Unsafe              ( unsafePtr
                                              , wrapAlloca, wrapMalloc
                                              )


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
--
-- This provides a safer replacement for @Foreign.Marshal.Utils.'FMU.with'@.
with :: (Storable a, RegionBaseControl IO pr)
     => a -> (forall sl. LocalPtr a (LocalRegion sl s)
             -> RegionT (Local s) pr b) -- ^
     -> RegionT s pr b
with = wrapAlloca . FMU.with

-- | Allocate a block of memory and marshal a value into it (the combination of
-- 'malloc' and 'poke').  The size of the area allocated is determined by the
-- 'sizeOf' method from the instance of 'Storable' for the appropriate type.
--
-- This provides a safer replacement for @Foreign.Marshal.Utils.'FMU.new'@.
new :: (region ~ RegionT s pr, RegionBaseControl IO pr, Storable a)
    => a -> region (RegionalPtr a region)
new = wrapMalloc . FMU.new


-- ** Marshalling of @MaybePointer@ values

-- | A @'MaybePointer' a@ corresponds to a @'Maybe' a@
-- but additionally introduces some type equalities to the type-checker.
data MaybePointer (a :: *) (pointer :: *) (b :: *) (r :: * -> *) where
    NullPointer ::      MaybePointer a (NullPtr     b RootRegion) b RootRegion
    JustPointer :: a -> MaybePointer a (RegionalPtr b r)          b r

-- | Allocate storage and marshal a storable value wrapped into a 'MaybePointer'.
--
-- The 'nullPtr' is used to represent 'NullPointer'.
--
-- Alternative for 'FMU.maybeNew'.
maybeNew :: Monad m
         => (a -> m (RegionalPtr b r)) -- ^
         -> (MaybePointer a pointer b r -> m pointer)
maybeNew _  NullPointer    = return nullPtr
maybeNew f (JustPointer x) = f x

-- | Converts a @withXXX@ combinator into one marshalling a value wrapped
-- into a 'MaybePointer', using 'nullPtr' to represent 'NoPointer'.
--
-- Alternative for 'FMU.maybeWith'
maybeWith :: (a ->                          (pointer -> m c) -> m c) -- ^
          -> (MaybePointer a pointer b r -> (pointer -> m c) -> m c)
maybeWith _  NullPointer    g = g nullPtr
maybeWith f (JustPointer x) g = f x g

class MaybePeek (pointer :: * -> (* -> *) -> *) where
    -- | Convert a @peek@ combinator into a one returning 'Nothing'
    -- if applied to a 'nullPtr'.
    --
    -- Alternative for 'FMU.maybePeek'.
    maybePeek :: Applicative m
              => (pointer a r -> m b) -- ^
              -> (pointer a r -> m (Maybe b))

instance MaybePeek NullPtr     where maybePeek _    _   = pure Nothing
instance MaybePeek RegionalPtr where maybePeek peek ptr = Just <$> peek ptr

-- TODO
-- -- ** Marshalling lists of storable objects
-- withMany :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res

-- ** Haskellish interface to memcpy and memmove

-- | Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas may /not/ overlap
--
-- Wraps: @Foreign.Marshal.Utils.'FMU.copyBytes'@.
copyBytes :: ( AllocatedPointer pointer1
             , AllocatedPointer pointer2
             , pr1 `AncestorRegion` cr
             , pr2 `AncestorRegion` cr
             , MonadBase IO cr
             )
          => pointer1 a pr1 -- ^ Destination
          -> pointer2 a pr2 -- ^ Source
          -> Int            -- ^ Number of bytes to copy
          -> cr ()
copyBytes pointer1 pointer2 = liftBase . FMU.copyBytes (unsafePtr pointer1)
                                                       (unsafePtr pointer2)

-- | Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas /may/ overlap
--
-- Wraps: @Foreign.Marshal.Utils.'FMU.moveBytes'@.
moveBytes :: ( AllocatedPointer pointer1
             , AllocatedPointer pointer2
             , pr1 `AncestorRegion` cr
             , pr2 `AncestorRegion` cr
             , MonadBase IO cr
             )
          => pointer1 a pr1 -- ^ Destination
          -> pointer2 a pr2 -- ^ Source
          -> Int            -- ^ Number of bytes to move
          -> cr ()
moveBytes pointer1 pointer2 = liftBase . FMU.moveBytes (unsafePtr pointer1)
                                                       (unsafePtr pointer2)
