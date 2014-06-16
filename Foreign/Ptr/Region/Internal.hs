{-# LANGUAGE NoImplicitPrelude
           , RankNTypes
           , FlexibleContexts
           , TypeFamilies
           , TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region.Internal
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region.Internal
    ( -- * Regional pointers
      RegionalPtr(RegionalPtr)
    , unsafeRegionalPtr
    , wrapMalloc

      -- * Null pointers
    , nullPtr, NullPtr

      -- * Foreign regional pointers
    , LocalPtr
    , wrapAlloca, wrapAlloca2

      -- * Wrapping @CStringLen@ operations
    , wrapPeekStringLen
    , wrapNewStringLen
    , wrapWithStringLen

      -- * Unsafe utility functions for lifting operations on @Ptrs@ to @RegionalPtrs@
    , Pointer(unsafePtr, mapPointer)

    , unsafeWrap, unsafeWrap2, unsafeWrap3

    , unsafeWrap2flp

      -- * Pointers to allocated memory
    , AllocatedPointer
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception                 ( mask_ )
import Control.Monad                     ( return, liftM )
import Control.Arrow                     ( first )
import Data.Function                     ( ($), (.), flip )
import Data.Int                          ( Int )
import System.IO                         ( IO )
import Foreign.Ptr                       ( Ptr )
import qualified Foreign.Ptr as FP       ( nullPtr )
import Foreign.Marshal.Alloc             ( free )

import Data.String                       ( String )

-- from transformers-base:
import Control.Monad.Base                ( MonadBase, liftBase )

-- from regions:
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, Finalizer, onExit )
import Control.Monad.Trans.Region        ( RegionT
                                         , RegionBaseControl
                                         , AncestorRegion
                                         , RootRegion
                                         , LocalRegion, Local
                                         , Dup(dup)
                                         )
import Control.Monad.Trans.Region.Unsafe ( unsafeStripLocal
                                         , unsafeControl
                                         , unsafeLiftBaseOp
                                         , RegionBaseControl(..)
                                         )

--------------------------------------------------------------------------------
-- * Regional pointers
--------------------------------------------------------------------------------

{-|
A regional pointer to memory.

This should provide a safer replacement for @Foreign.Ptr.'Ptr'@
-}
data RegionalPtr a (r :: * -> *) = RegionalPtr !(Ptr a) !(FinalizerHandle r)

instance Dup (RegionalPtr a) where
    dup (RegionalPtr ptr ch) = liftM (RegionalPtr ptr) (dup ch)

{-|
Construct a regional pointer from a native pointer and an @IO@ computation that
finalizes the pointer (like @free ptr@) which is performed when the region
terminates.

This function is unsafe because this library can't guarantee that the
finalizer will actually finalize the pointer (suppose having @return ()@ as
the finalizer). You have to verify the correct finalisation yourself.
-}
unsafeRegionalPtr :: ( region ~ RegionT s pr, MonadBase IO pr)
                  => Ptr a
                  -> Finalizer
                  -> region (RegionalPtr a region)
unsafeRegionalPtr ptr finalize = liftM (RegionalPtr ptr) (onExit finalize)

wrapMalloc :: ( region ~ RegionT s pr
              , RegionBaseControl IO pr
              , MonadBase IO pr
              )
           => IO (Ptr a) -> region (RegionalPtr a region)
wrapMalloc doMalloc = unsafeControl $ \runInIO -> mask_ $ do
                        ptr <- doMalloc
                        runInIO $ unsafeRegionalPtr ptr (liftBase $ free ptr)


--------------------------------------------------------------------------------
-- * Null pointers
--------------------------------------------------------------------------------

{-|
The constant @nullPtr@ is a pointer which is not associated with a valid
memory location.

Note that @nullPtr@ is a pure value. This means it does not perform the
side-effect of registering a finalizer like \"@free nullPtr@\"
in the 'RegionT' monad.

Finally note that the region parameter of the 'NullPtr' is set to
'RootRegion' which is the ancestor of any region.
This allows 'nullPtr' to be used in any region.
-}
nullPtr :: NullPtr a RootRegion
nullPtr = NullPtr FP.nullPtr

newtype NullPtr a (r :: * -> *) = NullPtr (Ptr a)


--------------------------------------------------------------------------------
-- * Foreign regional pointers
--------------------------------------------------------------------------------

-- | A regional pointer to memory which was locally allocated
-- by one of the @alloca@-like functions.
--
-- Note that a @LocalPtr@ can not be 'dup'licated to a parent region.
newtype LocalPtr a (r :: * -> *) = LocalPtr (Ptr a)

wrapAlloca :: (RegionBaseControl m pr)
           => ((Ptr a -> m (RegionStM (RegionT s pr) b)) -> m (RegionStM (RegionT s pr) b))
           -> (forall sl. LocalPtr a (LocalRegion sl s) -> RegionT (Local s) pr b)
           -> RegionT s pr b
wrapAlloca doAlloca f = unsafeLiftBaseOp doAlloca $
                          unsafeStripLocal . f . LocalPtr

wrapAlloca2 :: (RegionBaseControl m pr)
            => ((c -> Ptr a -> m (RegionStM (RegionT s pr) b)) -> m (RegionStM (RegionT s pr) b))
            -> (forall sl. c -> LocalPtr a (LocalRegion sl s) -> RegionT (Local s) pr b)
            -> RegionT s pr b
wrapAlloca2 doAlloca f = unsafeControl $ \runInIO ->
                           doAlloca $ \s ->
                             runInIO . unsafeStripLocal . f s . LocalPtr


--------------------------------------------------------------------------------
-- * Wrapping @CStringLen@ operations
--------------------------------------------------------------------------------

wrapPeekStringLen :: ( Pointer pointer
                     , pr `AncestorRegion` cr
                     , MonadBase m cr
                     )
                  => ((Ptr a, Int) -> m String)
                  -> (pointer a pr, Int) -> cr String
wrapPeekStringLen peekStringLen = liftBase . peekStringLen . first unsafePtr

wrapNewStringLen :: ( region ~ RegionT s pr
                    , RegionBaseControl IO pr
                    , MonadBase IO pr
                    )
                 => IO (Ptr a, Int)
                 -> region (RegionalPtr a region, Int)
wrapNewStringLen newStringLen = unsafeControl $ \runInIO -> mask_ $ do
                                  (ptr, len) <- newStringLen
                                  runInIO $ do
                                    rPtr <- unsafeRegionalPtr ptr ((liftBase free) ptr)
                                    return (rPtr, len)

wrapWithStringLen :: (RegionBaseControl m pr)
                  => (((Ptr a, Int) -> m (RegionStM (RegionT s pr) b)) -> m (RegionStM (RegionT s pr) b))
                  -> (forall sl. (LocalPtr a (LocalRegion sl s), Int) -> RegionT (Local s) pr b)
                  -> RegionT s pr b
wrapWithStringLen withStringLen f = unsafeLiftBaseOp withStringLen $
                                      unsafeStripLocal . f . first LocalPtr


--------------------------------------------------------------------------------
-- * Utility functions for lifting operations on Ptrs to RegionalPtrs
--------------------------------------------------------------------------------

class Pointer (pointer :: * -> (* -> *) -> *) where
    -- | Retrieve the native pointer from a regional pointer.
    --
    -- This function is unsafe because it allows you to both @free@ the pointer
    -- before the region terminates and use the pointer outside the region when it
    -- is already freed.
    unsafePtr :: pointer a r -> Ptr a

    -- | Apply a /pure/ function to the inner pointer of a regional pointer.
    mapPointer :: (Ptr a -> Ptr b) -> (pointer a r -> pointer b r)

instance Pointer RegionalPtr where
    unsafePtr    (RegionalPtr ptr _)  = ptr
    mapPointer f (RegionalPtr ptr ch) = RegionalPtr (f ptr) ch

instance Pointer NullPtr where
    unsafePtr    (NullPtr ptr) = ptr
    mapPointer f (NullPtr ptr) = NullPtr (f ptr)

instance Pointer LocalPtr where
    unsafePtr    (LocalPtr ptr) = ptr
    mapPointer f (LocalPtr ptr) = LocalPtr (f ptr)

unsafeWrap :: (MonadBase IO m, Pointer pointer)
           => (Ptr     a   -> IO b)
           -> (pointer a r -> m  b)
unsafeWrap f pointer = liftBase $ f (unsafePtr pointer)

unsafeWrap2 :: (MonadBase IO m, Pointer pointer)
            => (Ptr     a   -> c -> IO b)
            -> (pointer a r -> c -> m  b)
unsafeWrap2 f pointer x = liftBase $ f (unsafePtr pointer) x

unsafeWrap3 :: (MonadBase IO m, Pointer pointer)
            => (Ptr     a   -> c -> d -> IO b)
            -> (pointer a r -> c -> d -> m  b)
unsafeWrap3 f pointer x y = liftBase $ f (unsafePtr pointer) x y

unsafeWrap2flp :: (MonadBase IO m, Pointer pointer)
               => (c -> Ptr     a   -> IO b)
               -> (c -> pointer a r -> m  b)
unsafeWrap2flp = flip . unsafeWrap2 . flip

--------------------------------------------------------------------------------
-- * Allocated pointers
--------------------------------------------------------------------------------

class Pointer pointer => PrivateAllocatedPointer pointer

-- | Class of pointers which point to allocated memory. 'NullPtr' is the only
-- pointer which is not an instance of this class.
--
-- The super class 'PrivateAllocatedPointer' is not exported by this module
-- which ensures you can't accidentally make 'NullPtr' an instance of this class.
class PrivateAllocatedPointer pointer => AllocatedPointer pointer

instance PrivateAllocatedPointer RegionalPtr; instance AllocatedPointer RegionalPtr
instance PrivateAllocatedPointer LocalPtr;    instance AllocatedPointer LocalPtr
