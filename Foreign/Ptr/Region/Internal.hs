{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , RankNTypes
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
import Control.Monad                     ( return, liftM )
import Control.Arrow                     ( first )
import Data.Function                     ( ($), flip )
import Data.Int                          ( Int )
import Data.Char                         ( String )
import System.IO                         ( IO )
import Foreign.Ptr                       ( Ptr )
import qualified Foreign.Ptr as FP       ( nullPtr )
import Foreign.Marshal.Alloc             ( free )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad                     ( (>>=), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode             ( (∘) )

-- from transformers:
import Control.Monad.IO.Class            ( MonadIO, liftIO )

-- from regions:
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, Finalizer, onExit )
import Control.Monad.Trans.Region        ( RegionT
                                         , AncestorRegion
                                         , RootRegion
                                         , LocalRegion, Local
                                         , Dup(dup)
                                         )
import Control.Monad.Trans.Region.Unsafe ( unsafeStripLocal )

-- from monad-control:
import Control.Monad.IO.Control          ( MonadControlIO, controlIO, liftIOOp )

#if MIN_VERSION_base(4,3,0)
import Control.Exception.Control         ( mask_ )
#else
import Control.Exception.Control         ( block )

mask_ ∷ MonadControlIO m ⇒ m a → m a
mask_ = block
#endif


--------------------------------------------------------------------------------
-- * Regional pointers
--------------------------------------------------------------------------------

{-|
A regional pointer to memory.

This should provide a safer replacement for @Foreign.Ptr.'Ptr'@
-}
data RegionalPtr α (r ∷ * → *) = RegionalPtr !(Ptr α) !(FinalizerHandle r)

instance Dup (RegionalPtr α) where
    dup (RegionalPtr ptr ch) = liftM (RegionalPtr ptr) (dup ch)

{-|
Construct a regional pointer from a native pointer and an @IO@ computation that
finalizes the pointer (like @free ptr@) which is performed when the region
terminates.

This function is unsafe because this library can't guarantee that the
finalizer will actually finalize the pointer (suppose having @return ()@ as
the finalizer). You have to verify the correct finalisation yourself.
-}
unsafeRegionalPtr ∷ MonadIO pr
                  ⇒ Ptr α
                  → Finalizer
                  → RegionT s pr (RegionalPtr α (RegionT s pr))
unsafeRegionalPtr ptr finalize = liftM (RegionalPtr ptr) (onExit finalize)

wrapMalloc ∷ MonadControlIO pr
           ⇒ IO (Ptr α) → RegionT s pr (RegionalPtr α (RegionT s pr))
wrapMalloc doMalloc = mask_ $ do
                        ptr ← liftIO doMalloc
                        unsafeRegionalPtr ptr (free ptr)


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
nullPtr ∷ NullPtr α RootRegion
nullPtr = NullPtr FP.nullPtr

newtype NullPtr α (r ∷ * → *) = NullPtr (Ptr α)


--------------------------------------------------------------------------------
-- * Foreign regional pointers
--------------------------------------------------------------------------------

-- | A regional pointer to memory which was locally allocated
-- by one of the @alloca@-like functions.
--
-- Note that a @LocalPtr@ can not be 'dup'licated to a parent region.
newtype LocalPtr α (r ∷ * → *) = LocalPtr (Ptr α)

wrapAlloca ∷ MonadControlIO pr
           ⇒ ((Ptr α → IO (RegionT s pr β)) → IO (RegionT s pr β))
           → (∀ sl. LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
           → RegionT s pr β
wrapAlloca doAlloca f = liftIOOp doAlloca $
                          unsafeStripLocal ∘ f ∘ LocalPtr

wrapAlloca2 ∷ MonadControlIO pr
            ⇒ ((γ → Ptr α → IO (RegionT s pr β)) → IO (RegionT s pr β))
            → (∀ sl. γ → LocalPtr α (LocalRegion sl s) → RegionT (Local s) pr β)
            → RegionT s pr β
wrapAlloca2 doAlloca f = controlIO $ \runInIO →
                           doAlloca $ \s →
                             runInIO ∘ unsafeStripLocal ∘ f s ∘ LocalPtr


--------------------------------------------------------------------------------
-- * Wrapping @CStringLen@ operations
--------------------------------------------------------------------------------

wrapPeekStringLen ∷ (Pointer pointer, pr `AncestorRegion` cr, MonadIO cr)
                  ⇒ ((Ptr α, Int) → IO String)
                  → (pointer α pr, Int) → cr String
wrapPeekStringLen peekStringLen = liftIO ∘ peekStringLen ∘ first unsafePtr

wrapNewStringLen ∷ MonadControlIO pr
                 ⇒ IO (Ptr α, Int)
                 → RegionT s pr (RegionalPtr α (RegionT s pr), Int)
wrapNewStringLen newStringLen = mask_ $ do
                                  (ptr, len) ← liftIO newStringLen
                                  rPtr ← unsafeRegionalPtr ptr (free ptr)
                                  return (rPtr, len)

wrapWithStringLen ∷ MonadControlIO pr
                  ⇒ (((Ptr α, Int) → IO (RegionT s pr β)) → IO (RegionT s pr β))
                  → (∀ sl. (LocalPtr α (LocalRegion sl s), Int) → RegionT (Local s) pr β)
                  → RegionT s pr β
wrapWithStringLen withStringLen f = liftIOOp withStringLen $
                                      unsafeStripLocal ∘ f ∘ first LocalPtr


--------------------------------------------------------------------------------
-- * Utility functions for lifting operations on Ptrs to RegionalPtrs
--------------------------------------------------------------------------------

class Pointer (pointer ∷ * → (* → *) → *) where
    -- | Retrieve the native pointer from a regional pointer.
    --
    -- This function is unsafe because it allows you to both @free@ the pointer
    -- before the region terminates and use the pointer outside the region when it
    -- is already freed.
    unsafePtr ∷ pointer α r → Ptr α

    -- | Apply a /pure/ function to the inner pointer of a regional pointer.
    mapPointer ∷ (Ptr α → Ptr β) → (pointer α r → pointer β r)

instance Pointer RegionalPtr where
    unsafePtr    (RegionalPtr ptr _)  = ptr
    mapPointer f (RegionalPtr ptr ch) = RegionalPtr (f ptr) ch

instance Pointer NullPtr where
    unsafePtr    (NullPtr ptr) = ptr
    mapPointer f (NullPtr ptr) = NullPtr (f ptr)

instance Pointer LocalPtr where
    unsafePtr    (LocalPtr ptr) = ptr
    mapPointer f (LocalPtr ptr) = LocalPtr (f ptr)

unsafeWrap ∷ (MonadIO m, Pointer pointer)
           ⇒ (Ptr     α   → IO β)
           → (pointer α r → m  β)
unsafeWrap f pointer = liftIO $ f (unsafePtr pointer)

unsafeWrap2 ∷ (MonadIO m, Pointer pointer)
            ⇒ (Ptr     α   → γ → IO β)
            → (pointer α r → γ → m  β)
unsafeWrap2 f pointer x = liftIO $ f (unsafePtr pointer) x

unsafeWrap3 ∷ (MonadIO m, Pointer pointer)
            ⇒ (Ptr     α   → γ → δ → IO β)
            → (pointer α r → γ → δ → m  β)
unsafeWrap3 f pointer x y = liftIO $ f (unsafePtr pointer) x y

unsafeWrap2flp ∷ (MonadIO m, Pointer pointer)
               ⇒ (γ → Ptr     α   → IO β)
               → (γ → pointer α r → m  β)
unsafeWrap2flp = flip ∘ unsafeWrap2 ∘ flip


--------------------------------------------------------------------------------
-- * Allocated pointers
--------------------------------------------------------------------------------

class Pointer pointer ⇒ PrivateAllocatedPointer pointer

-- | Class of pointers which point to allocated memory. 'NullPtr' is the only
-- pointer which is not an instance of this class.
--
-- The super class 'PrivateAllocatedPointer' is not exported by this module
-- which ensures you can't accidentally make 'NullPtr' an instance of this class.
class PrivateAllocatedPointer pointer ⇒ AllocatedPointer pointer

instance PrivateAllocatedPointer RegionalPtr; instance AllocatedPointer RegionalPtr
instance PrivateAllocatedPointer LocalPtr;    instance AllocatedPointer LocalPtr


-- The End ---------------------------------------------------------------------
