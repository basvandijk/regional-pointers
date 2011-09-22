{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , RankNTypes
           , CPP
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.String.Region
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Lifts functions and types from @Foreign.C.String@ to regional pointers.
--
-------------------------------------------------------------------------------

module Foreign.C.String.Region
       ( -- * Regional C Strings
         RegionalCString,  RegionalCStringLen

         -- * Using a locale-dependent encoding
       , peekCString,          peekCStringLen
       , newCString,           newCStringLen
       , withCString,          withCStringLen

       , charIsRepresentable

         -- * Using 8-bit characters
       , FCS.castCharToCChar,  FCS.castCCharToChar

#if MIN_VERSION_base(4,3,0)
       , FCS.castCharToCUChar, FCS.castCUCharToChar
       , FCS.castCharToCSChar, FCS.castCSCharToChar
#endif
       , peekCAString,         peekCAStringLen
       , newCAString,          newCAStringLen
       , withCAString,         withCAStringLen

         -- * C wide strings
       , RegionalCWString,     RegionalCWStringLen
       , peekCWString,         peekCWStringLen
       , newCWString,          newCWStringLen
       , withCWString,         withCWStringLen
       ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool                         ( Bool )
import Data.Int                          ( Int )
import Data.Char                         ( Char )
import Foreign.C.Types                   ( CChar, CWchar )
import qualified Foreign.C.String as FCS ( peekCString,     peekCStringLen
                                         , newCString,      newCStringLen
                                         , withCString,     withCStringLen
                                         , charIsRepresentable
                                         , castCharToCChar, castCCharToChar
#if MIN_VERSION_base(4,3,0)
                                         , castCharToCUChar, castCUCharToChar
                                         , castCharToCSChar, castCSCharToChar
#endif
                                         , peekCAString,     peekCAStringLen
                                         , newCAString,      newCAStringLen
                                         , withCAString,     withCAStringLen
                                         , peekCWString,     peekCWStringLen
                                         , newCWString,      newCWStringLen
                                         , withCWString,     withCWStringLen
                                         )
#ifdef __HADDOCK__
import Foreign.C.String                  ( CString,  CStringLen
                                         , CWString, CWStringLen
                                         )
#endif

#if MIN_VERSION_base(4,4,0)
import Data.String                       ( String )
#else
import Data.Char                         ( String )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode             ( (∘) )

-- from transformers:
import Control.Monad.IO.Class            ( MonadIO, liftIO )

-- from monad-control:
import Control.Monad.IO.Control          ( MonadControlIO )

-- from regions:
import Control.Monad.Trans.Region        ( RegionT
                                         , AncestorRegion
                                         , LocalRegion, Local
                                         )

-- from ourselves:
import Foreign.Ptr.Region                ( AllocatedPointer, RegionalPtr )
import Foreign.Marshal.Alloc.Region      ( LocalPtr )
import Foreign.Ptr.Region.Unsafe         ( wrapMalloc, wrapAlloca

                                         , wrapPeekStringLen
                                         , wrapNewStringLen
                                         , wrapWithStringLen

                                         , unsafeWrap
                                         )


--------------------------------------------------------------------------------
-- * Regional C Strings
--------------------------------------------------------------------------------

-- | Handy type synonym for a regional pointer to an array of C characters
-- terminated by a NUL.
--
-- This should provide a safer replacement for @Foreign.C.String.'CString'@.
type RegionalCString (pointer ∷ * → (* → *) → *) r = pointer CChar r

-- | Handy type synonym for a regional pointer to an array of C characters which
-- is paired with the length of the array instead of terminated by a NUL.
-- (Thus allowing NUL characters in the middle of the string)
--
-- This should provide a safer replacement for @Foreign.C.String.'CStringLen'@.
type RegionalCStringLen pointer r = (RegionalCString pointer r, Int)


--------------------------------------------------------------------------------
-- * Using a locale-dependent encoding
--------------------------------------------------------------------------------

-- | Marshal a NUL terminated C string into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCString'@
peekCString ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
             ⇒ RegionalCString pointer pr → cr String
peekCString = unsafeWrap FCS.peekCString

-- | Marshal a C string with explicit length into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCStringLen'@.
peekCStringLen ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
               ⇒ RegionalCStringLen pointer pr → cr String
peekCStringLen = wrapPeekStringLen FCS.peekCStringLen

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- The Haskell string may /not/ contain any NUL characters
--
-- Wraps: @Foreign.C.String.'FCS.newCString'@.
newCString ∷ MonadControlIO pr
           ⇒ String → RegionT s pr (RegionalCString RegionalPtr (RegionT s pr))
newCString = wrapMalloc ∘ FCS.newCString

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- Wraps: @Foreign.C.String.'FCS.newCStringLen'@.
newCStringLen ∷ MonadControlIO pr
              ⇒ String → RegionT s pr (RegionalCStringLen RegionalPtr (RegionT s pr))
newCStringLen = wrapNewStringLen ∘ FCS.newCStringLen

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either normally or
--   via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCString'@.
withCString ∷ MonadControlIO pr
            ⇒ String
            → (∀ sl. RegionalCString LocalPtr (LocalRegion sl s)
                   → RegionT (Local s) pr α
              )
            → RegionT s pr α
withCString = wrapAlloca ∘ FCS.withCString

-- | Marshal a Haskell string into a C string (ie, character array) in temporary
-- storage, with explicit length information.
--
-- * the memory is freed when the subcomputation terminates (either normally or
--   via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCStringLen'@.
withCStringLen ∷ MonadControlIO pr
               ⇒ String
               → (∀ sl. RegionalCStringLen LocalPtr (LocalRegion sl s)
                      → RegionT (Local s) pr α
                 )
               → RegionT s pr α
withCStringLen = wrapWithStringLen ∘ FCS.withCStringLen

-- | Generalizes @Foreign.C.String.'FCS.charIsRepresentable'@ to any 'MonadIO'.
charIsRepresentable ∷ MonadIO m ⇒ Char → m Bool
charIsRepresentable = liftIO ∘ FCS.charIsRepresentable


--------------------------------------------------------------------------------
-- * Using 8-bit characters
--------------------------------------------------------------------------------

-- | Marshal a NUL terminated C string into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCAString'@.
peekCAString ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
             ⇒ RegionalCString pointer pr → cr String
peekCAString = unsafeWrap FCS.peekCAString

-- | Marshal a C string with explicit length into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCAStringLen'@.
peekCAStringLen ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
                ⇒ RegionalCStringLen pointer pr → cr String
peekCAStringLen = wrapPeekStringLen FCS.peekCAStringLen

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- The Haskell string may /not/ contain any NUL characters
--
-- Wraps: @Foreign.C.String.'FCS.newCAString'@.
newCAString ∷ MonadControlIO pr
            ⇒ String → RegionT s pr (RegionalCString RegionalPtr (RegionT s pr))
newCAString = wrapMalloc ∘ FCS.newCAString

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- Wraps: @Foreign.C.String.'FCS.newCAStringLen'@.
newCAStringLen ∷ MonadControlIO pr
               ⇒ String → RegionT s pr (RegionalCStringLen RegionalPtr (RegionT s pr))
newCAStringLen = wrapNewStringLen ∘ FCS.newCAStringLen

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either normally or
-- via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCAString'@.
withCAString ∷ MonadControlIO pr
             ⇒ String
             → (∀ sl. RegionalCString LocalPtr (LocalRegion sl s)
                    → RegionT (Local s) pr α
               )
             → RegionT s pr α
withCAString = wrapAlloca ∘ FCS.withCAString

-- | Marshal a Haskell string into a C string (ie, character array) in temporary
-- storage, with explicit length information.
--
-- * the memory is freed when the subcomputation terminates (either normally or
--   via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCAStringLen'@.
withCAStringLen ∷ MonadControlIO pr
                ⇒ String
                → (∀ sl. RegionalCStringLen LocalPtr (LocalRegion sl s)
                       → RegionT (Local s) pr α
                  )
                → RegionT s pr α
withCAStringLen = wrapWithStringLen ∘ FCS.withCAStringLen


--------------------------------------------------------------------------------
-- * C wide strings
--------------------------------------------------------------------------------

-- | Handy type synonym for a regional pointer to an array of C wide characters
-- terminated by a NUL.
--
-- This should provide a safer replacement for @Foreign.C.String.'CWString'@.
type RegionalCWString (pointer ∷ * → (* → *) → *) r = pointer CWchar r

-- | Handy type synonym for a regional pointer to an array of C wide characters
-- which is paired with the length of the array instead of terminated by a NUL.
-- (Thus allowing NUL characters in the middle of the string)
--
-- This should provide a safer replacement for @Foreign.C.String.'CWStringLen'@.
type RegionalCWStringLen pointer r = (RegionalCWString pointer r, Int)

-- | Marshal a NUL terminated C wide string into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCWString'@.
peekCWString ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
             ⇒ RegionalCWString pointer pr → cr String
peekCWString = unsafeWrap FCS.peekCWString

-- | Marshal a C wide string with explicit length into a Haskell string.
--
-- Wraps: @Foreign.C.String.'FCS.peekCWStringLen'@.
peekCWStringLen ∷ (AllocatedPointer pointer, pr `AncestorRegion` cr, MonadIO cr)
                ⇒ RegionalCWStringLen pointer pr → cr String
peekCWStringLen = wrapPeekStringLen FCS.peekCWStringLen

-- | Marshal a Haskell string into a NUL terminated C wide string.
--
-- The Haskell string may /not/ contain any NUL characters.
--
-- Wraps: @Foreign.C.String.'FCS.newCWString'@.
newCWString ∷ MonadControlIO pr
            ⇒ String → RegionT s pr (RegionalCWString RegionalPtr (RegionT s pr))
newCWString = wrapMalloc ∘ FCS.newCWString

-- | Marshal a Haskell string into a C wide string (ie, wide character array)
-- with explicit length information.
--
-- Wraps: @Foreign.C.String.'FCS.newCWStringLen'@.
newCWStringLen ∷ MonadControlIO pr
               ⇒ String → RegionT s pr (RegionalCWStringLen RegionalPtr (RegionT s pr))
newCWStringLen = wrapNewStringLen ∘ FCS.newCWStringLen

-- | Marshal a Haskell string into a NUL terminated C wide string using
-- temporary storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCWString'@.
withCWString ∷ MonadControlIO pr
             ⇒ String
             → (∀ sl. RegionalCWString LocalPtr (LocalRegion sl s)
                    → RegionT (Local s) pr α
               )
             → RegionT s pr α
withCWString = wrapAlloca ∘ FCS.withCWString

-- | Marshal a Haskell string into a NUL terminated C wide string using
-- temporary storage.
--
-- * the Haskell string may /not/ contain any NUL characters.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception).
--
-- Wraps: @Foreign.C.String.'FCS.withCWStringLen'@.
withCWStringLen ∷ MonadControlIO pr
                ⇒ String
                → (∀ sl. RegionalCWStringLen LocalPtr (LocalRegion sl s)
                       → RegionT (Local s) pr α
                  )
                → RegionT s pr α
withCWStringLen = wrapWithStringLen ∘ FCS.withCWStringLen


-- The End ---------------------------------------------------------------------
