-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region.Unsfe
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /Unsafe/ functions for constructing regional pointers, retrieving the native
-- @Ptr@ from a regional pointer and for lifting operations on @Ptrs@ to
-- @RegionalPtrs@.
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region.Unsafe
    ( -- * Unsafely constructing regional pointers
      unsafeRegionalPtr

      -- * Wrapping @alloca@- and @malloc@-like functions
    , wrapAlloca, wrapAlloca2
    , wrapMalloc

      -- * Wrapping @CStringLen@ operations
    , wrapPeekStringLen
    , wrapNewStringLen
    , wrapWithStringLen

      -- * Unsafe utility functions for lifting operations on @Ptrs@ to @RegionalPtrs@
    , unsafePtr

    , unsafeWrap, unsafeWrap2, unsafeWrap3

    , unsafeWrap2flp
    ) where

import Foreign.Ptr.Region.Internal
