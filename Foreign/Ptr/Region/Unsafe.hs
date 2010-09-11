{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr.Region.Unsfe
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /Unsafe/ functions for constructing regional pointers, retrieving the native
-- @Ptr@ from a regional pointer and for lifting operations on @Ptrs@ to
-- @RegionalPtrs@.
--
--
-------------------------------------------------------------------------------

module Foreign.Ptr.Region.Unsafe
    ( -- * Unsafely constructing regional pointers
      unsafeRegionalPtr
    , unsafePureRegionalPtr

      -- * Unsafe utility functions for lifting operations on @Ptrs@ to @RegionalPtrs@

    --  | These operations are unsafe because they allow you to @free@ the
    --  regional pointer before exiting their region. So they enable you to
    --  perform @IO@ with already freed pointers.
    , unsafePtr
    , unsafeWrap, unsafeWrap2, unsafeWrap3
    ) where

import Foreign.Ptr.Region.Internal ( unsafeRegionalPtr
                                   , unsafePureRegionalPtr

                                   , unsafePtr
                                   , unsafeWrap, unsafeWrap2, unsafeWrap3
                                   )
