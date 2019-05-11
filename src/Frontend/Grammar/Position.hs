{- |
Module      :  Frontend.Grammar.Position
Description :  Position tracking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Code for tracking of locations in source files.
-}
module Frontend.Grammar.Position
    ( SourcePosition(..)
    , SourceLocation(..)
    , WithLocation(..)
    , dummyLocation
    , getSourcePosition
    , sourceLocation
    ) where

import Data.Functor (Functor)
import Text.Megaparsec (MonadParsec, getSourcePos)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)

-- | Type describing a position in the source file (line and column)
data SourcePosition = SourcePosition
    { getSourceLine :: Int -- ^ Corresponding line in a source file
    , getSourceColumn :: Int -- ^ Corresponding column in a source file
    } deriving (Show, Eq, Ord)

-- | Type describing a "span" between two locations in the source file
data SourceLocation = SourceLocation
    { getLocationStart :: SourcePosition -- ^ Start of a span
    , getLocationEnd :: SourcePosition -- ^ End of a span
    } deriving (Show, Eq, Ord)

-- | Function gets source position during parsing
getSourcePosition :: MonadParsec e s m => m SourcePosition
getSourcePosition = castSourcePositions <$> getSourcePos
  where
    castSourcePositions :: SourcePos -> SourcePosition
    castSourcePositions src =
        SourcePosition (unPos . sourceLine $ src) (unPos . sourceColumn $ src)

-- | Function constructs 'SourceLocation' from 4 provided ints
sourceLocation :: Int -> Int -> Int -> Int -> SourceLocation
sourceLocation a b c d =
    SourceLocation (SourcePosition a b) (SourcePosition c d)

-- | Dummy location
dummyLocation :: SourceLocation
dummyLocation = sourceLocation 0 0 0 0

-- | Type representing an object with a location in a source file
data WithLocation a = WithLocation
    { getValue :: a -- ^ Object
    , getLocation :: SourceLocation -- ^ Corresponding location
    } deriving (Show, Eq, Ord)

instance Functor WithLocation where
    fmap f (WithLocation x loc) = WithLocation (f x) loc
