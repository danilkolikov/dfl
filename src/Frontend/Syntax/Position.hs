{- |
Module      :  Frontend.Syntax.Position
Description :  Position tracking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Code for tracking of locations in source files.
-}
module Frontend.Syntax.Position
    ( SourcePosition(..)
    , SourceLocation(..)
    , WithLocation(..)
    , castSourcePosition
    , dummyLocation
    , withDummyLocation
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

-- | Cast 'SourcePos' to 'SourcePosition'
castSourcePosition :: SourcePos -> SourcePosition
castSourcePosition src =
    SourcePosition (unPos . sourceLine $ src) (unPos . sourceColumn $ src)

-- | Function gets source position during parsing
getSourcePosition :: MonadParsec e s m => m SourcePosition
getSourcePosition = castSourcePosition <$> getSourcePos

-- | Function constructs 'SourceLocation' from 4 provided ints
sourceLocation :: Int -> Int -> Int -> Int -> SourceLocation
sourceLocation a b c d =
    SourceLocation (SourcePosition a b) (SourcePosition c d)

-- | Dummy location
dummyLocation :: SourceLocation
dummyLocation = sourceLocation 0 0 0 0

-- | Add a dummy location to an object
withDummyLocation :: a -> WithLocation a
withDummyLocation = (`WithLocation` dummyLocation)

-- | Type representing an object with a location in a source file
data WithLocation a = WithLocation
    { getValue :: a -- ^ Object
    , getLocation :: SourceLocation -- ^ Corresponding location
    } deriving (Eq, Ord)

instance (Show a) => Show (WithLocation a) where
    show = show . getValue

instance Functor WithLocation where
    fmap f (WithLocation x loc) = WithLocation (f x) loc

instance Foldable WithLocation where
    foldMap = (. getValue)

instance Traversable WithLocation where
    traverse f (WithLocation x loc) = fmap (`WithLocation` loc) (f x)
