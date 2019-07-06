{- |
Module      :  Compiler.Prettify.PrettyPrinter
Description :  Pretty printer
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Pretty printer supporting custom indentation
-}
module Compiler.Prettify.PrettyPrinter where

import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE

-- | Multiple lines
type Lines = NE.NonEmpty String

-- | Object which does pretty printing
type PrettyPrinter = Reader Int Lines

-- | Run pretty printer
runPrettyPrinter :: PrettyPrinter -> Int -> String
runPrettyPrinter printer = unlines . NE.toList . runReader printer

-- | Return single line
singleLine :: String -> PrettyPrinter
singleLine = return . singleLine'

emptyLine :: PrettyPrinter
emptyLine = singleLine ""

newLine :: PrettyPrinter
newLine = singleLine "\n"

-- | Make single line
singleLine' :: String -> Lines
singleLine' line = line NE.:| []

-- | Return muliple lines
multipleLines :: [Lines] -> PrettyPrinter
multipleLines = return . multipleLines'

-- | Make multiple lines
multipleLines' :: [Lines] -> Lines
multipleLines' [] = singleLine' ""
multipleLines' ((first NE.:| rest):restLists) =
    first NE.:| (rest ++ concatMap NE.toList restLists)

-- | Join two lists of lines
join :: Lines -> Lines -> Lines
left `join` (rightStart NE.:| rightEnd) =
    let leftStart = NE.init left
        leftEnd = NE.last left
        joined = leftEnd ++ rightStart
     in NE.fromList $ leftStart ++ (joined : rightEnd)

-- | Return joined lines
joinLines :: [Lines] -> PrettyPrinter
joinLines = return . joinLines'

-- | Join list of lines
joinLines' :: [Lines] -> Lines
joinLines' [] = singleLine' ""
joinLines' t = foldl1 join t

-- | Join results of printers
joinPrinters :: [PrettyPrinter] -> PrettyPrinter
joinPrinters printers =
    sequence (intersperse (singleLine " ") printers) >>= joinLines

-- | Join results on multiple lines
multiplePrinters :: [PrettyPrinter] -> PrettyPrinter
multiplePrinters printers = sequence printers >>= multipleLines

-- | Run printer with increased indentation
withIndent :: PrettyPrinter -> PrettyPrinter
withIndent printer = do
    defaultIndent <- ask
    let prependIndent str = replicate defaultIndent ' ' ++ str
    fmap prependIndent <$> printer
