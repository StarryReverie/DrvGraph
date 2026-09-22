module DrvGraph.Core.Export.Tree.Render
    ( renderEntryLine
    ) where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import System.Console.ANSI (ConsoleLayer (..), setSGRCode)
import System.Console.ANSI.Codes (SGR (..))

import DrvGraph.Core.Export.Tree.Flatten (EntryLine (..), EntryLineColor (..))

renderEntryLine :: Bool -> EntryLine -> Text
renderEntryLine reversed EntryLine{isSubtreeLastChild, content} =
    Text.concat (renderAncestor <$> reverse (drop 1 isSubtreeLastChild))
        <> maybe "" renderConnector (Maybe.listToMaybe isSubtreeLastChild)
        <> Text.intercalate " " (renderChunk <$> content)
  where
    renderAncestor True = "   "
    renderAncestor False = "│  "

    renderConnector True = if reversed then "┌─ " else "└─ "
    renderConnector False = "├─ "

    renderChunk :: (Text, EntryLineColor) -> Text
    renderChunk (chunk, EntryLineColor{color, intensity}) =
        Text.pack (setSGRCode [SetColor Foreground intensity color])
            <> chunk
            <> Text.pack (setSGRCode [Reset])
