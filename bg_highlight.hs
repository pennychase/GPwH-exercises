{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Chapter 24 
-- Highlighting queried text, including Unicode, with Data.Text
--
dharma :: Text
dharma = "धर्म"

bgText :: Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।\nस्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: Text -> Text -> Text
highlight query fullText = T.intercalate highlighted pieces
    where
        pieces = T.splitOn query fullText
        highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = do
    TIO.putStrLn $ highlight dharma bgText