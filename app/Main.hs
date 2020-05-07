-- dürfte ich bei Ihnen 2 Cappuccinos bestellen?
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (div)
import Data.Text (Text, pack)

import Element

-- "<div data-qdid=\"0\"></div>
-- <div data-qdid=\"1\"></div>
-- <div data-qdid=\"2\"></div>
-- <div data-qdid=\"3\"></div>
-- <div data-anchor data-qdid=\"4\"></div>
-- <div data-qdid=\"5\"></div>
-- <div data-qdid=\"6\"></div>
-- <div data-qdid=\"7\"></div>
-- <div data-qdid=\"8\"></div>"

div :: Int -> Element
div i = Element
    { content = EData
        { name = "div"
        , attributes = [("data-qdid", Just [pack $ show i])]
                    <> if i == 4 then [("data-anchor", Nothing)] else []
        }
    , children = []
    }

puzzle :: Element
puzzle = Element
    { content = EData { name = "div", attributes = [] }
    , children = div <$> [0..8]
    }

pretty :: Element -> Text
pretty = go 0
  where
    go level Element {content, children} =
        pack (replicate level ' ')
        <> Element.name content
        <> pack (show $ attributes content)
        <> "\n"
        <> foldMap (go (level + 1)) children

main :: IO ()
main = pure ()
