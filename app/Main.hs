-- dürfte ich bei Ihnen 2 Cappuccinos bestellen?
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (div)
import Data.List (foldl')
import Data.Text (Text, pack)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Selector

type Attr = (Text, Maybe [Text])

data ElementData = EData
  { name :: Text
  , attributes :: [Attr]
  } deriving Show

data Element = Element
  { content    :: ElementData
  , children   :: [Element]
  } deriving Show

data Crumb = Crumb
  { content        :: ElementData
  , siblingsBefore :: [Element]
  , siblingsAfter  :: [Element]
  } deriving Show

type Zipper = (Element, [Crumb])

toZipper :: Element -> Zipper
toZipper element = (element, [])

getElement :: Zipper -> Element
getElement = fst

firstChild :: Zipper -> Maybe Zipper
firstChild (Element {children = []}, _) = Nothing
firstChild (Element {content, children = first : cs}, crumbs)
  = Just (first, Crumb content [] cs : crumbs)

nextSibling :: Zipper -> Maybe Zipper
nextSibling (_, []) = Nothing
nextSibling (_, Crumb _ _ [] : _) = Nothing
nextSibling (e, (Crumb content ls  (e' : rs)) : crumbs)
  = Just (e', Crumb content (e : ls) rs : crumbs)

foldElement :: (a -> Element -> a) -> a -> Element -> a
foldElement f z e@(Element _ cs) = foldl' (foldElement f) (f z e) cs

zipperFold :: (a -> Zipper -> a) -> a -> Zipper -> a
zipperFold f z zipper = go child sibling
    where
      go Nothing   Nothing = z'
      go Nothing  (Just s) = zipperFold f z' s
      go (Just c)  Nothing = zipperFold f z' c
      go (Just c) (Just s) = zipperFold f (zipperFold f z' c) s
      child = firstChild zipper
      sibling = nextSibling zipper
      z' = f z zipper

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
                <> name content
                <> pack (show $ attributes content)
                <> "\n"
                <> foldMap (go (level + 1)) children

parent :: Zipper -> Maybe Zipper
parent (_, []) = Nothing
parent (element, Crumb content ls rs : crumbs) =
  Just (Element content (reverse ls ++ element : rs), crumbs)

matchAncestor :: Selector -> Zipper -> Maybe Element
matchAncestor selector zipper = do
  parentZipper <- parent zipper
  match selector parentZipper <|> matchAncestor selector parentZipper

matchPreceding :: Selector -> Zipper -> Maybe Element
matchPreceding selector zipper = do
  siblingZipper <- nextSibling zipper
  match selector siblingZipper <|> matchPreceding selector siblingZipper

mandr :: Maybe a -> Maybe b -> Maybe b
mandr Nothing _ = Nothing
mandr (Just _) x = x

eqAttr :: Attr -> Attr -> Bool
eqAttr (nameA, Nothing) (nameB, _) = nameA == nameB
eqAttr _ (_, Nothing) = False
eqAttr (nameA, Just valueA) (nameB, Just valueB) = nameA == nameB && valueA == valueB

match :: Selector -> Zipper -> Maybe Element
match AnyElement (element, _) = pure element
match (Elem elName) (element@Element {content = EData {name}}, _) = do
  guard $ name == elName
  pure element
match (Class className) (element@Element {content = EData {attributes}}, _) = do
  classAttr <- lookup "class" attributes
  classList <- classAttr
  guard $ elem className classList
  pure element
match (Attribute name mValue) (element@Element {content = EData {attributes}}, _) = do
  guard $ any (eqAttr (name, mValue)) attributes
  pure element
match (Child parentSelector selector) zipper = do
  parentZipper <- parent zipper
  match parentSelector parentZipper `mandr` match selector zipper
match (Descendant ancestorSelector selector) zipper =
  matchAncestor ancestorSelector zipper `mandr` match selector zipper
match (ImmediateSibling siblingSelector selector) zipper = do
  siblingZipper <- nextSibling zipper
  match siblingSelector siblingZipper `mandr` match selector zipper
match (Sibling siblingSelector selector) zipper =
  matchPreceding siblingSelector zipper `mandr` match selector zipper
match (And selectorA selectorB) zipper =
  match selectorA zipper `mandr` match selectorB zipper

matchTree :: Selector -> Zipper -> [Element]
matchTree selector =
  fromMaybe [] . zipperFold (\xs zipper -> ((:) <$> match selector zipper <*> xs) <|> xs) (Just [])

main :: IO ()
main = pure ()
