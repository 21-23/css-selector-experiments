{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NamedFieldPuns #-}

module Element where

import Prelude hiding (words)
import Data.Char (isAlpha, isAscii)
import Data.Functor (void)
import Control.Monad (join)

import Data.Text (Text, pack, words)
import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing (Parser)

type Attr = (Text, Maybe [Text])

data ElementData = EData
  { name :: Text
  , attributes :: [Attr]
  } deriving Show

data Element = Element
  { content    :: ElementData
  , children   :: [Element]
  } deriving Show

attribute :: Parser Attr
attribute = do
  name <- pack <$> some (satisfy isAlpha)
  mValue <- optional $ do
    void "="
    value <- pack <$> (try (some (satisfy isAlpha)) <|> "\"" *> manyTill (satisfy isAscii) "\"")
    pure $ words value
  pure (name, mValue)

textContent :: Parser Text
textContent = pack <$> many (satisfy $ \c -> isAscii c && c /= '<')

element :: Parser Element
element = do
  void "<"
  name <- pack <$> some (satisfy isAlpha)
  space
  attrs <- many (try attribute <* space)
  let eData = EData name attrs
      singleTag = do
        void "/"
        space
        void ">"
        pure $ Element eData []
      twoTags = do
        void ">"
        children <- many $ try element
        void "</"
        void $ string name
        void ">"
        pure $ Element eData children
  try singleTag <|> twoTags

getAttrValue :: Text -> Element -> Maybe [Text]
getAttrValue attrName Element { content = EData { attributes } } =
  join $ lookup attrName attributes

data Crumb = Crumb
  { elemContent    :: ElementData
  , siblingsBefore :: [Element]
  , siblingsAfter  :: [Element]
  } deriving Show

type Zipper = (Element, [Crumb])

toZipper :: Element -> Zipper
toZipper element = (element, [])

getElement :: Zipper -> Element
getElement = fst

firstChild :: Zipper -> Maybe Zipper
firstChild (Element { children = [] }, _) = Nothing
firstChild (Element { content, children = first : cs }, crumbs) =
  Just (first, Crumb content [] cs : crumbs)

nextSibling :: Zipper -> Maybe Zipper
nextSibling (_, []              ) = Nothing
nextSibling (_, Crumb _ _ [] : _) = Nothing
nextSibling (e, (Crumb content ls (e' : rs)) : crumbs) =
  Just (e', Crumb content (e : ls) rs : crumbs)

zipperFold :: (a -> Zipper -> a) -> a -> Zipper -> a
zipperFold f z zipper = go child sibling
  where
    go Nothing  Nothing  = z'
    go Nothing  (Just s) = zipperFold f z' s
    go (Just c) Nothing  = zipperFold f z' c
    go (Just c) (Just s) = zipperFold f (zipperFold f z' c) s
    child                = firstChild zipper
    sibling              = nextSibling zipper
    z'                   = f z zipper

zipperFoldM :: Monad m => (a -> Zipper -> m a) -> a -> Zipper -> m a
zipperFoldM f z = zipperFold f' (pure z)
  where
    f' ma zipper = ma >>= flip f zipper

parent :: Zipper -> Maybe Zipper
parent (_, []) = Nothing
parent (element, Crumb content ls rs : crumbs) =
  Just (Element content (reverse ls ++ element : rs), crumbs)

gotoRoot :: Zipper -> Zipper
gotoRoot zipper = maybe zipper gotoRoot $ parent zipper
