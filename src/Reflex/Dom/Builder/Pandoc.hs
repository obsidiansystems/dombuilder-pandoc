{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-|
Description: Convert pandoc documents to reflex dom widgets
|-}
module Reflex.Dom.Builder.Pandoc
  ( block, blockF
  , inline, inlineF
  , metaTable
  , metaMap, metaMapF
  , metaValue, metaValueF
  ) where

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Space, Link)
import Text.Pandoc.Definition

import Reflex.Dom.Builder.Pandoc.RawHtml

attr :: Attr -> Map Text Text
attr (ident, classes, other) = Map.fromListWith (\a b -> a <> " " <> b) $
  [ ("id", ident)
  , ("class", T.intercalate " " classes)
  ] <> other

block :: DomBuilder t m => Block -> m ()
block = blockF inline block . unBlock

blockF
  :: forall m t inline block
  .  DomBuilder t m
  => (inline -> m ())
  -> (block -> m ())
  -> BlockF inline block
  -> m ()
blockF inline' block' = \case
  Plain xs -> mapM_ inline' xs
  Para xs -> el "p" $ mapM_ inline' xs
  LineBlock xss -> el "p" $ sequence_ $
    intercalate [lineBreak] $ (fmap . fmap) inline' xss
  CodeBlock a code -> el "pre" $ elAttr "code" (attr a) $ text code
  RawBlock (Format f) t -> -- TODO: decide how to handle raw blocks
    elAttr "pre" ("class" =: "raw" <> "format" =: f) $ text t
  BlockQuote xs -> el "blockquote" $ mapM_ block' xs
  OrderedList (start, numStyle, _numDelim) xss -> -- TODO: use specified number delimiter
    elAttr "ol" (listStyle numStyle <> "start" =: T.pack (show start)) $
      mapM_ (el "li" . mapM_ block') xss
  BulletList xss -> el "ul" $ mapM_ (el "li" . mapM_ block') xss
  DefinitionList xs -> el "dl" $ forM_ xs $ \(t, d) -> do
    el "dt" $ mapM_ inline' t
    mapM_ (el "dd" . mapM_ block') d
  Header lvl a xs -> elAttr ("h" <> T.pack (show lvl)) (attr a) $
    mapM_ inline' xs
  HorizontalRule -> el "hr" $ pure ()
  Table a caption _colSpecs (TableHead hattrs hrows) tbody (TableFoot fattrs frows) ->
    -- TODO: format columns
    -- TODO: format cells
    -- TODO: handle intermediate table heads in body
    elAttr "table" (attr a) $ do
      el "caption" $ case caption of
        (Caption (Just short) xs) -> do
          mapM_ inline' short
          mapM_ block' xs
        (Caption Nothing xs) -> mapM_ block' xs
      let mkRow cell (Row ra cs) = elAttr "tr" (attr ra) $
            mapM_ (\(Cell ca _align _rowSpan _colSpan ys) ->
              elAttr cell (attr ca) $ mapM_ block' ys) cs
      elAttr "thead" (attr hattrs) $ mapM_ (mkRow "th") hrows
      forM_ tbody $ \(TableBody ba _rowHead _rowHeadCells cs) ->
        elAttr "tbody" (attr ba) $ mapM_ (mkRow "td") cs
      elAttr "tfoot" (attr fattrs) $ mapM_ (mkRow "td") frows
  Div a xs -> elAttr "div" (attr a) $ mapM_ block' xs
  Null -> blank
  where
    listStyle = \case
      DefaultStyle -> mempty
      Example -> mempty
      Decimal -> listStyleType "decimal"
      LowerRoman -> listStyleType "lower-roman"
      UpperRoman -> listStyleType "upper-roman"
      LowerAlpha -> listStyleType "lower-alpha"
      UpperAlpha -> listStyleType "upper-alpha"
    listStyleType :: Text -> Map Text Text
    listStyleType a = "style" =: ("list-style-type: " <> a <> ";")

inline :: DomBuilder t m => Inline -> m ()
inline = inlineF block inline . unInline

inlineF
  :: DomBuilder t m
  => (block -> m ())
  -> (inline -> m ())
  -> InlineF block inline -> m ()
inlineF block' inline' = \case
  Str x -> text x
  Emph xs -> el "em" $ mapM_ inline' xs
  Strong xs -> el "strong" $ mapM_ inline' xs
  Strikeout xs -> el "s" $ mapM_ inline' xs
  Superscript xs -> el "sup" $ mapM_ inline' xs
  Subscript xs -> el "sub" $ mapM_ inline' xs
  SmallCaps xs -> elAttr "span" ("style" =: "font-variant: small-caps;") $
    mapM_ inline' xs
  Quoted _qtype xs -> -- TODO: use quote type
    el "q" $ mapM_ inline' xs
  Cite _citations xs -> -- TODO: use citation meta
    el "cite" $ mapM_ inline' xs
  Code a t -> elAttr "code" (attr a) $ text t
  Space -> text " "
  SoftBreak -> text "\n"
  LineBreak -> lineBreak
  Math mathType t -> case mathType of
    DisplayMath -> elClass "pre" "math" $ text t
    InlineMath -> elClass "code" "math" $ text t
  RawInline (Format f) t -> -- TODO: decide how to handle raw inline'
    renderRaw f t
  Link a xs target ->
    elAttr "a" (attr a <> "href" =: fst target <> "title" =: snd target) $
      mapM_ inline' xs
  Image a xs target ->
    elAttr "img" (attr a <> "src" =: fst target <> "title" =: snd target) $
      mapM_ inline' xs
  Note xs -> el "comment" $ mapM_ block' xs
  Span a xs -> case a of
    (aid, [tagName], attrs) | aid == overloadedSpanId ->
      elAttr tagName (attr ("", [], attrs)) $ mapM_ inline' xs
    _ -> elAttr "span" (attr a) $ mapM_ inline' xs
  Underline xs -> el "u" $ mapM_ inline' xs
  where
    renderRaw f =
      elAttr "code" ("class" =: "raw" <> "format" =: f) . text

lineBreak :: DomBuilder t m => m ()
lineBreak = el "br" blank

metaTable :: DomBuilder t m => Meta -> m ()
metaTable (Meta mm) = metaMap mm

metaMap :: DomBuilder t m => Map Text MetaValue -> m ()
metaMap = metaMapF metaValue

metaMapF :: DomBuilder t1 m => (metaValue -> m ()) -> Map Text metaValue -> m ()
metaMapF metaValue' x = el "table" $ el "tbody" $
  forM_ (Map.toList x) $ \(h, v) -> el "tr" $ do
    el "th" $ text h
    el "td" $ metaValue' v

metaValue :: DomBuilder t m => MetaValue -> m ()
metaValue = metaValueF inline block metaValue . unMetaValue

metaValueF
  :: DomBuilder t m
  => (inline -> m ())
  -> (block -> m ())
  -> (metaValue -> m ())
  -> MetaValueF inline block metaValue -> m ()
metaValueF inline' block' metaValue' = \case
        MetaMap m -> metaMapF metaValue' m
        MetaList m -> sequence_ $
          intersperse (text ", ") $ metaValue' <$> m
        MetaBool m -> text $ T.pack $ show m
        MetaString m -> text m
        MetaInlines m -> mapM_ inline' m
        MetaBlocks m -> mapM_ block' m
