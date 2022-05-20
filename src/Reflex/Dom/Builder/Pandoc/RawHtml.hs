{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-|
  Description:
    Helper functions to convert raw inline html elements
    into sanitized Pandoc AST nodes.
-}
module Reflex.Dom.Builder.Pandoc.RawHtml where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Text.HTML.Parser as Html

-- | We overload the 'Span' node to store inline html nodes of various
-- types. It appears as though pandoc itself does the same.  For example,
-- the html @<mark>a</mark>@ and @<span class="mark">a</span>@ appear to
-- produce the same AST when parsed.
overloadedSpanId :: Text
overloadedSpanId = "dombuilder-pandoc-overloaded-span"

-- | Whitelist-based sanitization. For use with 'Text.Pandoc.Walk.walk'.
-- Most attributes are stripped from the output. Allowed elements:
--
-- @
--"a", "abbr", "acronym", "code", "del", "dfn", "em", "i", "img",
-- "ins", "kbd", "mark", "q", "s", "samp", "small", "span",
-- "strong", "sub", "sup", "time", "u", "var"
-- @
--
-- Note that this function (mis)uses the 'Span' node type to store details
-- of the inline html node that will eventually be produced.
inlineHtmlSanitizer :: [Inline] -> [Inline]
inlineHtmlSanitizer rs = case rs of
  (r@(Inline (RawInline "html" tag)) : xs) -> case Html.parseTokens tag of
    [Html.TagSelfClose t atts] -> case Map.lookup t allowedInlineHtml of
      Nothing -> r : sanitizeInlineHtml xs
      Just allowed -> Inline (Span (overloadedSpanId, [t], allowedAttrs atts allowed) []) :
        sanitizeInlineHtml xs
    [Html.TagOpen t atts] -> case Map.lookup t allowedInlineHtml of
      Nothing -> r : sanitizeInlineHtml xs
      Just allowed -> case break (isClosingTagOf t) xs of
        (contents, _closing:rest) ->
          Inline (Span (overloadedSpanId, [t], allowedAttrs atts allowed) contents) :
            sanitizeInlineHtml rest
        _ -> r : sanitizeInlineHtml xs
    _ -> r : sanitizeInlineHtml xs
  (x:xs) -> x : sanitizeInlineHtml xs
  _ -> rs
  where
    isClosingTagOf :: Html.TagName -> Inline -> Bool
    isClosingTagOf t = \case
      Inline (RawInline "html" raw) -> case Html.parseTokens raw of
        [Html.TagClose t'] -> t == t'
        _ -> False
      _ -> False
    allowedAttrs :: [Html.Attr] -> [Text] -> [(Text, Text)]
    allowedAttrs attrs allowed = flip mapMaybe attrs $ \(Html.Attr k v) ->
      if k `elem` allowed
        then Just (k, v)
        else Nothing
    allowedInlineHtml = Map.fromList
      [ ("a", ["title", "href"])
      , ("abbr", ["title"])
      , ("acronym", ["title"])
      , ("code", ["title"])
      , ("del", ["title"])
      , ("dfn", ["title"])
      , ("em", [])
      , ("i", [])
      , ("img", ["width", "height", "title", "alt", "align"])
      , ("ins", ["title"])
      , ("kbd", ["title"])
      , ("mark", ["title"])
      , ("q", ["title"])
      , ("s", ["title"])
      , ("samp", ["title"])
      , ("small", [])
      , ("span", [])
      , ("strong", [])
      , ("sub", [])
      , ("sup", [])
      , ("time", ["title", "datetime"])
      , ("u", [])
      , ("var", ["title"])
      ]

-- | Translate the raw inline html in a 'Pandoc' document to overloaded
-- spans
sanitizeInlineHtml :: Walkable [Inline] b => b -> b
sanitizeInlineHtml = walk inlineHtmlSanitizer
