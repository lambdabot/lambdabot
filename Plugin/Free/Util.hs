module Plugin.Free.Util (
    Pretty(..),
    prettyParen,
    prettyParenIndent,
    module Text.PrettyPrint.HughesPJ
) where


import Text.PrettyPrint.HughesPJ


class Pretty a where
    prettyP :: Int -> a -> Doc

    pretty :: a -> Doc
    pretty x = prettyP 0 x


prettyParen :: Bool -> Doc -> Doc
prettyParen b doc
    = if b then parens doc else doc

prettyParenIndent :: Bool -> Doc -> Doc
prettyParenIndent b doc
    = if b
      then vcat [lparen, nest 2 doc, rparen]
      else doc


-- vim: ts=4:sts=4:expandtab
