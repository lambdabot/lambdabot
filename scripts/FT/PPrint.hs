-----------------------------------------------------------
-- Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--
-- $version: $
--
-- Pretty print module based on Philip Wadlers "prettier printer"
--      "A prettier printer"
--      Draft paper, April 1997, revised March 1998.
--      http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps
--
-- Haskell98 compatible
-----------------------------------------------------------
module PPrint
        ( Doc
        , Pretty, pretty

        , show, putDoc, hPutDoc

        , (<>)
        , (<+>)
        , (</>), (<//>)
        , (<$>), (<$$>)

        , sep, fillSep, hsep, vsep
        , cat, fillCat, hcat, vcat
        , punctuate

        , align, hang, indent
        , fill, fillBreak

        , list, tupled, semiBraces, encloseSep
        , angles, langle, rangle
        , parens, lparen, rparen
        , braces, lbrace, rbrace
        , brackets, lbracket, rbracket
        , dquotes, dquote, squotes, squote

        , comma, space, dot, backslash
        , semi, colon, equals

        , string, bool, int, integer, float, double, rational

        , softline, softbreak
        , empty, char, text, line, linebreak, nest, group
        , column, nesting, width

        , SimpleDoc(..)
        , renderPretty, renderCompact
        , displayS, displayIO
        ) where

import IO      (Handle,hPutStr,hPutChar,stdout)

infixr 5 </>,<//>,<$>,<$$>
infixr 6 <>,<+>


-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------
list            = encloseSep lbracket rbracket comma
tupled          = encloseSep lparen   rparen  comma
semiBraces      = encloseSep lbrace   rbrace  semi

encloseSep left right sep ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right)


-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------
sep             = group . vsep
fillSep         = fold (</>)
hsep            = fold (<+>)
vsep            = fold (<$>)

cat             = group . vcat
fillCat         = fold (<//>)
hcat            = fold (<>)
vcat            = fold (<$$>)

fold f []       = empty
fold f ds       = foldr1 f ds

x <> y          = x `beside` y
x <+> y         = x <> space <> y
x </> y         = x <> softline <> y
x <//> y        = x <> softbreak <> y
x <$> y         = x <> line <> y
x <$$> y        = x <> linebreak <> y

softline        = group line
softbreak       = group linebreak

squotes         = enclose squote squote
dquotes         = enclose dquote dquote
braces          = enclose lbrace rbrace
parens          = enclose lparen rparen
angles          = enclose langle rangle
brackets        = enclose lbracket rbracket
enclose l r x   = l <> x <> r

lparen          = char '('
rparen          = char ')'
langle          = char '<'
rangle          = char '>'
lbrace          = char '{'
rbrace          = char '}'
lbracket        = char '['
rbracket        = char ']'

squote          = char '\''
dquote          = char '"'
semi            = char ';'
colon           = char ':'
comma           = char ','
space           = char ' '
dot             = char '.'
backslash       = char '\\'
equals          = char '='


-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"
string ""       = empty
string ('\n':s) = line <> string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs <> string ys

bool :: Bool -> Doc
bool b          = text (show b)

int :: Int -> Doc
int i           = text (show i)

integer :: Integer -> Doc
integer i       = text (show i)

float :: Float -> Doc
float f         = text (show f)

double :: Double -> Doc
double d        = text (show d)

rational :: Rational -> Doc
rational r      = text (show r)


-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------
class Pretty a where
  pretty        :: a -> Doc
  prettyList    :: [a] -> Doc
  prettyList    = list . map pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList

instance Pretty Doc where
  pretty        = id

instance Pretty () where
  pretty ()     = text "()"

instance Pretty Bool where
  pretty b      = bool b

instance Pretty Char where
  pretty c      = char c
  prettyList s  = string s

instance Pretty Int where
  pretty i      = int i

instance Pretty Integer where
  pretty i      = integer i

instance Pretty Float where
  pretty f      = float f

instance Pretty Double where
  pretty d      = double d


--instance Pretty Rational where
--  pretty r      = rational r

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x



-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------
fillBreak f x   = width x (\w ->
                  if (w > f) then nest f linebreak
                             else text (spaces (f - w)))

fill f d        = width d (\w ->
                  if (w >= f) then empty
                              else text (spaces (f - w)))

width d f       = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------
indent i d      = hang i (text (spaces i) <> d)

hang i d        = align (nest i d)

align d         = column (\k ->
                  nesting (\i -> nest (k - i) d))   --nesting might be negative :-)



-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------
data Doc        = Empty
                | Char Char             -- invariant: char is not '\n'
                | Text !Int String      -- invariant: text doesn't contain '\n'
                | Line !Bool            -- True <=> when undone by group, do not insert a space
                | Cat Doc Doc
                | Nest !Int Doc
                | Union Doc Doc         -- invariant: first lines of first doc longer than the first lines of the second doc
                | Column  (Int -> Doc)
                | Nesting (Int -> Doc)

data SimpleDoc  = SEmpty
                | SChar Char SimpleDoc
                | SText !Int String SimpleDoc
                | SLine !Int SimpleDoc


empty           = Empty

char '\n'       = line
char c          = Char c

text ""         = Empty
text s          = Text (length s) s

line            = Line False
linebreak       = Line True

beside x y      = Cat x y
nest i x        = Nest i x
column f        = Column f
nesting f       = Nesting f
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line break)    = if break then Empty else Text 1 " "
flatten (Union x y)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text



-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs   = Nil
            | Cons !Int Doc Docs

renderPretty :: Float -> Int -> Doc -> SimpleDoc
renderPretty rfrac w x
    = best 0 0 (Cons 0 x Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best n k Nil      = SEmpty
      best n k (Cons i d ds)
        = case d of
            Empty       -> best n k ds
            Char c      -> let k' = k+1 in seq k' (SChar c (best n k' ds))
            Text l s    -> let k' = k+l in seq k' (SText l s (best n k' ds))
            Line _      -> SLine i (best i i ds)
            Cat x y     -> best n k (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in seq i' (best n k (Cons i' x ds))
            Union x y   -> nicest n k (best n k (Cons i x ds))
                                      (best n k (Cons i y ds))

            Column f    -> best n k (Cons i (f k) ds)
            Nesting f   -> best n k (Cons i (f i) ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x y    | fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)


fits w x        | w < 0         = False
fits w SEmpty                   = True
fits w (SChar c x)              = fits (w - 1) x
fits w (SText l s x)            = fits (w - l) x
fits w (SLine i x)              = True


-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------
renderCompact :: Doc -> SimpleDoc
renderCompact x
    = scan 0 [x]
    where
      scan k []     = SEmpty
      scan k (d:ds) = case d of
                        Empty       -> scan k ds
                        Char c      -> let k' = k+1 in seq k' (SChar c (scan k' ds))
                        Text l s    -> let k' = k+l in seq k' (SText l s (scan k' ds))
                        Line _      -> SLine 0 (scan 0 ds)
                        Cat x y     -> scan k (x:y:ds)
                        Nest j x    -> scan k (x:ds)
                        Union x y   -> scan k (y:ds)
                        Column f    -> scan k (f k:ds)
                        Nesting f   -> scan k (f 0:ds)



-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------
displayS :: SimpleDoc -> ShowS
displayS SEmpty             = id
displayS (SChar c x)        = showChar c . displayS x
displayS (SText l s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':indentation i) . displayS x

displayIO :: Handle -> SimpleDoc -> IO ()
displayIO handle simpleDoc
    = display simpleDoc
    where
      display SEmpty        = return ()
      display (SChar c x)   = do{ hPutChar handle c; display x}
      display (SText l s x) = do{ hPutStr handle s; display x}
      display (SLine i x)   = do{ hPutStr handle ('\n':indentation i); display x}


-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show Doc where
  showsPrec d doc       = displayS (renderPretty 0.4 80 doc)

putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc      = displayIO handle (renderPretty 0.4 80 doc)



-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '

indentation n   = spaces n

--indentation n   | n >= 8    = '\t' : indentation (n-8)
--                | otherwise = spaces n
