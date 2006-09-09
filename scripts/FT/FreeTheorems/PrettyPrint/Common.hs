-- Copyright 2006, Sascha Boehme.




-- | This module gives common pretty-printing functions shared by various output
--   formats. It is heavily based 'Document' to let the output look uniformly
--   for different output formats and pretty printers.

module FreeTheorems.PrettyPrint.Common (
    docNamedType,
    docType,
    docTheorem,
    docUnfoldedRelation
) where



import FreeTheorems.Types
import FreeTheorems.PrettyPrint.Document
import Control.Monad (liftM)
import Data.Maybe (maybeToList)





-- | Creates a document from a named type.

docNamedType :: Document a => NamedType -> a
docNamedType (NamedType tv t) =
  docTermVariable tv <+> docColon <> docColon <+> align (docType False t)



-- | Creates a document from a type. The second parareter is used to switch
--   between a compact notation ('True') and a better readable notation
--   ('False').

docType :: Document a => Bool -> Type -> a
docType = docTypeP id



-- | Creates a document from a type. This is just a helper function for
--   'docTypeP'.
--
--   The first argument should either be 'id' or 'parens'. It is
--   used to put parentheses around certain parts of types depending on their
--   position. It effects only functions and type abstractions.
--
--   The second argument is used to switch between a compact notation ('True')
--   and a better readable notation ('False'). If turned off with False, it
--   breaks long functions into several lines.

docTypeP :: Document a => (a -> a) -> Bool -> Type -> a
docTypeP useParens compact t =
  case t of
    TypeBasic b    -> docBasicType b

    TypeVar v      -> docTypeVariable v

    TypeTermVar tv -> docTypeTermVariable tv

    TypeCon c ts   -> let docts = map (docTypeP parens compact) ts
                          doc   = fillSep (docTypeConstructor c : docts)
                      in  if ts == []
                            then doc
                            else parens doc

    TypeList t'    -> brackets (docTypeP id compact t')

    TypeUnit       -> parens empty

    TypeTuple ts   -> tupled (map (docTypeP id compact) ts)

    TypeFun t1 t2  -> let ts = collectFun t2
                          t2d = (\t -> docArrow <.> docTypeP parens compact t)
                          docts = map t2d ts
                          docts' = (docTypeP parens compact t1) : docts
                      in  if compact
                            then useParens (fillSoft docts')
                            else useParens (catSoft docts')

    TypeForall _ _ -> let (vs,t') = collectForall t
                          docvs = fillSoft (map docTypeVariable vs)
                          doct' = docTypeP id compact t'
                          doc   = docForall <.> docvs <> docDot <.> align doct'
                      in  useParens doc

  where
    -- Collects all types being part of a function.
    collectFun t =
      case t of
        TypeFun t1 t2 -> t1 : (collectFun t2)
        otherwise     -> [t]

    -- Collects variables of consecutive type abstractions.
    collectForall t =
      case t of
        TypeForall v t1 -> (v:vs, t2) where (vs,t2) = (collectForall t1)
        otherwise       -> ([], t)



--------------------------------------------------------------------------------



-- | Creates a document from a theorem.
--   To make theorems better readable, every quantification indents its
--   subtheorem a bit. Otherwise, the parts of a theorem are written on a line,
--   possibly continuing in the next line.

docTheorem :: Document a => Theorem -> a


docTheorem (IsElementOf (t1,t2) rel) =
  let doct1  = docTerm t1
      doct2  = docTerm t2
  in  case rel of
        RelTerm (TermVar (PV "id")) _ -> fillSoft [doct1, docEqual, doct2]

        RelTerm (TermIns (TermVar (PV "id")) _) _ -> fillSoft [doct1, docEqual,
                                                               doct2]

        RelTerm t _ -> fillSoft [docTerm (TermApp t t1), docEqual, doct2]

        otherwise -> let docrel = case rel of
                                    RelForall _ _ _ -> parens $ docRelation rel
                                    RelFun _ _ _    -> parens $ docRelation rel
                                    otherwise       -> docRelation rel
                     in  fillSoft [tupled [doct1, doct2], docIn, docrel]


docTheorem (ForallPairs (tv1,tv2) rel theorem) =
  let doctv1 = docTermVariable tv1
      doctv2 = docTermVariable tv2
      docrel = docRelation rel
  in  fillSoft [docForall, tupled [doctv1, doctv2], docIn, docrel <> docDot]
      <$> indent (docTheorem theorem)


docTheorem (ForallRelations rv res theorem) =
  let R _ _ (ttv1, ttv2) = rv
      docttvs = docTypeTermVariable ttv1 <> docComma <> docTypeTermVariable ttv2
      docrv   = docRelationVariable rv
      docres  = case res of
                  []        -> docDot
                  otherwise -> let drs = map docRestriction res
                                   i = map (\dr -> dr <> docComma) (init drs)
                                   l = last drs <> docDot
                               in  fillSep ([docComma] ++ i ++ [l])
  in  fillSoft [docForall, docttvs, docIn, docTypes <> docDot,
                docForall, docrv, docIn, docRel <> parens docttvs <> docres]
      <$> indent (docTheorem theorem)


docTheorem (ForallFunctions f (ttv1, ttv2) res theorem) =
  let docttvs = docTypeTermVariable ttv1 <> docComma <> docTypeTermVariable ttv2
      doct    = docType True (TypeFun (TypeTermVar ttv1) (TypeTermVar ttv2))
      docf    = docTermVariable f
      docres  = case res of
                  []        -> docDot
                  otherwise -> let drs = map docRestriction res
                                   i = map (\dr -> dr <> docComma) (init drs)
                                   l = last drs <> docDot
                               in  fillSep ([docComma] ++ i ++ [l])
  in  fillSoft [docForall, docttvs, docIn, docTypes <> docDot,
                docForall, docf, docColon <> docColon, doct <> docres]
      <$> indent (docTheorem theorem)


docTheorem (ForallElements tv t theorem) =
  let doctv   = docTermVariable tv
      doct    = case t of
                  TypeForall _ _ -> parens (docType True t)
                  otherwise      -> docType True t
  in  fillSoft [docForall, doctv, docColon <> docColon, doct <> docDot]
      <$> indent (docTheorem theorem)


docTheorem (Conjunction theorem1 theorem2) =
  let collect t = case t of
                    Conjunction t1 t2 -> t1 : collect t2
                    otherwise         -> [t]
      docts = map (\t -> align $ docTheorem t) (collect theorem2)
  in  foldl (<$>) (parens $ align $ docTheorem theorem1)
                  (map (\d -> docConjunction <+> parens d) docts)


docTheorem (Implication theorem1 theorem2) =
  (parens $ align $ docTheorem theorem1)
  <$> (docImplication <+> (align $ docTheorem theorem2))





-- | Creates a document from a term.

docTerm :: Document a => Term -> a
docTerm term =
  case term of
    TermVar tv    -> docTermVariable tv

    TermApp t1 t2 -> case t2 of
                       TermApp _ _ -> fillSep [docTerm t1, parens (docTerm t2)]
                       otherwise   -> fillSep [docTerm t1, docTerm t2]

    TermIns t ty  -> let docty = docType True ty
                     in  case t of
                           TermApp _ _ -> instant (parens (docTerm t)) docty
                           otherwise   -> instant (docTerm t) docty





-- | Creates a document from a restriction.

docRestriction :: Document a => Restriction -> a
docRestriction res =
  case res of
    IsStrict tv              -> fillSep [docTermVariable tv, docStrict]
    IsStrictAndContinuous rv -> fillSep [docRelationVariable rv,
                                         docStrictAndContinuous]





-- | Creates a document from a relation.

docRelation :: Document a => Relation -> a
docRelation rel =
  case rel of
    RelTerm t _             -> docTerm t

    RelVar rv               -> docRelationVariable rv

    RelLift model con rels  -> let doccon = docTypeConstructor con
                               in  docLiftRelation model doccon rels

    RelLiftList model rel'  -> docLiftRelation model (brackets empty) [rel']

    RelLiftTuple model rels -> let doccon = parens (docInt (length rels))
                               in  docLiftRelation model doccon rels

    RelFun model rel1 rel2  -> fillSoft [docRelation rel1,
                                         docArrow,
                                         docRelation rel2]

    RelForall model rv rel' -> let quant = case model of
                                             BasicModel -> docForall
                                             FixModel   -> docForallFix
                               in  fillSoft [quant,
                                             docRelationVariable rv <> docDot,
                                             docRelation rel']



-- | Creates a document from a lift relation.

docLiftRelation :: Document a => LanguageModel -> a -> [Relation] -> a
docLiftRelation model doccon rels =
  let doclift = case model of
                  BasicModel -> docLift
                  FixModel   -> docLiftPointed
      docrels =  map docRelation rels
  in  (instant doclift doccon) <> (tupled docrels)



--------------------------------------------------------------------------------



-- | Creates a document from an unfolded relation.

docUnfoldedRelation :: Document a => UnfoldedRelation -> a
docUnfoldedRelation (UnfoldedRelation rel model unfolded) =
  let docsets = case unfolded of
                  UnfoldedLift cs        -> docUnfoldedLift model cs
                  UnfoldedLiftList vs t  -> docUnfoldedListLift model vs t
                  UnfoldedLiftTuple vs t -> docUnfoldedTupleLift model vs t
                  UnfoldedFunction vs t  -> [docUnfoldedFunction model vs t]
                  UnfoldedForall vs t    -> [docUnfoldedForall model vs t]
      docrel = docRelation rel
      h = docEqual <+> head docsets
      t = map (\d -> docUnion <+> d) (tail docsets)
  in  foldr1 (<$>) (docrel : h : t)



-- | Creates a document from a tuple and a corresponding theorem.

docUnfoldedSet :: Document a => (a, a) -> Maybe a -> a
docUnfoldedSet (x, y) maybeTheorem =
  case maybeTheorem of
    Nothing      -> braces (tupled [x, y])
    Just theorem -> braces ((tupled [x, y]) <+> docBar <+> align theorem)



-- | Creates a document showing a set containing only a pair of bottoms.

docBottomSet :: Document a => LanguageModel -> Maybe a
docBottomSet model =
  case model of
    BasicModel -> Nothing
    FixModel   -> Just (docUnfoldedSet (docBottom, docBottom) Nothing)



-- | Creates a document for an unfolded lifted relation.

docUnfoldedLift :: Document a => LanguageModel
                                   -> [( DataConstructor
                                       , [TermVariable], [TermVariable]
                                       , Maybe Theorem)]
                                   -> [a]

docUnfoldedLift model list =
  maybeToList (docBottomSet model) ++ map makeSet list
  where
    makeSet (con, xs, ys, maybeTheorem) =
      let doccon = docDataConstructor con
          docxs = doccon <+> fillSep (map docTermVariable xs)
          docys = doccon <+> fillSep (map docTermVariable ys)
          doctheorem = liftM docTheorem maybeTheorem
      in  docUnfoldedSet (docxs, docys) doctheorem



-- | Creates a document for an unfolded lifted list relation.

docUnfoldedListLift :: Document a => LanguageModel
                                     -> ( (TermVariable, TermVariable)
                                        , (TermVariable, TermVariable))
                                     -> Theorem
                                     -> [a]

docUnfoldedListLift model ((x,xs),(y,ys)) theorem =
  let docx  = docTermVariable x
      docy  = docTermVariable y
      docxs = docTermVariable xs
      docys = docTermVariable ys
      doctuple   = (docx <.> docColon <.> docxs, docy <.> docColon <.> docys)
      docempty   = docUnfoldedSet (brackets empty, brackets empty) Nothing
      doctheorem = Just $ docTheorem theorem
  in  maybeToList (docBottomSet model)
      ++ [docempty] ++ [docUnfoldedSet doctuple doctheorem]



-- | Creates a document for an unfolded lifted tuple relation.

docUnfoldedTupleLift :: Document a => LanguageModel
                                      -> ([TermVariable], [TermVariable])
                                      -> Theorem
                                      -> [a]

docUnfoldedTupleLift model (xs, ys) theorem =
  let docxs    = tupled (map docTermVariable xs)
      docys    = tupled (map docTermVariable ys)
      doctuple = (docxs, docys)
      doctheorem = Just $ docTheorem theorem
  in  maybeToList (docBottomSet model) ++ [docUnfoldedSet doctuple doctheorem]



-- | Creates a document for an unfolded functional relation.

docUnfoldedFunction :: Document a => LanguageModel
                                     -> (TermVariable, TermVariable)
                                     -> Theorem
                                     -> a

docUnfoldedFunction model (f,g) theorem =
  docUnfoldedSet (docTermVariable f, docTermVariable g)
                 (Just $ docTheorem theorem)



-- | Creates a document for an unfolded relational abstraction.

docUnfoldedForall :: Document a => LanguageModel
                                   -> (TermVariable, TermVariable)
                                   -> Theorem
                                   -> a

docUnfoldedForall model (x,y) theorem =
  docUnfoldedSet (docTermVariable x, docTermVariable y)
                 (Just $ docTheorem theorem)
