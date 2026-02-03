{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Ast.Context where

import Data.Bifunctor (Bifunctor (first))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)

-- \* Pretty imports
import Prettyprinter
  ( Pretty (pretty)
  , concatWith
  , indent
  , line
  , (<+>)
  )

-- \* Internal imports
import qualified Common

-- \* JSON imports
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

-- \* Internal imports
import Ast.Symbol (Symbol, makeSymbol)
import Ast.Type (SimpleType)
import Common (indentPretty, pString, throwDocError)


newtype Context = Context
  { expressionsMap :: Map Symbol SimpleType
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Context


instance Pretty Context where
  pretty (Context c) =
    let
      items = Map.toList c
      prettyItems =
        Common.prettyItemList items (pretty ',') (pretty ':')
     in
      pretty @String "Context"
        <+> pretty '['
        <> line
        <> prettyItems
        <> line
        <> pretty ']'


emptyContext :: Context
emptyContext = Context mempty


contextFromList
  :: [(String, SimpleType)]
  -> Context
contextFromList ls =
  Context
    ( Map.fromList
        ( first makeSymbol <$> ls
        )
    )


findExpressionVariable
  :: Error String :> es
  => Symbol
  -> Context
  -> Eff es SimpleType
findExpressionVariable symbol context =
  case Map.lookup symbol context.expressionsMap of
    Just st -> pure st
    Nothing ->
      throwDocError
        ( pString "Can't find the required variable"
            <> indentPretty symbol
            <> line
            <> pString "context"
            <> indentPretty context
        )


addExpressionVars
  :: Error String :> es
  => [(Symbol, SimpleType)]
  -> Context
  -> Eff es Context
addExpressionVars newVars (Context ctx) =
  let
    countUniques =
      Map.fromListWith
        (<>)
        ( (\(x, y) -> (x, [y]))
            <$> newVars
        )
    (regularItems, repeatedItems) =
      Map.foldrWithKey go (Map.empty, Map.empty) countUniques
      where
        go k [x] (m1, m2) = (Map.insert k x m1, m2)
        go k xs (m1, m2) = (m1, Map.insert k xs m2)
   in
    if Map.null regularItems
      then
        let keys = Map.keys regularItems
            lookupResults = (\k -> (\x -> (k, x)) <$> Map.lookup k ctx) <$> keys
         in case catMaybes lookupResults of
              [] -> pure $ Context $ Map.union ctx regularItems
              newValues ->
                let
                  errorMsg (name, oldValue) =
                    pString "The variable "
                      <> indentPretty name
                      <> line
                      <> pString "is already defined as"
                      <> indentPretty oldValue
                      <> line
                      <> pString "and is being redefined as "
                      <> indentPretty (Map.lookup name regularItems)
                 in
                  throwDocError
                    ( pString "Error"
                        <> indent
                          Common.defaultIndentationSpaces
                          ( concatWith
                              (\x y -> x <> line <> y)
                              ( errorMsg
                                  <$> newValues
                              )
                          )
                    )
      else
        throwDocError
          ( pString "Tried to add duplicate elements to context"
              <> indent
                Common.defaultIndentationSpaces
                ( line
                    <> pretty (Map.toList repeatedItems)
                )
          )


addTypeToExpression
  :: Context
  -> Symbol
  -> SimpleType
  -> Context
addTypeToExpression context symbol ty =
  Context $ Map.insert symbol ty context.expressionsMap
