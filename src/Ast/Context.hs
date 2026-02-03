{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use (,)" #-}
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
      Map.mapEither
        ( \xs ->
            case xs of
              [x] -> Left x
              _ -> Right xs
        )
        countUniques
   in
    if Map.null repeatedItems
      then
        let inter = Map.intersectionWith (\x y -> (x, y)) ctx regularItems
         in if Map.null inter
              then pure $ Context $ Map.union ctx regularItems
              else
                let
                  errorMsg (name, (oldValue, newValue)) =
                    pString "The variable "
                      <> indentPretty name
                      <> line
                      <> pString "is already defined as"
                      <> indentPretty oldValue
                      <> line
                      <> pString "and is being redefined as "
                      <> indentPretty newValue
                 in
                  throwDocError
                    ( pString "Error"
                        <> indent
                          Common.defaultIndentationSpaces
                          ( concatWith
                              (\x y -> x <> line <> y)
                              ( errorMsg
                                  <$> Map.toList inter
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
