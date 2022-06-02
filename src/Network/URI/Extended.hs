module Network.URI.Extended
  ( module Network.URI,
    addPath,
    addQueryParams,
    stringifyQueryList,
    stringifyQueryPair,
    QueryParam (..),
  )
where

import Data.List (intercalate)
import Network.URI

infixr 0 :=:

data QueryParam
  = String :=: String
  deriving (Show)

addPath :: URI -> String -> URI
addPath uri p = uri {uriPath = uriPath uri <> p}

addQueryParams :: URI -> [QueryParam] -> URI
addQueryParams uri [] = uri
addQueryParams uri qs =
  case uriQuery uri of
    [] -> uri {uriQuery = '?' : stringifyQueryList qs}
    q -> uri {uriQuery = q <> ('&' : stringifyQueryList qs)}

stringifyQueryList :: [QueryParam] -> String
stringifyQueryList = intercalate "&" . (stringifyQueryPair =<<)

stringifyQueryPair :: MonadFail m => QueryParam -> m String
stringifyQueryPair ("" :=: _vM) = fail "Key is empty"
stringifyQueryPair (k :=: "") = fail $ "Value is empty for " <> show k
stringifyQueryPair (k :=: v)
  | all isUnescapedInURIComponent k =
    pure . (k <>) . ('=' :) . escapeURIString isUnescapedInURIComponent $ v
  | otherwise = fail $ "key has unescaped chars " <> show k
