module Network.URI.Extended
  ( module Network.URI
  , addPath
  , addQueryParams
  , stringifyQueryList
  , stringifyQueryPair
  , QueryParams
  , QueryParam
  ) where

import Data.List (intercalate)
import Network.URI

type QueryParam = (String, Maybe String)

type QueryParams = [QueryParam]

addPath :: URI -> String -> URI
addPath uri p = uri {uriPath = uriPath uri <> p}

addQueryParams :: URI -> QueryParams -> URI
addQueryParams uri [] = uri
addQueryParams uri qs =
  case uriQuery uri of
    [] -> uri {uriQuery = '?' : stringifyQueryList qs}
    q -> uri {uriQuery = q <> ('&' : stringifyQueryList qs)}

stringifyQueryList :: QueryParams -> String
stringifyQueryList = intercalate "&" . (stringifyQueryPair =<<)

stringifyQueryPair :: MonadFail m => QueryParam -> m String
stringifyQueryPair (k, Nothing) = fail $ "No value present for " <> show k
stringifyQueryPair (k, Just v) =
  pure . (k <>) . ('=' :) . escapeURIString isUnescapedInURIComponent $ v
