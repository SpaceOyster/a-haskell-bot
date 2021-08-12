module Network.URI.Extended
  ( module Network.URI
  , addPath
  , addQueryParams
  , QueryParams
  , QueryParam
  ) where

import Control.Monad.Fail (fail)
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
        [] -> uri {uriQuery = '?' : stringifyQuery qs}
        q -> uri {uriQuery = q <> ('&' : stringifyQuery qs)}

stringifyQuery :: QueryParams -> String
stringifyQuery = intercalate "&" . fmap (\(k, v) -> k <> maybe "" ('=' :) v)

stringifyQueryPair :: MonadFail m => QueryParam -> m String
stringifyQueryPair (k, Nothing) = fail $ "No value present for " <> show k
stringifyQueryPair (k, Just v) = pure . (k <>) . ('=' :) . fmap spaceToPlus $ v
  where
    spaceToPlus :: Char -> Char
    spaceToPlus ' ' = '+'
    spaceToPlus c = c
