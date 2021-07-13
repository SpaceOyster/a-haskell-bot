module Network.URI.Extended
    ( module Network.URI
    , addPath
    , addQueryParams
    ) where

import Data.List (intercalate)
import Network.URI

type QueryParams = [(String, Maybe String)]

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
