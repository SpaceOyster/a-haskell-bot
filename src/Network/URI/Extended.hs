module Network.URI.Extended
    ( module Network.URI
    , addPath
    ) where

import Network.URI

type QueryParams = [(String, Maybe String)]

addPath :: URI -> String -> URI
addPath uri p = uri {uriPath = uriPath uri <> p}
