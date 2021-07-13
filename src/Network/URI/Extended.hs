module Network.URI.Extended
    ( module Network.URI
    , addPath
    ) where

import Network.URI


addPath :: URI -> String -> URI
addPath uri p = uri {uriPath = uriPath uri <> p}
