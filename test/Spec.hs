import Data.List.ExtendedSpec as LE
import Network.URI.ExtendedSpec as NUE
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    LE.spec
    NUE.spec
