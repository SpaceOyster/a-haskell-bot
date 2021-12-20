import qualified Data.List.ExtendedSpec
import qualified Network.URI.ExtendedSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.List.Extended" Data.List.ExtendedSpec.spec
  describe "Network.URI.Extended" Network.URI.ExtendedSpec.spec
