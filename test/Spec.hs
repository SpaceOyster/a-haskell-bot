import qualified Network.URI.ExtendedSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Network.URI.Extended" Network.URI.ExtendedSpec.spec
