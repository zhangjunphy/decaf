import Test.Tasty

data Type = Type1 String
          | Type2
            deriving (Show)

testFunc :: String -> Type
testFunc s = Type1 s

main = do
  print $ show $ testFunc "good"
