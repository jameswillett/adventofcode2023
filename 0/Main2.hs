import Data.Char
import Data.List
import Text.Regex
import Control.Applicative

numbersWords = ["zero","one","two","three","four","five","six","seven","eight","nine"]
numbersWordsRev = map reverse numbersWords
numbersDigits = "[0-9]"

makeRegex r = mkRegex ((intercalate "|" r) ++ "|" ++ numbersDigits)

regexForwards = makeRegex numbersWords
regexBackwards = makeRegex numbersWordsRev

fromValueToInt v =
  case elemIndex v numbersWords of
    Just i  -> i
    Nothing -> digitToInt (head v)

getMatches v = (liftA2 (,)) firstMatch lastMatch
  where
    firstMatch = extractMatch <$> matchRegexAll regexForwards v
    lastMatch = reverse . extractMatch <$> (matchRegexAll regexBackwards (reverse v))
    extractMatch (_, match, _, _) = match

getValue v =
  case (getMatches v) of
    Just (x, y) -> (fromValueToInt x) * 10 + (fromValueToInt y)
    Nothing     -> 0


main = do
  input <- readFile "./input"
  let values = lines input
  let answer = sum $ map (getValue) values
  putStrLn $ show answer
