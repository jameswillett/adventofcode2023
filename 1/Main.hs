import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Split as S
import qualified Text.Regex      as R

-- Game 25: 3 green, 2 red, 1 blue; 2 blue, 10 green, 1 red; 8 red, 4 green, 1 blue; 3 blue, 15 red, 6 green; 2 blue, 13 red, 8 green; 2 red, 5 blue, 5 green

data Color = Red | Blue | Green

maxNumber Red = 12
maxNumber Blue = 14
maxNumber Green = 13

trim = f . f
   where f = reverse . dropWhile C.isSpace

extractMatch Nothing                = ""
extractMatch (Just (_, _, _, x:xs)) = x

stringToInt s = (read s)::Int

getId = stringToInt . extractMatch . R.matchRegexAll (R.mkRegex "^Game ([0-9]*)")

getHands = map trim . tail . S.splitOneOf ":;"

getValidDice = all isValidRoll . map trim . S.splitOn ","
  where
    isValidRoll roll = maxNumber (getColor roll) >= (getNumber roll)
    getColor roll | L.isSuffixOf "red" roll  = Red
                  | L.isSuffixOf "blue" roll = Blue
                  | otherwise                = Green
    getNumber roll = stringToInt $ takeWhile (not . C.isSpace) roll

getValidGame = all (==True) . (map getValidDice) . getHands

extractIDFromValidGame game | getValidGame game = getId game
                            | otherwise         = 0

main = do
  input <- lines <$> readFile "./input"
  let o = sum $ map extractIDFromValidGame input
  putStrLn $ show o
  
