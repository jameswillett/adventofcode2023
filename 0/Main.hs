import Data.Char

onlyNums = filter isDigit

getCalibrationValStr "" = "0"
getCalibrationValStr v = [head v, last v]

withIndex = zip [0..]

fromZipToInt :: [(Int, Char)] -> Int
fromZipToInt = foldl (\acc (i, v) -> acc + ((digitToInt v) * (10 ^ i))) 0

getCalibrationValFromString = fromZipToInt . withIndex . reverse . getCalibrationValStr . onlyNums

main = do
  input <- readFile "./input"
  let values = lines input
  let answer = sum $ map (getCalibrationValFromString) values
  putStrLn $ show answer
