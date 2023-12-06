import qualified Text.Regex as R

data Maximums = Maximums {
  red :: Int,
  blue :: Int,
  green :: Int
} deriving (Show)

stringToInt s = (read s)::Int

getColorUpdateFn c =
  case (c) of
    "red"   -> red
    "blue"  -> blue
    "green" -> green

updateMax num color maximums =
    case color of
      "red"   -> maximums{red   = num}
      "blue"  -> maximums{blue  = num}
      "green" -> maximums{green = num}

getMaximums game = extractVals game initMax
  where
    initMax         = Maximums 0 0 0
    getMatch        = R.matchRegexAll (R.mkRegex "([0-9]*) (red|green|blue)")
    getMax num c m  = max (stringToInt num) (getColorUpdateFn c $ m)
    extractVals s m =
      case (getMatch s) of
        Just (_, _, rest, num:c:[]) ->
          extractVals
          rest 
          (updateMax (getMax num c m) c m)
        Nothing                           -> m

getPower m = product $ [red, blue, green] <*> [m]

main = do
  input <- lines <$> readFile "./input"
  let output = sum $ map (getPower . getMaximums) input
  putStrLn $ show output
  
