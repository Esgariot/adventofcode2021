import System.Environment
import System.Exit

data Direction = Forward Int | Up Int | Down Int

newtype Horizontal = Horizontal Integer

newtype Vertical = Vertical Integer

data Route = Route Horizontal Vertical

instance Num Horizontal where
  Horizontal a + Horizontal b = Horizontal $ a + b
  Horizontal a * Horizontal b = Horizontal $ a * b
  negate (Horizontal x) = Horizontal (- x)
  abs (Horizontal x) = Horizontal (abs x)
  signum (Horizontal x) = Horizontal (signum x)
  fromInteger x = Horizontal x

instance Num Vertical where
  Vertical a + Vertical b = Vertical $ a + b
  Vertical a * Vertical b = Vertical $ a * b
  negate (Vertical x) = Vertical (- x)
  abs (Vertical x) = Vertical (abs x)
  signum (Vertical x) = Vertical (signum x)
  fromInteger x = Vertical x

instance Semigroup Route where
  Route h1 v1 <> Route h2 v2 = Route (h1 + h2) (v1 + v2)

instance Monoid Route where
  mempty = Route 0 0

horizontal :: Int -> Horizontal
horizontal = Horizontal . fromIntegral

vertical :: Int -> Vertical
vertical = Vertical . fromIntegral

move :: Direction -> Route
move (Forward n) = Route (horizontal n) 0
move (Up n) = Route 0 (vertical (- n))
move (Down n) = Route 0 (vertical n)

travel :: [Direction] -> Route
travel = mconcat . map move

multiply (Route (Horizontal h) (Vertical v)) = h * v

parseDirection :: String -> Int -> Direction
parseDirection "forward" = Forward
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection _ = error "Unexpected input" -- input is well-formed

parseLines s = parse . words <$> lines s
  where
    parse l = parseDirection (head l) (read $ last l :: Int)

-- readDirections =  map words  . lines

parseArgs ["-h"] = usage >> exitSuccess
parseArgs [input] = readFile input
parseArgs _ = usage >> exitFailure

usage = putStrLn "provide path to file containing list commands."

main = getArgs >>= parseArgs >>= print . multiply . travel .  parseLines