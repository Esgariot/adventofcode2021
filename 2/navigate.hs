import System.Environment
import System.Exit

data Direction = Forward Int | Up Int | Down Int deriving(Show)

data Route = Route {horizontal :: Int, vertical :: Int, aim :: Int} deriving(Show)

move :: Direction -> Route -> Route
move (Forward n) (Route h v a) = Route (n + h) (v + (n * a)) a
move (Up n) (Route h v a) = Route h v (a - n)
move (Down n) (Route h v a) = Route h v (a + n)

travel :: [Direction] -> Route
travel = foldr move (Route 0 0 0)

multiply Route {horizontal = h, vertical = v} = h * v

parseDirection :: String -> Int -> Direction
parseDirection "forward" = Forward
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection _ = error "Unexpected input" -- input is well-formed

parseLines :: String -> [Direction]
parseLines s = parse . words <$> lines s
  where
    parse l = parseDirection (head l) (read $ last l :: Int)

parseArgs ["-h"] = usage >> exitSuccess
parseArgs [input] = readFile input
parseArgs _ = usage >> exitFailure

usage = putStrLn "provide path to file containing list commands."

main = getArgs >>= parseArgs >>= print . multiply . travel . reverse . parseLines