--
-- Adapted from "Programming in Haskell" by Graham Hutton
--
import Control.Concurrent

width :: Int
width = 50

height :: Int
height = 25

type Pos = (Int, Int)
type Board = [Pos]

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt pos xs = do goto pos
                    putStr xs

showCells :: Board -> IO ()
showCells b = sequence_ [writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

neighbours :: Pos -> [Pos]
neighbours (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                              (x + 1, y - 1), (x - 1, y),
                              (x + 1, y), (x - 1, y + 1),
                              (x, y + 1), (x + 1, y + 1)]

liveNeighbours :: Board -> Pos -> Int
liveNeighbours b = length . filter (isAlive b) . neighbours

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbours b p) [2, 3]]							 

births :: Board -> [Pos]
births b = [p | p <- rmDupes (concat (map neighbours b)), isEmpty b p, liveNeighbours b p == 3]

rmDupes :: Eq a => [a] -> [a]
rmDupes [] = []
rmDupes (x:xs) = x : rmDupes (filter (/= x) xs)

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

wait :: Float -> IO ()
wait seconds =  threadDelay $ round $ seconds * 1000000

life :: Board -> IO ()
life b = do 
	cls
	showCells b
	wait 0.05
	life (nextGen b)

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

gliderPlus :: [(Int, Int)]
gliderPlus = [(5,2), (3,3), (5,3), (4,4), (5,4), (1,1), (2,1), (1,2), (1,3)]
