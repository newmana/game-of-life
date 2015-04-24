import Control.Concurrent

liveNeighbours :: (Int, Int) -> [(Int, Int)] -> Int
liveNeighbours (x, y) world = length $ concat $ do
    offsetX <- [-1..1]
    offsetY <- [-1..1]
    return (if ((offsetX /= 0 || offsetY /= 0) && (elem (x + offsetX, y + offsetY) world)) then [(x, y)] else [])

isAlive :: (Int, Int) -> [(Int, Int)] -> Bool
isAlive (x, y) world 
    | (x, y) `elem` world = ln `elem` [2,3]
    | otherwise = ln == 3
    where ln = (liveNeighbours (x,y) world)

minX world = (minimum $ fst $ unzip world) - 1
maxX world = (maximum $ fst $ unzip world) + 1
minY world = (minimum $ snd $ unzip world) - 1
maxY world = (maximum $ snd $ unzip world) + 1

newWorld :: [(Int, Int)] -> [(Int, Int)]
newWorld world = concat $ do
    y <- [minY world..maxY world]
    x <- [minX world..maxX world]
    return (if (isAlive (x, y) world) then [(x,y)] else [] )

ch :: (Int, Int) -> [(Int, Int)] -> [Char]
ch (x,y) world  
    | isAlive (x,y) world = "O"
    | otherwise = " "
    
br :: (Int, Int) -> [(Int, Int)] -> [Char]
br (x,y) world  
    | x == maxX world = "\n"
    | otherwise = ""

printXY :: (Int, Int) -> [(Int, Int)] -> [Char]
printXY (x, y) world = (ch (x,y) world) ++ (br (x,y) world)
    
worldGrid :: [(Int, Int)] -> [[Char]]
worldGrid world = do 
    y <- [minY world..maxY world]
    x <- [minX world..maxX world]
    return (printXY (x, y) world)

printWorld :: [(Int, Int)] -> IO ()
printWorld world = putStr $ concat $ worldGrid world

cls :: IO ()
cls = putStr "\ESC[2J"

wait :: Float -> IO ()
wait seconds =  threadDelay $ round $ seconds * 1000000

life :: [(Int, Int)] -> IO ()
life b = do 
    cls
    printWorld b
    wait 0.05
    life $ (newWorld b)

startWorld :: [(Int, Int)]
startWorld = [(1,1), (2,1), (1,2), (1,3)]

glider :: [(Int, Int)]
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

gliderPlus :: [(Int, Int)]
gliderPlus = [(5,2), (3,3), (5,3), (4,4), (5,4), (1,1), (2,1), (1,2), (1,3)]

endWorld :: [(Int, Int)]
endWorld = [(1,1), (2,1), (0,2), (1,2)]

-- TESTS
testLN1 = liveNeighbours (2,2) startWorld == 4
testLN2 = liveNeighbours (1,2) startWorld == 3
testLN3 = liveNeighbours (2,1) startWorld == 2
testLN4 = liveNeighbours (1,3) startWorld == 1
testLN5 = liveNeighbours (3,3) startWorld == 0

testAlive0 = isAlive(1,1) startWorld == True
testAlive1 = isAlive(2,2) startWorld == False
testAlive2 = isAlive(1,3) startWorld == False
testAlive3 = isAlive(0,2) startWorld == True

testProgress0 = newWorld startWorld == endWorld
