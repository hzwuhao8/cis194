{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank

data Direction = R | U | L | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)


someCoord :: Coord
someCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L initialCoord))



wall, ground,storage, box :: Picture

wall = colored (grey 0.4) (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box = colored brown (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank


 

 
drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))
         
maze :: Integer -> Integer -> Tile 
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
 
ll = [-5 .. 5 ]
ll5 = concatMap (\x -> map (\y -> drawTileAt x y) ll )  ll
pictureOfMaze = foldr (\x y ->  x & y ) blank ll5 


main :: IO ()

--main = drawingOf (atCoord someCoord pictureOfMaze)

main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

 
handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c      = c


drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze


