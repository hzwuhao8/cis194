import CodeWorld

wall, ground,storage, box :: Picture

wall = colored (grey 0.4) (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box = colored brown (solidRectangle 1 1)


drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

colsmax = 11

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r  = drawCols r (-10) & drawRows (r+1)

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols r c = drawTileAt r c & drawCols r (c+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  
pictureOfMaze :: Picture
pictureOfMaze = drawRows (-1)

ll = [-5 .. 5 ]
ll2 = map (\x -> drawCols x (-10) ) ll

sum1 = foldr (\x y ->  x & y ) blank ll2

ll3 = zipWith (\x y -> drawTileAt x y ) ll ll 

sum2 = foldr (\x y ->  x & y ) blank ll3

ll4 = map (\x ->  foldr ( \x y ->  x & y ) blank ( map (\y -> drawTileAt x y) ll )) ll

sum4 = foldr (\x y ->  x & y ) blank ll4


ll5 = concatMap (\x -> map (\y -> drawTileAt x y) ll )  ll
 
sum5 = foldr (\x y ->  x & y ) blank ll5 

main :: IO ()

main = drawingOf sum5

