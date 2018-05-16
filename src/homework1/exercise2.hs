import CodeWorld

tree :: Integer -> Picture

tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))
  
bloom :: Picture->  Integer -> Picture

bloom b 0 = b 
bloom b n =  translated 0 1 ( 
  rotated (pi/10) (bloom b (n-1)) & rotated (- pi/10) (bloom b (n-1)))  

t :: Double -> Picture
t  ts = (colored red (solidCircle ((min ts 10)/50)))

myAnimation :: Double -> Picture
myAnimation ts = ( (tree deep) & (   bloom  (t ts)  deep)   )

--drawingOf  ( (tree deep) & (   bloom  (t 5)  deep)   )
deep=8
main :: IO ()

main =  animationOf myAnimation
