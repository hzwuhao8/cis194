import CodeWorld

 


mycc c dx dy = colored c ( translated dx dy (solidCircle 1 ))

topCircle c = mycc c 0 (2.5) 
midCircle c = mycc c 0 0 
botCircle c = mycc c 0 (-2.5)
frame =  rectangle 2.5 7.5

trafficLight 1  = topCircle black & midCircle black &  botCircle green & frame

trafficLight 2  = topCircle black & midCircle yellow &  botCircle black & frame

trafficLight 3 = topCircle  red & midCircle black & botCircle  black   & frame

trafficLight 4  = topCircle red & midCircle yellow &  botCircle black & frame

delay = 2
trafficController :: Double -> Picture
trafficController t
  | round (t/delay) `mod` 8 == 0 = trafficLight 1
  | round (t/delay) `mod` 8 == 1 = trafficLight 1
  | round (t/delay) `mod` 8 == 2 = trafficLight 1
  | round (t/delay) `mod` 8 == 3 = trafficLight 2
  | round (t/delay) `mod` 8 == 4 = trafficLight 3
  | round (t/delay) `mod` 8 == 5 = trafficLight 3
  | round (t/delay) `mod` 8 == 6 = trafficLight 3
  | round (t/delay) `mod` 8 == 7 = trafficLight 4
   
tmp1 =  (translated (-6) 0 (trafficLight 1 ) & translated (-3) 0 (trafficLight 2 ) & ( trafficLight 3 ) & translated (3) 0 (trafficLight 4 )  )


main :: IO ()

main = animationOf trafficController

