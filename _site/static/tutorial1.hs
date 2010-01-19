import Tea
import Control.Monad.State
import Control.Monad

screenWidth = 640
screenHeight = 480

data BallState = Ball { x  :: Int
                      , y  :: Int
                      , dx :: Int
                      , dy :: Int
                      }

main :: IO ()
-- we use a fixed point combinator to achieve the loop. 
-- This could be done with explicit recursion also.
main = runTea screenWidth screenHeight (Ball 320 240 1 1) $ fix $ \loop -> do 
           modify $ \s -> tick s -- update the ball state
           get >>= draw          -- draw the ball
           loop                  -- and around we go again

tick :: BallState -> BallState
tick (Ball x y dx dy) | collides_x = Ball (x-dx) (y+dy) (-dx)  dy 
                      | collides_y = Ball (x+dx) (y-dy)   dx (-dy) 
                      | otherwise  = Ball (x+dx) (y+dy)   dx   dy
    where collides_y = y > screenHeight || y < 0
          collides_x = x > screenWidth  || x < 0

draw :: BallState -> Tea BallState ()
draw (Ball x y _ _) = do clearM screen -- clear the screen of the old ball
                         circleM screen (x,y) radius white $ defaults { filled = True } -- draw the new ball
                         update -- update changes on the hardware screen
       where white  = Color 255 255 255 255
             radius = 10
