{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Tea.Monad 
       ( screen
       , update
       , runTea
       , handleEvents
       , (?)
       , is
       , mouseCoords
       , mouseButtons
       , currentModKeys
       ) where

import Tea.Types
import Tea.BitmapConstructorsInternal
import Tea.Event
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Sprig as SPG
import qualified Graphics.UI.SDL.Mixer as Mixer
import Data.Array ((//),(!), listArray)
import qualified Data.Map as M
instance Functor Tea where
   fmap f (Tea v) = Tea $ fmap f v

instance MonadIO Tea where
   liftIO f = Tea $ (liftIO f)


instance MonadState s (Teacup s) where
   get = Teacup $ get
   put = Teacup . put

instance Functor (Teacup s) where
   fmap f (Teacup v) = Teacup $ fmap f v

instance MonadIO (Teacup s) where
   liftIO f = Teacup $ liftIO f

screen :: Teacup s Screen
screen = _screen <$> getT

eventStateModify f = modifyT $ \ts@(TS {_eventState = es}) -> ts {_eventState = f es}
keyCodesModify f  = eventStateModify $ \es@(ES { keyCodes = s }) -> es { keyCodes = f s }
anyKeyModify   f  = eventStateModify $ \es@(ES { keysDown = s }) -> es { keysDown = f s }

setEventQuery :: EventQuery -> Teacup s ()
setEventQuery (KeyDown code) = keyCodesModify  $ \c -> c // [(code, True) ]
setEventQuery (KeyUp code)   = keyCodesModify  $ \c -> c // [(code, False)]
setEventQuery AnyKeyDown     = anyKeyModify (+ 1)
setEventQuery NoKeyDown      = anyKeyModify (\x -> x - 1)
setEventQuery _              = undefined

getEventQuery :: EventQuery -> Teacup s Bool
getEventQuery (KeyDown code)       = queryKeyCode code
getEventQuery (KeyUp code)         = not <$> queryKeyCode code
getEventQuery (AnyKeyDown)         = queryKeyDown
getEventQuery (NoKeyDown)          = not <$> queryKeyDown
getEventQuery (ModOn code)         = queryModState code
getEventQuery (ModOff code)        = not <$> queryModState code
getEventQuery (MouseIn c1 c2)      = queryMouseIn c1 c2
getEventQuery (MouseOutside c1 c2) = not <$> queryMouseIn c1 c2
getEventQuery (AppVisible)         = queryAppVisible
getEventQuery (AppInvisible)       = not <$> queryAppVisible
getEventQuery (AnyMouseDown)       = queryMouseDown 
getEventQuery (NoMouseDown)        = not <$> queryMouseDown
getEventQuery (MouseDown button)   = queryMouseButton button
getEventQuery (MouseUp button)     = not <$> queryMouseButton button

queryKeyCode code  = (! code) <$> keyCodes <$> _eventState <$> getT
queryKeyDown       = (> 0)    <$> keysDown <$> _eventState <$> getT
queryMouseDown     = mouseButtons   >>= return . (/= [])
queryModState code = currentModKeys >>= return . elem code
queryMouseButton b = mouseButtons   >>= return . elem b
queryMouseIn c1 c2 = within c1 c2 <$> mouseCoords
queryAppVisible :: Teacup s Bool
queryAppVisible    = liftIO $ SDL.getAppState >>= return . elem (SDL.ApplicationFocus)

currentModKeys :: Teacup s [Mod]
currentModKeys = liftIO $ SDL.getModState >>= return . map (sdlMod)

mouseCoords :: Teacup s (Int, Int)
mouseCoords  = liftIO $ SDL.getMouseState >>= \(x, y, _) -> return (x,y)

mouseButtons :: Teacup s [Button]
mouseButtons = liftIO $ SDL.getMouseState >>= \(_, _, l) -> return $ map sdlButton l

within (x1,y1) (x2,y2) (x, y) = x > x1 && y > y1 && x < x2 && y < y2

(?) :: EventQuery -> Teacup s z -> Teacup s ()
q ? m = do a <- getEventQuery q
           if a 
              then m >> return ()
              else return ()

is :: EventQuery -> Teacup s Bool
is = getEventQuery

setFrameRate :: Int ->  Teacup s ()
setFrameRate n = modifyT $ \ts -> ts { _fpsCap = 1000 `div` n }

update :: Tea ()
update = do ts@(TS { _screen = (Screen x), _fpsCap = fps, _lastUpdate = last}) <- get             
            t <- liftIO $ SDL.getTicks
            liftIO $ do
                     when (fromIntegral t < last + fps) $ SDL.delay $ fromIntegral $ last + fps - fromIntegral t
                     SDL.tryFlip x
            put $ ts { _lastUpdate = fromIntegral t}

handleEvents :: Event s -> Teacup s ()
handleEvents e = let e' = eventHandler {
                             keyDown = \code _-> (setEventQuery (KeyDown code) >> setEventQuery AnyKeyDown),
                             keyUp   = \code _-> (setEventQuery (KeyUp   code) >> setEventQuery NoKeyDown)
                          } +> e 
                     this = do
                            event <- liftIO $ SDL.pollEvent
                            buttons <- mouseButtons
                            case event of 
                               SDL.GotFocus l            -> foldl (>>) (return ()) $ map gotFocus' l
                               SDL.LostFocus l           -> foldl (>>) (return ()) $ map lostFocus' l
                               SDL.KeyDown ks            -> keyDown e'   (sdlKey (SDL.symKey ks)) (map sdlMod (SDL.symModifiers ks))
                               SDL.KeyUp   ks            -> keyUp   e'   (sdlKey (SDL.symKey ks)) (map sdlMod (SDL.symModifiers ks))
                               SDL.MouseButtonUp x y b   -> mouseUp e'   (sdlButton b) (fromIntegral x, fromIntegral y)
                               SDL.MouseButtonDown x y b -> mouseDown e' (sdlButton b) (fromIntegral x, fromIntegral y)
                               SDL.MouseMotion x y _ _   -> mouseMove e' (fromIntegral x, fromIntegral y) buttons
                               SDL.Quit                  -> exit e'                               
                               _                         -> return ()
                            if event /= SDL.NoEvent then this else return ()
                     gotFocus' SDL.MouseFocus = mouseGained e'
                     gotFocus' SDL.InputFocus = keyboardGained e'
                     gotFocus' SDL.ApplicationFocus = restored e'
                     lostFocus' SDL.MouseFocus = mouseLost e'
                     lostFocus' SDL.InputFocus = keyboardLost e'
                     lostFocus' SDL.ApplicationFocus = minimized e'
                 in this

bitsPerPixel = 32

initialEventState = ES { keyCodes    = listArray (minBound :: KeyCode, maxBound :: KeyCode) $ repeat False
                       , keysDown    = 0
                       }

runTea :: Int -> Int -> Tea m -> IO m
runTea w h m = do
               SDL.init [SDL.InitEverything]
               Mixer.openAudio 44100 Mixer.AudioS16Sys 2 1024               
               surf <- SDL.setVideoMode w h bitsPerPixel [SDL.SWSurface]
               let initialState = (TS (Screen surf) initialEventState (1000 `div` 60) 0 M.empty) 
               (v, st) <- runStateT (extract m) initialState 
               Mixer.closeAudio
               SDL.quit
               return $ v
            
 
