{-# LANGUAGE NoMonomorphismRestriction #-}
-- |Includes event handling concerns and mouse, application and keyboard state
--  query mechanisms.
module Tea.Event ( Event (..)
                 , EventQuery (..)
                 , (+>)
                 , eventHandler
                 , handleEvents
                 , updateEvents
                 , (?)
                 , is
                 , mouseCoords
                 , mouseButtons
                 , currentModKeys
                 ) where
import qualified Graphics.UI.SDL as SDL
import Data.Array ((//),(!))
import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad(when)
import Data.Monoid
import Tea.Input
import Tea.Tea
import Tea.TeaState

-- | A data type used for phrasing queries passed to the `?' and `is' functions
data EventQuery = KeyDown KeyCode -- ^ Is the specified key currently held down?
                | KeyUp   KeyCode -- ^ Opposite of KeyDown
                | ModOn   Mod     -- ^ Is the specified modifier key currently enabled?
                | ModOff  Mod     -- ^ Opposite of ModOn
                | MouseIn (Int, Int) (Int, Int)      -- ^ Is the mouse between these two coordinates?
                | MouseOutside (Int, Int) (Int, Int) -- ^ Opposite of MouseIn
                | AnyKeyDown        -- ^ Is any key currently held down?
                | NoKeyDown         -- ^ Opposite of AnyKeyDown
                | MouseDown Button  -- ^ Is the specified mouse button being clicked?
                | MouseUp Button    -- ^ Opposite of MouseDown
                | AnyMouseDown      -- ^ Are any mouse buttons currently being clicked?
                | NoMouseDown       -- ^ Opposite of AnyMouseDown
                | AppVisible        -- ^ Is the app window currently non-minimized?
                | AppInvisible      -- ^ Opposite of AppVisible
                deriving (Show, Eq)

-- | A monoidal data type that specifies what to do when an event occurs. Use the
--   default no-op `eventHandler' (or `mempty') constant and override fields rather
--   than specify this directly.
--   You can combine the functionality of many Events using `+>' or `mappend', which
--   will run both handlers in the order they were appended.
data Event s = Event { keyDown        :: KeyCode -> [Mod] -> Tea s () -- ^ When a key is pressed
                     , keyUp          :: KeyCode -> [Mod] -> Tea s () -- ^ When a key stops being pressed
                     , mouseDown      :: Button -> (Int, Int) -> Tea s () -- ^ When a mouse button is pressed
                     , mouseUp        :: Button -> (Int, Int) -> Tea s () -- ^ When a mouse button stops being pressed
                     , mouseMove      :: (Int, Int) -> [Button] -> Tea s ()  -- ^ When the mouse moves
                     , mouseGained    :: Tea s () -- ^ When the application gains mouse focus
                     , mouseLost      :: Tea s () -- ^ When the application loses mouse focus
                     , keyboardGained :: Tea s () -- ^ When the application gains keyboard focus
                     , keyboardLost   :: Tea s () -- ^ When the application loses keyboard focus
                     , exit           :: Tea s () -- ^ When the application recieves the exit signal
                     , minimized      :: Tea s () -- ^ When the application is minimized
                     , restored       :: Tea s () -- ^ When the application ceases being minimized
                     }

instance Monoid (Event s) where
  mappend (Event a1 a2 a3 a4 a5 a7 a8 a9 a10 a11 a12 a13) (Event b1 b2 b3 b4 b5 b7 b8 b9 b10 b11 b12 b13) = Event {
    keyDown        = \key mods -> a1 key mods >> b1 key mods,
    keyUp          = \key mods -> a2 key mods >> b2 key mods,
    mouseDown      = \c b    -> a3 c b >> b3 c b,
    mouseUp        = \c b    -> a4 c b >> b4 c b,
    mouseMove      = \c b    -> a5 c b >> b5 c b,
    mouseGained    = a7 >> b7,
    mouseLost      = a8 >> b8,
    keyboardGained = a9 >> b9,
    keyboardLost   = a10 >> b10,
    exit           = a11 >> b11,
    minimized      = a12 >> b12,
    restored       = a13 >> b13
  }
  mempty = Event {
    keyDown           = \key mods -> return (),
    keyUp             = \key mods -> return (),
    mouseDown         = \x b -> return (),
    mouseUp           = \x b -> return (),
    mouseMove         = \x b -> return (),
    mouseGained       = return (),
    mouseLost         = return (),
    keyboardGained    = return (),
    keyboardLost      = return (),
    exit              = return (),
    minimized         = return (),
    restored          = return ()
  }

z :: Tea s ()
z = return ()

-- |Combine two event handlers. Analogous to mappend
(+>) :: Event s -> Event s -> Event s
(+>) = mappend

-- |A default, no-op event handler for overriding. Analogous to mzero.
eventHandler :: Event s
eventHandler = mempty

-- |Flush the event queue and update mouse and keyboard state queries, but
--  do not run any event handlers. Add this to your game loop if you use
--  `?' and `is' but not `handleEvents'.
updateEvents :: Tea s ()
updateEvents = handleEvents eventHandler

-- |Flush the event queue, updating mouse and keyboard state queries, and
--  executing actions defined in the specified event handler.
handleEvents :: Event s -> Tea s ()
handleEvents e = let e' = eventHandler {
                             keyDown = \code _-> (setEventQuery (KeyDown code) >> setEventQuery AnyKeyDown),
                             keyUp   = \code _-> (setEventQuery (KeyUp   code) >> setEventQuery NoKeyDown)
                          } +> e
                     this = do
                            event <- liftIO SDL.pollEvent
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
                            when (event /= SDL.NoEvent) this
                     gotFocus' SDL.MouseFocus = mouseGained e'
                     gotFocus' SDL.InputFocus = keyboardGained e'
                     gotFocus' SDL.ApplicationFocus = restored e'
                     lostFocus' SDL.MouseFocus = mouseLost e'
                     lostFocus' SDL.InputFocus = keyboardLost e'
                     lostFocus' SDL.ApplicationFocus = minimized e'
                 in this

setEventQuery (KeyDown code) = keyCodesModify (// [(code, True) ])
setEventQuery (KeyUp code)   = keyCodesModify (// [(code, False)])
setEventQuery AnyKeyDown     = anyKeyModify (+ 1)
setEventQuery NoKeyDown      = anyKeyModify (subtract 1)
setEventQuery _              = undefined

getEventQuery :: EventQuery -> Tea s Bool
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

eventStateModify f = modifyT $ \ts@(TS {_eventState = es}) -> ts {_eventState = f es}
keyCodesModify f  = eventStateModify $ \es@(ES { keyCodes = s }) -> es { keyCodes = f s }
anyKeyModify   f  = eventStateModify $ \es@(ES { keysDown = s }) -> es { keysDown = f s }

queryKeyCode code  = (! code)     <$> keyCodes <$> _eventState <$> getT
queryKeyDown       = (> 0)        <$> keysDown <$> _eventState <$> getT
queryMouseDown     = (/= [])      <$> mouseButtons
queryModState code = elem code    <$> currentModKeys
queryMouseButton b = elem b       <$> mouseButtons
queryMouseIn c1 c2 = within c1 c2 <$> mouseCoords
queryAppVisible    = liftIO $ elem SDL.ApplicationFocus <$> SDL.getAppState

-- |Get the current modifier keys that are enabled
currentModKeys :: Tea s [Mod]
currentModKeys = liftIO $ map sdlMod <$> SDL.getModState

-- |Get the current mouse coordinates
mouseCoords :: Tea s (Int, Int)
mouseCoords  = liftIO $ SDL.getMouseState >>= \(x, y, _) -> return (x,y)

-- |Get the currently pressed mouse buttons
mouseButtons :: Tea s [Button]
mouseButtons = liftIO $ SDL.getMouseState >>= \(_, _, l) -> return $ map sdlButton l

within (x1,y1) (x2,y2) (x, y) = x > x1 && y > y1 && x < x2 && y < y2

-- | Execute the specified Tea action if the EventQuery specified is true.
(?) :: EventQuery -> Tea s v -> Tea s ()
q ? m = getEventQuery q >>= flip when (m >> return ())


-- | Produces a boolean value based on the specified EventQuery.
is :: EventQuery -> Tea s Bool
is = getEventQuery
