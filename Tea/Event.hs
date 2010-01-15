{-# LANGUAGE NoMonomorphismRestriction #-}
module Tea.Event ((+>), eventHandler) where
import Data.Monoid
import qualified Graphics.UI.SDL as SDL
import Tea.Types
import Data.Ix

instance Monoid (Event s) where
  mappend (Event a1 a2 a3 a4 a5 a7 a8 a9 a10 a11 a12 a13) (Event b1 b2 b3 b4 b5 b7 b8 b9 b10 b11 b12 b13) = Event {
    keyDown        = \key mods -> (a1 key mods) >> (b1 key mods),
    keyUp          = \key mods -> (a2 key mods) >> (b2 key mods),
    mouseDown      = \c b    -> (a3 c b) >> (b3 c b),
    mouseUp        = \c b    -> (a4 c b) >> (b4 c b),
    mouseMove      = \c b    -> (a5 c b) >> (b5 c b),
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

z = return ()

(+>) :: Event s -> Event s -> Event s
(+>) = mappend

eventHandler :: Event s
eventHandler = mempty




