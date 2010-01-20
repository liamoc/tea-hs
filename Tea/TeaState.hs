{-# LANGUAGE MultiParamTypeClasses #-}
-- | Internal only. Do Not Eat.
module Tea.TeaState ( TeaState (..)
                    , EventState (..)
                    ) where
import Data.Map(Map)
import Data.Array(Array)
import Tea.Screen(Screen)
import Tea.Input(KeyCode)

data TeaState = TS { _screen :: Screen, _eventState :: EventState, _fpsCap :: Int, _lastUpdate :: Int, _channels :: Map Int Int}

data EventState = ES { keyCodes    :: Array KeyCode Bool
                     , keysDown    :: Int
                     }
