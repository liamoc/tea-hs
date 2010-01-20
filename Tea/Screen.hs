-- | Includes the screen type.
module Tea.Screen (Screen (..)) where
import Graphics.UI.SDL (Surface)

-- | Type that represents the hardware linked drawing buffer.
data Screen = Screen { screenBuffer :: Surface }
