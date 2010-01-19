module Tea.Screen (Screen (..)) where
import Graphics.UI.SDL (Surface)

data Screen = Screen { screenBuffer :: Surface }
