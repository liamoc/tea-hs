-- | Includes basic types for mouse buttons and keyboard keys.
module Tea.Input ( Button (..)
                 , KeyCode (..)
                 , Mod (..)
                 , sdlMod
                 , sdlButton
                 , sdlKey
                 ) where

import Data.Array (Ix (..))
import qualified Graphics.UI.SDL as SDL

-- |Type representing all mouse buttons.
data Button = ButtonLeft | ButtonRight | ButtonMiddle | ButtonScrollUp | ButtonScrollDown deriving (Show, Eq, Ord, Enum, Bounded)
-- |Type representing all keys
data KeyCode = KeyUnknown     | KeyFirst     | KeyBackspace    | KeyTab       | KeyClear      | KeyReturn      | KeyPause    | KeyEscape
             | KeySpace       | KeyExclaim   | KeyDoubleQuote  | KeyHash      | KeyDollar     | KeyAmpersand   | KeyQuote    | KeyLeftParen
             | KeyRightParen  | KeyAsterisk  | KeyPlus         | KeyComma     | KeyMinus      | KeyPeriod      | KeySlash    | KeyNum0
             | KeyNum1        | KeyNum2      | KeyNum3         | KeyNum4      | KeyNum5       | KeyNum6        | KeyNum7     | KeyNum8
             | KeyNum9        | KeyColon     | KeySemicolon    | KeyLess      | KeyEquals     | KeyGreater     | KeyQuestion | KeyAt
             | KeyLeftBracket | KeyBackslash | KeyRightBracket | KeyCaret     | KeyUnderscore | KeyBackquote   | KeyA        | KeyB
             | KeyC           | KeyD         | KeyE            | KeyF         | KeyG          | KeyH           | KeyI        | KeyJ
             | KeyK           | KeyL         | KeyM            | KeyN         | KeyO          | KeyP           | KeyQ        | KeyR
             | KeyS           | KeyT         | KeyU            | KeyV         | KeyW          | KeyX           | KeyY        | KeyZ
             | KeyDelete      | KeyPad0      | KeyPad1         | KeyPad2      | KeyPad3       | KeyPad4        | KeyPad5     | KeyPad6
             | KeyPad7        | KeyPad8      | KeyPad9         | KeyPadPeriod | KeyPadDivide  | KeyPadMultiply | KeyPadMinus | KeyPadPlus
             | KeyPadEnter    | KeyPadEquals | KeyUpArrow      | KeyDownArrow | KeyRightArrow | KeyLeftArrow   | KeyInsert   | KeyHome
             | KeyEnd         | KeyPageUp    | KeyPageDown     | KeyF1        | KeyF2         | KeyF3          | KeyF4       | KeyF5
             | KeyF6          | KeyF7        | KeyF8           | KeyF9        | KeyF10        | KeyF11         | KeyF12      | KeyF13
             | KeyF14         | KeyF15       | KeyNumLock      | KeyCapsLock  | KeyScrolLock  | KeyRShift      | KeyLShift   | KeyRCtrl
             | KeyLCtrl       | KeyRAlt      | KeyLAlt         | KeyRMeta     | KeyLMeta      | KeyLSuper      | KeyRSuper   | KeyAltGr
             | KeyCompose     | KeyHelp      | KeyPrint        | KeySysReq    | KeyBreak      | KeyMenu        | KeyPower    | KeyEuro
             | KeyUndo        | KeyLast
             deriving (Show, Eq, Ord, Enum, Bounded)

-- |Type representing all keyboard modifiers
data Mod = ModLeftShift
         | ModRightShift
         | ModLeftCtrl
         | ModRightCtrl
         | ModLeftAlt
         | ModRightAlt
         | ModLeftMeta
         | ModRightMeta
         | ModNumLock
         | ModCapsLock
         | ModAltGr
         | ModCtrl
         | ModShift
         | ModAlt
         | ModMeta deriving (Show, Eq, Ord, Enum, Bounded)

range' (a,b)     = map toEnum $ range (fromEnum a,fromEnum b)
inRange' (a,b)   = inRange (fromEnum a, fromEnum b) . fromEnum
index' (a,b) c   = fromEnum c - fromEnum a

instance Ix KeyCode where
   range   = range'
   inRange = inRange'
   index   = index'

instance Ix Mod where
   range   = range'
   inRange = inRange'
   index   = index'

instance Ix Button where
   range   = range'
   inRange = inRange'
   index   = index'

sdlButton SDL.ButtonLeft      = ButtonLeft
sdlButton SDL.ButtonRight     = ButtonRight
sdlButton SDL.ButtonMiddle    = ButtonMiddle
sdlButton SDL.ButtonWheelUp   = ButtonScrollUp
sdlButton SDL.ButtonWheelDown = ButtonScrollDown

sdlMod SDL.KeyModLeftShift  = ModLeftShift
sdlMod SDL.KeyModRightShift = ModRightShift
sdlMod SDL.KeyModLeftCtrl   = ModLeftCtrl
sdlMod SDL.KeyModRightCtrl  = ModRightCtrl
sdlMod SDL.KeyModLeftAlt    = ModLeftAlt
sdlMod SDL.KeyModRightAlt   = ModRightAlt
sdlMod SDL.KeyModLeftMeta   = ModLeftMeta
sdlMod SDL.KeyModRightMeta  = ModRightMeta
sdlMod SDL.KeyModNum        = ModNumLock
sdlMod SDL.KeyModCaps       = ModCapsLock
sdlMod SDL.KeyModMode       = ModAltGr
sdlMod SDL.KeyModCtrl       = ModCtrl
sdlMod SDL.KeyModShift      = ModShift
sdlMod SDL.KeyModAlt        = ModAlt
sdlMod SDL.KeyModMeta       = ModMeta

modSDL ModLeftShift  = SDL.KeyModLeftShift
modSDL ModRightShift = SDL.KeyModRightShift
modSDL ModLeftCtrl   = SDL.KeyModLeftCtrl
modSDL ModRightCtrl  = SDL.KeyModRightCtrl
modSDL ModLeftAlt    = SDL.KeyModLeftAlt
modSDL ModRightAlt   = SDL.KeyModRightAlt
modSDL ModLeftMeta   = SDL.KeyModLeftMeta
modSDL ModRightMeta  = SDL.KeyModRightMeta
modSDL ModNumLock    = SDL.KeyModNum
modSDL ModCapsLock   = SDL.KeyModCaps
modSDL ModAltGr      = SDL.KeyModMode
modSDL ModCtrl       = SDL.KeyModCtrl
modSDL ModShift      = SDL.KeyModShift
modSDL ModAlt        = SDL.KeyModAlt
modSDL ModMeta       = SDL.KeyModMeta

sdlKey SDL.SDLK_UNKNOWN      = KeyUnknown
sdlKey SDL.SDLK_FIRST        = KeyFirst
sdlKey SDL.SDLK_BACKSPACE    = KeyBackspace
sdlKey SDL.SDLK_TAB          = KeyTab
sdlKey SDL.SDLK_CLEAR        = KeyClear
sdlKey SDL.SDLK_RETURN       = KeyReturn
sdlKey SDL.SDLK_PAUSE        = KeyPause
sdlKey SDL.SDLK_ESCAPE       = KeyEscape
sdlKey SDL.SDLK_SPACE        = KeySpace
sdlKey SDL.SDLK_EXCLAIM      = KeyExclaim
sdlKey SDL.SDLK_QUOTEDBL     = KeyDoubleQuote
sdlKey SDL.SDLK_HASH         = KeyHash
sdlKey SDL.SDLK_DOLLAR       = KeyDollar
sdlKey SDL.SDLK_AMPERSAND    = KeyAmpersand
sdlKey SDL.SDLK_QUOTE        = KeyQuote
sdlKey SDL.SDLK_LEFTPAREN    = KeyLeftParen
sdlKey SDL.SDLK_RIGHTPAREN   = KeyRightParen
sdlKey SDL.SDLK_ASTERISK     = KeyAsterisk
sdlKey SDL.SDLK_PLUS         = KeyPlus
sdlKey SDL.SDLK_COMMA        = KeyComma
sdlKey SDL.SDLK_MINUS        = KeyMinus
sdlKey SDL.SDLK_PERIOD       = KeyPeriod
sdlKey SDL.SDLK_SLASH        = KeySlash
sdlKey SDL.SDLK_0            = KeyNum0
sdlKey SDL.SDLK_1            = KeyNum1
sdlKey SDL.SDLK_2            = KeyNum2
sdlKey SDL.SDLK_3            = KeyNum3
sdlKey SDL.SDLK_4            = KeyNum4
sdlKey SDL.SDLK_5            = KeyNum5
sdlKey SDL.SDLK_6            = KeyNum6
sdlKey SDL.SDLK_7            = KeyNum7
sdlKey SDL.SDLK_8            = KeyNum8
sdlKey SDL.SDLK_9            = KeyNum9
sdlKey SDL.SDLK_COLON        = KeyColon
sdlKey SDL.SDLK_SEMICOLON    = KeySemicolon
sdlKey SDL.SDLK_LESS         = KeyLess
sdlKey SDL.SDLK_EQUALS       = KeyEquals
sdlKey SDL.SDLK_GREATER      = KeyGreater
sdlKey SDL.SDLK_QUESTION     = KeyQuestion
sdlKey SDL.SDLK_AT           = KeyAt
sdlKey SDL.SDLK_LEFTBRACKET  = KeyLeftBracket
sdlKey SDL.SDLK_BACKSLASH    = KeyBackslash
sdlKey SDL.SDLK_RIGHTBRACKET = KeyRightBracket
sdlKey SDL.SDLK_CARET        = KeyCaret
sdlKey SDL.SDLK_UNDERSCORE   = KeyUnderscore
sdlKey SDL.SDLK_BACKQUOTE    = KeyBackquote
sdlKey SDL.SDLK_a            = KeyA
sdlKey SDL.SDLK_b            = KeyB
sdlKey SDL.SDLK_c            = KeyC
sdlKey SDL.SDLK_d            = KeyD
sdlKey SDL.SDLK_e            = KeyE
sdlKey SDL.SDLK_f            = KeyF
sdlKey SDL.SDLK_g            = KeyG
sdlKey SDL.SDLK_h            = KeyH
sdlKey SDL.SDLK_i            = KeyI
sdlKey SDL.SDLK_j            = KeyJ
sdlKey SDL.SDLK_k            = KeyK
sdlKey SDL.SDLK_l            = KeyL
sdlKey SDL.SDLK_m            = KeyM
sdlKey SDL.SDLK_n            = KeyN
sdlKey SDL.SDLK_o            = KeyO
sdlKey SDL.SDLK_p            = KeyP
sdlKey SDL.SDLK_q            = KeyQ
sdlKey SDL.SDLK_r            = KeyR
sdlKey SDL.SDLK_s            = KeyS
sdlKey SDL.SDLK_t            = KeyT
sdlKey SDL.SDLK_u            = KeyU
sdlKey SDL.SDLK_v            = KeyV
sdlKey SDL.SDLK_w            = KeyW
sdlKey SDL.SDLK_x            = KeyX
sdlKey SDL.SDLK_y            = KeyY
sdlKey SDL.SDLK_z            = KeyZ
sdlKey SDL.SDLK_DELETE       = KeyDelete
sdlKey SDL.SDLK_KP0          = KeyPad0
sdlKey SDL.SDLK_KP1          = KeyPad1
sdlKey SDL.SDLK_KP2          = KeyPad2
sdlKey SDL.SDLK_KP3          = KeyPad3
sdlKey SDL.SDLK_KP4          = KeyPad4
sdlKey SDL.SDLK_KP5          = KeyPad5
sdlKey SDL.SDLK_KP6          = KeyPad6
sdlKey SDL.SDLK_KP7          = KeyPad7
sdlKey SDL.SDLK_KP8          = KeyPad8
sdlKey SDL.SDLK_KP9          = KeyPad9
sdlKey SDL.SDLK_KP_PERIOD    = KeyPadPeriod
sdlKey SDL.SDLK_KP_DIVIDE    = KeyPadDivide
sdlKey SDL.SDLK_KP_MULTIPLY  = KeyPadMultiply
sdlKey SDL.SDLK_KP_MINUS     = KeyPadMinus
sdlKey SDL.SDLK_KP_PLUS      = KeyPadPlus
sdlKey SDL.SDLK_KP_ENTER     = KeyPadEnter
sdlKey SDL.SDLK_KP_EQUALS    = KeyPadEquals
sdlKey SDL.SDLK_UP           = KeyUpArrow
sdlKey SDL.SDLK_DOWN         = KeyDownArrow
sdlKey SDL.SDLK_RIGHT        = KeyRightArrow
sdlKey SDL.SDLK_LEFT         = KeyLeftArrow
sdlKey SDL.SDLK_INSERT       = KeyInsert
sdlKey SDL.SDLK_HOME         = KeyHome
sdlKey SDL.SDLK_END          = KeyEnd
sdlKey SDL.SDLK_PAGEUP       = KeyPageUp
sdlKey SDL.SDLK_PAGEDOWN     = KeyPageDown
sdlKey SDL.SDLK_F1           = KeyF1
sdlKey SDL.SDLK_F2           = KeyF2
sdlKey SDL.SDLK_F3           = KeyF3
sdlKey SDL.SDLK_F4           = KeyF4
sdlKey SDL.SDLK_F5           = KeyF5
sdlKey SDL.SDLK_F6           = KeyF6
sdlKey SDL.SDLK_F7           = KeyF7
sdlKey SDL.SDLK_F8           = KeyF8
sdlKey SDL.SDLK_F9           = KeyF9
sdlKey SDL.SDLK_F10          = KeyF10
sdlKey SDL.SDLK_F11          = KeyF11
sdlKey SDL.SDLK_F12          = KeyF12
sdlKey SDL.SDLK_F13          = KeyF13
sdlKey SDL.SDLK_F14          = KeyF14
sdlKey SDL.SDLK_F15          = KeyF15
sdlKey SDL.SDLK_NUMLOCK      = KeyNumLock
sdlKey SDL.SDLK_CAPSLOCK     = KeyCapsLock
sdlKey SDL.SDLK_SCROLLOCK    = KeyScrolLock
sdlKey SDL.SDLK_RSHIFT       = KeyRShift
sdlKey SDL.SDLK_LSHIFT       = KeyLShift
sdlKey SDL.SDLK_RCTRL        = KeyRCtrl
sdlKey SDL.SDLK_LCTRL        = KeyLCtrl
sdlKey SDL.SDLK_RALT         = KeyRAlt
sdlKey SDL.SDLK_LALT         = KeyLAlt
sdlKey SDL.SDLK_RMETA        = KeyRMeta
sdlKey SDL.SDLK_LMETA        = KeyLMeta
sdlKey SDL.SDLK_LSUPER       = KeyLSuper
sdlKey SDL.SDLK_RSUPER       = KeyRSuper
sdlKey SDL.SDLK_MODE         = KeyAltGr
sdlKey SDL.SDLK_COMPOSE      = KeyCompose
sdlKey SDL.SDLK_HELP         = KeyHelp
sdlKey SDL.SDLK_PRINT        = KeyPrint
sdlKey SDL.SDLK_SYSREQ       = KeySysReq
sdlKey SDL.SDLK_BREAK        = KeyBreak
sdlKey SDL.SDLK_MENU         = KeyMenu
sdlKey SDL.SDLK_POWER        = KeyPower
sdlKey SDL.SDLK_EURO         = KeyEuro
sdlKey SDL.SDLK_UNDO         = KeyUndo
sdlKey SDL.SDLK_LAST         = KeyLast
sdlKey _                 = KeyUnknown
