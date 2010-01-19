{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Tea.Tea ( Tea (..)
               , getT
               , putT
               , modifyT
               ) where
import Control.Monad.State
import Control.Monad.Trans
import Tea.TeaState (TeaState)

newtype Tea s v = Tea { extractTea :: StateT s (StateT TeaState IO) v }

instance Monad (Tea s) where
   return f         = Tea $ return f
   (Tea a) >>= b = Tea $ a >>= extractTea . b

instance MonadState s (Tea s) where
   get = Tea $ get
   put = Tea . put

instance Functor (Tea s) where
   fmap f (Tea v) = Tea $ fmap f v

instance MonadIO (Tea s) where
   liftIO f = Tea $ liftIO f

getT :: Tea s TeaState
getT = Tea $ lift $ get
putT = Tea . lift . put
modifyT = Tea . lift . modify
