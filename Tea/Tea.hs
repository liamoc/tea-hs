{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Includes the Tea type and internal state accessors.
module Tea.Tea ( Tea (..)
               , getT
               , putT
               , modifyT
               ) where
import Control.Monad.State
import Control.Monad.Trans
import Tea.TeaState (TeaState)
-- |Copointed monad that provides a State instance and
--  ensures the initialization of hardware devices
--  used by Tea.
newtype Tea s v = Tea { extractTea :: StateT s (StateT TeaState IO) v }

instance Monad (Tea s) where
   return        = Tea . return
   (Tea a) >>= b = Tea $ a >>= extractTea . b

instance MonadState s (Tea s) where
   get = Tea get
   put = Tea . put

instance Functor (Tea s) where
   fmap f (Tea v) = Tea $ fmap f v

instance MonadIO (Tea s) where
   liftIO = Tea . liftIO

getT = Tea $ lift get
putT = Tea . lift . put
modifyT = Tea . lift . modify
