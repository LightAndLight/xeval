module Errors where

orElseM :: Monad m => m (Maybe a) -> m a -> m a
orElseM mma alt =
  maybe alt pure =<< mma

onErrorM :: Monad m => m (Either e a) -> (e -> m a) -> m a
onErrorM mma f =
  either f pure =<< mma
