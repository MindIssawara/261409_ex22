
import Control.Monad (liftM)
import Control.Monad.Trans.Class

-- implement a monad transformer for Identity monad:
-- IdentityT m a, wrapping m a
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT ma) = IdentityT (fmap f ma)

instance Applicative m => Applicative (IdentityT m) where
    pure = IdentityT . pure
    (IdentityT mf) <*> (IdentityT ma) = IdentityT (mf <*> ma)

instance Monad m => Monad (IdentityT m) where
    return = pure
    (IdentityT ma) >>= f = IdentityT (ma >>= runIdentityT . f)

instance MonadTrans IdentityT where
    lift = IdentityT

-- implement a monad transformer for Either monad:
-- EitherT a m b, wrapping m (Either a b)
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT (fmap (fmap f) mea)

instance Monad m => Applicative (EitherT e m) where
    pure = EitherT . pure . Right
    (EitherT mef) <*> (EitherT mea) = EitherT $ do
        ef <- mef
        ea <- mea
        return $ case ef of
            Left e  -> Left e
            Right f -> fmap f ea

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT mea) >>= f = EitherT $ do
        ea <- mea
        case ea of
            Left e  -> return (Left e)
            Right a -> runEitherT (f a)

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

-- implement a monad transformer for the arrow monad:
-- ContT r m a, wrapping m (r -> a)
newtype ContT r m a = ContT { runContT :: m (r -> a) }

instance Functor m => Functor (ContT r m) where
    fmap f (ContT ma) = ContT (fmap (f .) ma)

instance Applicative m => Applicative (ContT r m) where
    pure x = ContT (pure (const x))
    (ContT mf) <*> (ContT ma) = ContT (pure (<*>) <*> mf <*> ma)

instance Monad m => Monad (ContT r m) where
    return = pure
    (ContT ma) >>= f = ContT $ do
        ra <- ma  -- ra :: r -> a
        return $ \r -> do
            rb <- runContT (f (ra r)) 
            rb r  


instance MonadTrans (ContT r) where
    lift ma = ContT (fmap const ma)
