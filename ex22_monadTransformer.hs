-- implement a monad transformer for Identity monad:
-- IdentityT m a, wrapping m a
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT ma) = IdentityT (fmap f ma)

instance (Applicative m) => Applicative (IdentityT m) where
    pure = IdentityT . pure
    (IdentityT mf) <*> (IdentityT ma) = IdentityT (mf <*> ma)

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (IdentityT ma) >>= f = IdentityT (ma >>= \a -> runIdentityT (f a))


-- implement a monad transformer for Either monad:
-- EitherT a m b, wrapping m (Either a b)
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT (fmap (fmap f) mea)

instance (Monad m) => Applicative (EitherT e m) where
    pure = EitherT . pure . Right
    (EitherT mef) <*> (EitherT mea) = EitherT $ do
        ef <- mef
        ea <- mea
        return $ case ef of
            Left e -> Left e
            Right f -> fmap f ea

instance (Monad m) => Monad (EitherT e m) where
    return = pure
    (EitherT mea) >>= f = EitherT $ do
        ea <- mea
        case ea of
            Left e  -> return (Left e)
            Right a -> runEitherT (f a)

-- implement a monad transformer for the arrow monad:
-- ContT r m a, wrapping m (r -> a)
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
    fmap f (ContT c) = ContT $ \k -> c (k . f)

instance Applicative (ContT r m) where
    pure a = ContT ($ a)
    (ContT cf) <*> (ContT ca) = ContT $ \k -> cf (\f -> ca (\a -> k (f a)))

instance Monad (ContT r m) where
    return = pure
    (ContT ca) >>= f = ContT $ \k -> ca (\a -> runContT (f a) k)

    