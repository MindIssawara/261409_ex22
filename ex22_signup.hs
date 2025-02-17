-- modify the signup page example to use EitherT transformer
-- be sure to give helpful error messages

import Data.Char

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

liftEither :: Monad m => Either e a -> EitherT e m a
liftEither = EitherT . return

readEmail :: IO (Either String String)
readEmail = do
    putStrLn "Please enter your email!"
    str <- getLine
    if '@' `elem` str && '.' `elem` str
        then return $ Right str
        else return $ Left "Invalid email format! must contain '@' and '.'"

readPassword :: IO (Either String String)
readPassword = do
    putStrLn "Please enter your password!"
    str <- getLine
    if length str < 8 
        then return $ Left "Password must be at least 8 characters long!"
        else if null (filter isUpper str)
            then return $ Left "Password must contain at least one uppercase letter!"
            else if null (filter isLower str)
                then return $ Left "Password must contain at least one lowercase letter!"
                else return $ Right str

readEmail' :: EitherT String IO String
readEmail' = EitherT readEmail

readPassword' :: EitherT String IO String
readPassword' = EitherT readPassword

signup :: EitherT String IO (String, String)
signup = do
    email <- readEmail'
    password <- readPassword'
    password2 <- readPassword'
    if password == password2
        then return (email, password)
        else liftEither $ Left "Passwords do not match!"

main :: IO ()
main = do
    result <- runEitherT signup
    case result of
        Left err -> putStrLn $ "Signup failed: " ++ err
        Right _  -> putStrLn "Signup success!"
