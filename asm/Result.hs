module Result where

import Prelude hiding (error)
import qualified Prelude
import Data.Bifunctor
import Control.Applicative
import System.IO
import System.Exit


-- Result allows errors and warnings to wrap values
data Result e a
  = Ok a
  | Warning [e] a
  | Error [e]

ok :: b -> Result a b
ok = Ok

warning :: a -> b -> Result a b
warning = Warning . pure

error :: a -> Result a b
error = Error . pure


instance Functor (Result e) where
    fmap f = \case
        Ok a         -> Ok (f a)
        Warning es a -> Warning es (f a)
        Error es     -> Error es

instance Bifunctor Result where
    bimap f g = \case
        Ok a         -> Ok (g a)
        Warning es a -> Warning (map f es) (g a)
        Error es     -> Error (map f es)

instance Foldable (Result e) where
    foldr f b = \case
        Ok a        -> f a b
        Warning _ a -> f a b
        Error _     -> b

instance Traversable (Result e) where
    traverse f = \case
        Ok a         -> Ok <$> f a
        Warning es a -> Warning es <$> f a
        Error es     -> pure (Error es)

instance Applicative (Result e) where
    pure = Ok
    a <*> b = case a of
        Ok a         -> case b of
            Ok b         -> Ok (a b)
            Warning ez b -> Warning ez (a b)
            Error ez     -> Error ez
        Warning es a -> case b of
            Ok b         -> Ok (a b)
            Warning ez b -> Warning (es++ez) (a b)
            Error ez     -> Error (es++ez)
        Error es     -> Error es

instance Alternative (Result e) where
    empty = Error []
    a <|> b = case a of
        Ok a         -> Ok a
        Warning es a -> Warning es a
        Error _      -> b

instance Monad (Result e) where
    return = Ok
    fail m = Error [Prelude.error m]
    a >>= b = case a of
        Ok a         -> b a
        Warning es a -> case b a of
            Ok b         -> Warning es b
            Warning ez b -> Warning (es++ez) b
            Error ez     -> Error (es++ez)
        Error es     -> Error es


result :: Either e a -> Result e a
result = \case
    Left e  -> error e
    Right a -> ok a

check :: Result String a -> IO a
check = \case
    Ok a         -> return a
    Warning es a -> do
        mapM_ (hPutStrLn stderr) es
        return a
    Error es     -> do
        mapM_ (hPutStrLn stderr) es
        exitFailure

force :: Result a b -> b
force = \case
    Ok a        -> a
    Warning _ a -> a
    Error _     -> Prelude.error "Resulted in error"

