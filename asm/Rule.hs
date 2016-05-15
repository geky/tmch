module Rule (many, optional, module Rule) where

import Control.Applicative
import Data.Bifunctor
import Data.Maybe
import Data.List


-- Rule definition and operations
newtype Rule t a = Rule
    { unrule :: forall b . 
        ( a -> Int -> [t] -> b -- accept
        ,      Int -> [t] -> b -- reject
        ) -> Int -> [t] -> b   -- rule
    }

rule :: ([t] -> Rule t a) -> Rule t a
rule x = Rule $ \c m ts -> unrule (x ts) c m ts

accept :: Int -> a -> Rule t a
accept n x = Rule $ \(a,_) m ts -> a x (n+m) (drop n ts)

reject :: Rule t a
reject = Rule $ \(_,r) m ts -> r m ts


instance Functor (Rule t) where
    fmap f x = Rule $ \(a,r) -> unrule x (a.f,r)

instance Applicative (Rule t) where
    pure = accept 0
    x <*> y = Rule $ \(a,r) -> unrule x (\f -> unrule y (a.f,r), r)

instance Monad (Rule t) where
    return = pure
    fail _ = empty
    x >>= y = Rule $ \(a,r) -> unrule x (\z -> unrule (y z) (a,r), r)

instance Alternative (Rule t) where
    empty = reject
    x <|> y = Rule $ \(a,r) n -> do
        let c = (a, \m -> if m == n then unrule y (a,r) m else r m)
        unrule x c n

-- many is already defined in Control.Applicative
many1 :: Alternative f => f a -> f [a]
many1 = some

delimited, delimited1 :: Alternative f => f a -> f b -> f [a]
delimited  r s = delimited1 r s <|> pure []
delimited1 r s = (:) <$> r <*> many (s *> r)

terminated, terminated1 :: Alternative f => f a -> f b -> f [a]
terminated  r s = many  (r <* s)
terminated1 r s = many1 (r <* s)

separated, separated1 :: Alternative f => f a -> f b -> f [a]
separated  r s = many s *> ((:) <$> r <*> rest <|> pure [])
  where rest = s *> separated r s <|> pure []
separated1 r s = many s *> ((:) <$> r <*> rest)
  where rest = s *> separated r s <|> pure []

suffixM :: (Alternative f, Monad f) => f a -> (a -> f a) -> f a
suffixM r x = r >>= \a -> suffixM (x a) x <|> pure a

suffix :: Alternative f => f a -> f (a -> a) -> f a
suffix r s = r <**> (foldl (flip (.)) id <$> many s)

prefix :: Alternative f => f (a -> a) -> f a -> f a
prefix  p r = (foldr (.) id <$> many p) <*> r


-- rule modifiers
look :: Rule t a -> Rule t a
look x = Rule $ \(a,r) n ts -> do
    let c = (\z _ _ -> a z n ts, \_ _ -> r n ts)
    unrule x c n ts

try :: Rule t a -> Rule t a
try x = Rule $ \(a,r) n ts -> do
    let c = (a, \_ _ -> r n ts)
    unrule x c n ts

over :: (t -> s) -> Rule s a -> Rule t a
over f x = Rule $ \(a,r) n ts -> do
    let c = (\z m _ -> a z m $ drop (m-n) ts, \m _ -> r m $ drop (m-n) ts)
    unrule x c n $ map f ts

overM :: (Applicative m, Traversable m) => Rule t a -> Rule (m t) (m a)
overM x = Rule $ \(a,r) n ts -> do
    let c = (\z m _ -> (Just z,m), \m _ -> (Nothing,m))
    let x' = unrule x c n <$> sequenceA ts
    let (z,m) = bimap sequenceA maximum (fst <$> x', snd <$> x')
    maybe r a z m $ drop (m-n) ts


-- general rules
current :: Rule t t
current = look matchAny

match :: Eq t => t -> Rule t t
match t = matchIf (== t)

matches :: Eq t => [t] -> Rule t [t]
matches ts = rule $ \case
    ts' | isPrefixOf ts ts' -> accept (length ts) ts
    _                       -> reject

matchIf :: (t -> Bool) -> Rule t t
matchIf p = rule $ \case
    t:_ | p t -> accept 1 t
    _         -> reject

matchAny :: Rule t t
matchAny = matchIf (const True)

matchMaybe :: (t -> Maybe a) -> Rule t a
matchMaybe f = fromJust . f <$> matchIf (isJust . f)


-- Running the actual rules
run :: Rule t a -> [t] -> Either [t] a
run x = unrule x (accept, reject) 0
  where
    accept a _ [] = Right a
    accept _ _ ts = Left ts
    reject   _ ts = Left ts

