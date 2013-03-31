{-# LANGUAGE TupleSections, FlexibleInstances #-}

import Control.Monad
import Control.Applicative
import Data.Char

data Parser a = Parser { parse :: String -> [(a, String)] }

instance Monad Parser where
    p >>= f = Parser $ \s -> do (a, s) <- parse p s
                                parse (f a) s
    return a = Parser $ return . (a,)
    fail _ = failure

instance Functor Parser where
    fmap f a = a >>= return . f

instance Applicative Parser where
    pure = return
    f <*> a = liftM2 ($) f a

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

failure :: Parser a
failure = Parser $ const []

infixl 2 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ p' = Parser $ \s -> parse p s ++ parse p' s

count :: (Int, Int) -> Parser a -> Parser [a]
count (min, max) p = Parser $ reverse . drop min . take_max . reverse . parse (star p) where
    take_max = if max < 0 then id else take $ 1 + max

qmark :: Parser a -> Parser [a]
qmark p = fmap return p +++ return []

plus :: Parser a -> Parser [a]
plus p = liftM2 (:) p (star p)

star :: Parser a -> Parser [a]
star p = plus p +++ return []

invert :: Parser a -> Parser a
invert p = Parser $ reverse . parse p

infixl 8 ??
(??) :: Parser a -> (a -> Bool) -> Parser a
p ?? f = do a <- p
            if f a
              then return a
              else failure

-----------------------------------------------------------

eof :: Parser String
eof = Parser eof' where
    eof' [] = [("", "")]
    eof' (_:_) = []

anychar :: Parser Char
anychar = Parser anychar' where
    anychar' [] = []
    anychar' (c:s) = [(c, s)]

char :: Char -> Parser Char
char c = anychar ?? (c==)

anyof :: String -> Parser Char
anyof s = anychar ?? (`elem` s)

noneof :: String -> Parser Char
noneof s = anychar ?? not . (`elem` s)

digit :: Parser Char
digit = anychar ?? isDigit

number :: Parser Int
number = fmap read $ plus digit

-----------------------------------------------------------

data Tree a = Leaf a | Branch [Tree a]

instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch l) = show l

type Regex = Parser (Tree Char)

class Treeable a where
    to3 :: a -> Tree Char

instance Treeable (Tree Char) where
    to3 = id

instance Treeable Char where
    to3 = Leaf

instance Treeable a => Treeable [a] where
    to3 = Branch . fmap to3

toRe :: Treeable a => Parser a -> Regex
toRe = fmap to3

-----------------------------------------------------------

ifchar :: (Char, a) -> Parser a
ifchar (c, a) = char c $> a

interval :: Parser String
interval = liftM2 enumFromTo anychar (char '-' *> anychar)

anyofRe :: Parser Regex
anyofRe = char '[' *> liftM2 (toRe .) inv syms <* char ']' where
    inv = ifchar ('^', noneof) +++ return anyof
    syms = interval +++ star (noneof "-]")

getCounts :: Parser (Parser a -> Parser [a])
getCounts = fmap count (getCount' +++ getCount'') where
    numForCount :: Parser Int
    numForCount = do s <- star digit
                     return $ if null s then -1 else read s
    getCount' :: Parser (Int, Int)
    getCount' = do x <- char '{' *> number <* char '}'
                   return (x, x)
    getCount'' :: Parser (Int, Int)
    getCount'' = liftM2 (,) (char '{' *> numForCount <* char ',') (numForCount <* char '}')

quantifiers = [('+', plus), ('*', star), ('?', qmark)]

getQuantifier :: Parser (Parser a -> Parser [a])
getQuantifier = foldl1 (+++) $ fmap ifchar quantifiers

getRepetition :: Parser (Parser a -> Parser [a])
getRepetition = liftM2 (flip (.)) rep inv where
    rep = getCounts +++ getQuantifier
    inv = ifchar ('?', invert) +++ return id

serviceSyms = "\\|()[]{}"
metaSyms = [('.', toRe anychar), ('$', toRe eof)]
metaSymsEsc = [('d', toRe digit)]
needEsc = map fst metaSyms ++ map fst quantifiers ++ serviceSyms

escharRe :: Parser Regex
escharRe = char '\\' *> fmap (toRe . char) (anyof needEsc)

metaSymsRe :: Parser Regex
metaSymsRe = char '\\' *> sumRe metaSymsEsc +++ sumRe metaSyms where
    sumRe = foldl1 (+++) . fmap ifchar

charRe :: Parser Regex
charRe = fmap (toRe . char) (noneof needEsc)

basicRe :: Parser Regex
basicRe = liftM2 (flip ($)) re opt where
    re = foldl1 (+++) [parRe, anyofRe, metaSymsRe, escharRe, charRe]
    opt = fmap (toRe .) getRepetition +++ return id
    parRe = char '(' *> fullRe <* char ')'

fullRe :: Parser Regex
fullRe = liftM2 (foldl (+++)) singleRe (star $ char '|' *> singleRe) where
    singleRe = fmap (toRe . sequence) $ plus basicRe

regex :: String -> Regex
regex s = case parse (fullRe <* eof) s of
            (re, _):_  -> re
            _ -> error "Failed to parse regex"
