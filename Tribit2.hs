{-# LANGUAGE TypeFamilies, ViewPatterns, FlexibleInstances #-}

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import System.Environment

data Bit = B0 | B1

data Triangle a = Triangle a a a a

instance Functor Triangle where
    fmap f (Triangle a1 a2 a3 a4) = Triangle (f a1) (f a2) (f a3) (f a4)

sequenceA :: (Applicative f) => Triangle (f a) -> f (Triangle a)
sequenceA (Triangle a1 a2 a3 a4) = Triangle <$> a1 <*> a2 <*> a3 <*> a4

traverse :: (Applicative f) => (a -> f b) -> Triangle a -> f (Triangle b)
traverse f t = sequenceA $ fmap f t

reverseT :: Triangle a -> Triangle a
reverseT (Triangle a1 a2 a3 a4) = (Triangle a4 a3 a2 a1)

data Tribit a
    = Bit a
    | Tri (Triangle (Tribit a))

isBit :: Tribit Bit -> Bool
isBit (Bit _) = True
isBit _ = False

bit :: Tribit Bit -> Maybe Bit
bit (Bit b) = Just b
bit _ = Nothing

triangleBit :: Tribit Bit -> Maybe (Triangle Bit)
triangleBit (Tri t) = traverse bit t
triangleBit _ = Nothing

{-------------------- Rewriting --------------------}

rewriteReverse :: Triangle Bit -> Triangle Bit
rewriteReverse x =
    case x of
      Triangle B0 B0 B0 B0 -> Triangle B0 B0 B0 B0
      Triangle B0 B0 B0 B1 -> Triangle B1 B0 B0 B0
      Triangle B0 B0 B1 B0 -> Triangle B0 B0 B0 B1
      Triangle B0 B0 B1 B1 -> Triangle B0 B0 B1 B0
      Triangle B0 B1 B0 B0 -> Triangle B0 B0 B0 B0
      Triangle B0 B1 B0 B1 -> Triangle B0 B0 B1 B0
      Triangle B0 B1 B1 B0 -> Triangle B1 B0 B1 B1
      Triangle B0 B1 B1 B1 -> Triangle B1 B0 B1 B1
      Triangle B1 B0 B0 B0 -> Triangle B0 B1 B0 B0
      Triangle B1 B0 B0 B1 -> Triangle B0 B1 B0 B1
      Triangle B1 B0 B1 B0 -> Triangle B0 B1 B1 B1
      Triangle B1 B0 B1 B1 -> Triangle B1 B1 B1 B1
      Triangle B1 B1 B0 B0 -> Triangle B1 B1 B0 B1
      Triangle B1 B1 B0 B1 -> Triangle B1 B1 B1 B0
      Triangle B1 B1 B1 B0 -> Triangle B0 B1 B1 B1
      Triangle B1 B1 B1 B1 -> Triangle B1 B1 B1 B1

rewrite :: Tribit Bit -> Maybe (Tribit Bit)
rewrite (triangleBit -> Just t) = Just .
                                  Tri .
                                  fmap Bit .
                                  reverseT .
                                  rewriteReverse .
                                  reverseT $ t
rewrite (Tri t) = Tri <$> traverse rewrite t
rewrite _ = Nothing

{-------------------- Reducing --------------------}

reduce1 :: Tribit Bit -> Maybe (Tribit Bit)
reduce1 (triangleBit -> Just (Triangle B0 B0 B0 B0)) = Just $ Bit B0
reduce1 (triangleBit -> Just (Triangle B1 B1 B1 B1)) = Just $ Bit B1
reduce1 (Tri t) = Tri <$> traverse reduce1 t
reduce1 _ = Nothing

reduce :: Tribit Bit -> Maybe (Tribit Bit)
reduce = loop Nothing
    where
      loop :: Maybe (Tribit Bit) -> Tribit Bit -> Maybe (Tribit Bit)
      loop prev (reduce1 -> Just t) = loop (Just t) t
      loop prev _ = prev

{-------------------- Utils --------------------}

char2bit :: Char -> Maybe Bit
char2bit '0' = Just B0
char2bit '1' = Just B1
char2bit _ = Nothing

bit2char :: Bit -> Char
bit2char B0 = '0'
bit2char B1 = '1'

slice :: Int -> Int -> [a] -> [a]
slice min max = drop min . take max

split :: [a] -> [[a]]
split = get 1 . reverse
    where
      get _ [] = []
      get len xs = take len xs : get (2 + len) (drop len xs)

rconcat :: [[a]] -> [a]
rconcat = reverse . concat

{-------------------- Reading Tribit --------------------}

load :: [[Bit]] -> Maybe (Tribit Bit)
load [] = Nothing
load [[bit]] = Just (Bit bit)
load [_] = Nothing
load bits =
    let height = length bits
        -- width of rectangle of two triangles
        width = height
        (bits1, bits234) = splitAt (height `div` 2) bits
        helper len line = (take len line ,
                           slice len width line ,
                           drop width line)
        (bits2, bits3, bits4) = unzip3 $ zipWith helper [1,3..] bits234
    in Tri <$> traverse load (Triangle bits1 bits2 (reverse bits3) bits4)

fromString :: String -> Maybe (Tribit Bit)
fromString = mapM char2bit >=> load . split

{-------------------- Printing Tribit --------------------}

unload :: Tribit Bit -> [[Bit]]
unload (Bit bit) = [[bit]]
unload (Tri (Triangle a1 a2 a3 a4)) =
    let [t1, t2, t3, t4] = map unload [a1, a2, a3, a4]
        t234 = zipWith3 (\l2 l3 l4 -> l2 ++ l3 ++ l4) t2 (reverse t3) t4
    in t1 ++ t234

toString :: Tribit Bit -> String
toString t = map bit2char . rconcat . unload $ t

{-------------------- Running Tribit --------------------}

instance Show (Tribit Bit) where
    show = toString

onlyOne :: [a] -> Maybe a
onlyOne [a] = Just a
onlyOne _ = Nothing

run :: Tribit Bit -> IO ()
run t = do print t
           unless (isBit t)
                  (run $ fromMaybe
                           (error "Runtime error: wrong bit length")
                           (reduce t <|> rewrite t))

main :: IO ()
main = do a <- getArgs
          run $ fromMaybe
                  (error "Error\nUsage:\n\tTribit <list of bits of length 4^n>")
                  (onlyOne a >>= fromString)
