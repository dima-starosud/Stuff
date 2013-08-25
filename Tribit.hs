{-# LANGUAGE TypeFamilies, DataKinds, GADTs, ViewPatterns #-}

import Data.Type.Natural
import Data.Maybe
import Control.Monad
import Control.Applicative
import System.Environment

data Bit = B0 | B1

data Triangle a = Triangle a a a a

instance Functor Triangle where
    fmap f (Triangle a1 a2 a3 a4) = Triangle (f a1) (f a2) (f a3) (f a4)

{-
  Passing different n one can build such types:
  a, f a, f (f a), f (f (f a)), and so on.
 -}
type family Compose (n :: Nat) (f :: * -> *) (a :: *) :: *
type instance Compose Z f a = a
type instance Compose (S n) f a = f (Compose n f a)

{-
  Alias for building:
  Bit, Triangle Bit, Triangle (Triangle Bit), and so on.
 -}
type MultiTriangle (n :: Nat) = Compose n Triangle Bit

{-
  All the type-level things are hidden in this GADT.
  (SingI n) and (SNat n) for building and destruction of n.
 -}
data Tribit where
    Tribit :: SingI n => SNat n -> MultiTriangle n -> Tribit

{- For checking if we are done -}
isBit :: Tribit -> Bool
isBit (Tribit SZ _) = True
isBit _ = False

{- Analogue of Traversable sequenceA -}
sequenceA :: (Applicative f) => Triangle (f a) -> f (Triangle a)
sequenceA (Triangle a1 a2 a3 a4) = Triangle <$> a1 <*> a2 <*> a3 <*> a4

reverseT :: Triangle a -> Triangle a
reverseT (Triangle a1 a2 a3 a4) = (Triangle a4 a3 a2 a1)

{-------------------- Rewriting --------------------}

{-
  Rules are given for string represantation of Tribit:
  e.g. 0010 -> 0001
  This rule adopted for (Triangle Bit) data type will be the following:
  Triangle B0 B1 B0 B0 -> Triangle B1 B0 B0 B0
  i.e. in reversed order
 -}
rewriteReverseT :: Triangle Bit -> Triangle Bit
rewriteReverseT x =
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

rewriteMT :: SNat n -> MultiTriangle (S n) -> MultiTriangle (S n)
rewriteMT SZ = reverseT . rewriteReverseT . reverseT
rewriteMT (SS n) = fmap (rewriteMT n)

rewrite :: Tribit -> Tribit
rewrite h@(Tribit SZ _) = h
rewrite (Tribit (SS n) t) = Tribit (SS n) (rewriteMT n t)

{-------------------- Reducing --------------------}

reduceT :: Triangle Bit -> Maybe Bit
reduceT (Triangle B0 B0 B0 B0) = Just B0
reduceT (Triangle B1 B1 B1 B1) = Just B1
reduceT _ = Nothing

reduceMT :: SNat n -> MultiTriangle (S n) -> Maybe (MultiTriangle n)
reduceMT SZ = reduceT
reduceMT (SS n) = sequenceA . fmap (reduceMT n)

{-
  "Infinite" variant of reduce: returned value cannot be reduced.
  Nothing means that entire value cannot be reduced.
  Consider this as return code.
 -}
reduce :: Tribit -> Maybe Tribit
reduce = loop Nothing
    where
      loop :: Maybe Tribit -> Tribit -> Maybe Tribit
      loop prev (Tribit (SS n) (reduceMT n -> Just t)) =
          let t' = Tribit n t in loop (Just t') t'
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

{-
  Builds triangle matrix.
  Every next row is two elements bigger than previous one.
 -}
split :: [a] -> [[a]]
split = get 1 . reverse
    where
      get _ [] = []
      get len xs = take len xs : get (2 + len) (drop len xs)

rconcat :: [[a]] -> [a]
rconcat = reverse . concat

{-------------------- Reading Tribit --------------------}

{-
  Check if n equals to m. If it does returns Just triangle.
 -}
tryCastMT :: SNat n -> SNat m -> MultiTriangle m -> Maybe (MultiTriangle n)
tryCastMT SZ SZ t = Just t
tryCastMT (SS n) (SS m) t = sequenceA $ fmap (tryCastMT n m) t
tryCastMT _ _ _ = Nothing

tryCast :: SNat n -> Tribit -> Maybe (MultiTriangle n)
tryCast n (Tribit m t) = tryCastMT n m t

{-
  Check if 4 input triangles are of the same depth.
  If they are builds from them bigger triangle.
 -}
tribits2tribit :: Triangle Tribit -> Maybe Tribit
tribits2tribit (Triangle (Tribit n t1)
                         (tryCast n -> Just t2)
                         (tryCast n -> Just t3)
                         (tryCast n -> Just t4)
               ) = Just $ Tribit (SS n) (Triangle t1 t2 t3 t4)
tribits2tribit _ = Nothing

maybeTribit :: Triangle (Maybe Tribit) -> Maybe Tribit
maybeTribit = sequenceA >=> tribits2tribit

{-
  Splits big triangle in 4 smaller ones, and tries to build Tribit.
  If there is only one-element matrix then this is a Bit.
  If triangle is empty or there is only one line, which doesn't contain only one element,
  algorithm returns Nothing.
 -}
load :: [[Bit]] -> Maybe Tribit
load [] = Nothing
load [[bit]] = Just (Tribit SZ bit)
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
    in maybeTribit $ fmap load (Triangle bits1 bits2 (reverse bits3) bits4)

fromString :: String -> Maybe Tribit
fromString = mapM char2bit >=> load . split

{-------------------- Printing Tribit --------------------}

unload :: SNat n -> MultiTriangle n -> [[Bit]]
unload SZ bit = [[bit]]
unload (SS n) (Triangle a1 a2 a3 a4) =
    let [t1, t2, t3, t4] = map (unload n) [a1, a2, a3, a4]
        t234 = zipWith3 (\l2 l3 l4 -> l2 ++ l3 ++ l4) t2 (reverse t3) t4
    in t1 ++ t234

toString :: Tribit -> String
toString (Tribit n t) = map bit2char . rconcat $ unload n t

{-------------------- Running Tribit --------------------}

instance Show Tribit where
    show = toString

onlyOne :: [a] -> Maybe a
onlyOne [a] = Just a
onlyOne _ = Nothing

run :: Tribit -> IO ()
run t = do print t
           unless (isBit t)
                  (run $ fromMaybe (rewrite t) (reduce t))

main :: IO ()
main = do a <- getArgs
          run $ fromMaybe
                  (error "Error\nUsage:\n\tTribit <list of bits of length 4^n>")
                  (onlyOne a >>= fromString)
