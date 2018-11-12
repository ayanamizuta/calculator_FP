{-# LANGUAGE FlexibleContexts #-}
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))
import Debug.Trace

data Semipoly = Val Int
              | Expo Semipoly Semipoly
              | Mul Semipoly Semipoly
              | Add Semipoly Semipoly
              | Sub Semipoly Semipoly
              | Neg Semipoly
              | Semicircular Int
              | Var Int
              deriving Show

data Varpoly = Value Int
             | Variable String
             
type Monomial = [Int]

-- calculate moments

nck :: Int -> Int -> Int
nck n 0 = 1
nck n k =  (n*(nck (n-1) (k-1))) `div` k
  
moments_sc = [div (nck (2*k) k) (k+1) | k <- (iterate (+1) 1)]

-- caluclate expectations

expand :: Semipoly -> Semipoly -- change to Expo-free style
expand (Expo s (Val 1)) = expand s
expand (Expo s (Val n)) = let s_exp = expand s
                          in Mul s_exp $ expand $ Expo s_exp (Val (n-1))
expand (Mul s1 s2) = Mul (expand s1) (expand s2)
expand (Add s1 s2) = Add (expand s1) (expand s2)
expand (Sub s1 s2) = Sub (expand s1) (expand s2)
expand (Neg s) = Neg $ expand s
expand x = x

monomial_sub :: Semipoly -> Semipoly
monomial_sub s@(Add s1 s2) = monomial s
monomial_sub s@(Sub s1 s2) = monomial s
monomial_sub s@(Mul (Add s1 s2) m) = monomial s
monomial_sub s@(Mul m (Add s1 s2)) = monomial s
monomial_sub s@(Mul (Sub s1 s2) m) = monomial s
monomial_sub s@(Mul m (Sub s1 s2)) = monomial s
monomial_sub x = x
                                  
monomial :: Semipoly -> Semipoly -- change to sum of monomial style
monomial (Add s1 s2) = Add (monomial s1) (monomial s2)
monomial (Sub s1 s2) = Sub (monomial s1) (monomial s2)
monomial (Mul (Add s1 s2) m) = let t1 = monomial $ Mul s1 m
                                   t2 = monomial $ Mul s2 m
                               in Add t1 t2
monomial (Mul m (Add s1 s2)) = let t1 = monomial $ Mul m s1
                                   t2 = monomial $ Mul m s2
                               in Add t1 t2
monomial (Mul (Sub s1 s2) m) = let t1 = monomial $ Mul s1 m
                                   t2 = monomial $ Mul s2 m
                               in Sub t1 t2
monomial (Mul m (Sub s1 s2)) = let t1 = monomial $ Mul m s1
                                   t2 = monomial $ Mul m s2
                               in Sub t1 t2
monomial (Mul m1 m2) = monomial_sub $ Mul (monomial m1) (monomial m2)
monomial x = x

negative :: [(Int,Monomial)] -> [(Int,Monomial)]
negative [] = []
negative ((n,l):xs) = (-n,l):(negative xs)

free_gen :: Semipoly -> [(Int,Monomial)]
free_gen (Add s1 s2) = free_gen s1 ++ free_gen s2
free_gen (Sub s1 s2) = free_gen s1 ++ (free_gen $ Neg s2)
free_gen (Neg s) = negative $ free_gen s
free_gen (Semicircular m) = [(1,[m])]
free_gen (Val n) = [(n,[])]
free_gen (Mul (Val n) s) = let [(recn,l)] = free_gen s in [(n*recn,l)]
free_gen (Mul s (Val n)) = let [(recn,l)] = free_gen s in [(n*recn,l)]
free_gen (Mul (Semicircular m) s) = let [(recn,l)] = free_gen s in [(recn,m:l)]
free_gen (Mul s (Semicircular m)) = let [(recn,l)] = free_gen s in [(recn,l++[m])]
free_gen (Mul s1 s2) = let [(recn1,l1)] = free_gen s1
                           [(recn2,l2)] = free_gen s2
                       in [(recn1*recn2,l1++l2)]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

make_tag_of_interval :: Monomial -> Int -> [Int]
make_tag_of_interval [x] n = [n]
make_tag_of_interval (x:y:xs) n = case x==y of
                                    True -> make_tag_of_interval (y:xs) n
                                    False -> n:(make_tag_of_interval (y:xs) (n+1))

make_interval :: Monomial -> [[Int]]
make_interval mon = init . powerset $ make_tag_of_interval mon 0

make_interval_length :: Monomial -> Int -> [Int]
make_interval_length [x] n = [n+1]
make_interval_length (x:y:xs) n = case x==y of
                                    True -> make_interval_length (y:xs) (n+1)
                                    False -> (n+1):(make_interval_length (y:xs) 0)

elim_rv :: Monomial -> [Int] -> Int -> Monomial
elim_rv l [] _ = l
elim_rv [x] (z:zs) w
  | z == w = []
  | otherwise = [x]
elim_rv (x:y:xs) (z:zs) w
  | w == z = case x == y of     
               True -> elim_rv (y:xs) (z:zs) w -- eliminate x,y
               False -> elim_rv (y:xs) (zs) (w+1) -- eliminate x
  | otherwise = case x == y of
                  True -> x:(elim_rv (y:xs) (z:zs) w)
                  False -> x:(elim_rv (y:xs) (z:zs) (w+1))
                            
                            
expectation_monomial :: Monomial -> Int
expectation_monomial x
  | x == [] = 1
  | x == (take (length x) $ repeat (x !! 0)) = case length x `mod` 2 of
                                               1 -> 0
                                               0 -> moments_sc !! ((length x)`div`2-1)
  | otherwise = let intervals = make_interval x
                    interval_length = make_interval_length x 0
                    cross_prod k interval =
                      case foldl (\r -> \x -> r * (expectation_monomial $ take (interval_length !! x) $ repeat 0)) 1 interval of
                        0 -> k + 0
                        n -> k + (-1)^(1 + length interval) * n * (expectation_monomial $ elim_rv x interval 0)
                in foldl cross_prod 0 intervals

exp_monomial :: Int -> (Int,Monomial) -> Int
exp_monomial n (a,b) = n + a * (expectation_monomial b)

expectation :: Semipoly -> Int
expectation sp = foldl exp_monomial 0 $ free_gen $ monomial $ expand sp

-- parsing

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- many $ digitToInt <$> digit
  return $ Val $ foldl f 0 xs
  where f x y = x*10+y

parens = do
  symbol "("
  result <- expr
  symbol ")"
  return result

semicircular = do
  symbol "s"
  symbol "_"
  xs <- many $ digitToInt <$> digit
  return $ Semicircular $ foldl f 0 xs
  where f x y = x*10+y

chebyshev_polynomial :: Semipoly -> Int -> Semipoly
chebyshev_polynomial (Semicircular n) deg
  | deg == 0 = Val 1
  | deg == 1 = Semicircular n
  | otherwise = Sub (Mul (Semicircular n) (chebyshev_polynomial (Semicircular n) (deg-1))) (chebyshev_polynomial (Semicircular n) (deg-2))

chebyshev = do
  symbol "ch"
  symbol "("
  semi <- semicircular
  symbol ","
  xs <- many $ digitToInt <$> digit
  symbol ")"
  return $ chebyshev_polynomial semi $ foldl f 0 xs
  where f x y = x*10+y

term = try parens <|> try semicircular <|> try chebyshev <|> num

op0 = (const Expo <$> symbol "^")
op1 = (const Mul <$> symbol "*")
op2 = (const Add <$> symbol "+") <|> (const Sub  <$> symbol "-")

expr = do
  spaces
  term `chainl1` op0 `chainl1` op1 `chainl1` op2

fourth_cumulant :: Semipoly -> Int
fourth_cumulant s = let exp = expectation $ s
                        centered_s = Sub s $ Val exp
                    in (expectation $ Expo centered_s $ Val 4) - 2*(expectation $ Expo centered_s $ Val 2)^2

unw :: Either ParseError Semipoly -> Semipoly
unw (Right sp) = sp

main :: IO ()
main = do
  poly <- getLine
  let ast = parse expr "" poly
  putStrLn $ "AST          :" ++ (show ast)
  putStr "Expectation  :"
  putStrLn . show $ expectation $ unw ast
  putStr "4-th cumulant:"
  putStrLn . show $ fourth_cumulant $ unw ast
  

