{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import Data.List
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))
import Debug.Trace

data Semipoly = Val Integer
              | Expo Semipoly Semipoly
              | Mul Semipoly Semipoly
              | Add Semipoly Semipoly
              | Sub Semipoly Semipoly
              | Neg Semipoly
              | Sc Int
              | Var Int
              deriving Show
             
type Monomial = [Int]

-- calculate moments of standard semicircular element

nck :: Integer -> Integer -> Integer
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

negative :: [(Integer,Monomial)] -> [(Integer,Monomial)]
negative [] = []
negative ((n,l):xs) = (-n,l):(negative xs)

free_gen :: Semipoly -> [(Integer,Monomial)]
free_gen (Add s1 s2) = free_gen s1 ++ free_gen s2
free_gen (Sub s1 s2) = free_gen s1 ++ (free_gen $ Neg s2)
free_gen (Neg s) = negative $ free_gen s
free_gen (Sc m) = [(1,[m])]
free_gen (Val n) = [(n,[])]
free_gen (Mul (Val n) s) = let [(recn,l)] = free_gen s in [(n*recn,l)]
free_gen (Mul s (Val n)) = let [(recn,l)] = free_gen s in [(n*recn,l)]
free_gen (Mul (Sc m) s) = let [(recn,l)] = free_gen s in [(recn,m:l)]
free_gen (Mul s (Sc m)) = let [(recn,l)] = free_gen s in [(recn,l++[m])]
free_gen (Mul s1 s2) = let [(recn1,l1)] = free_gen s1
                           [(recn2,l2)] = free_gen s2
                       in [(recn1*recn2,l1++l2)]

max_nums_var :: [(Integer,Monomial)]->Int
max_nums_var [] = 0
max_nums_var ((_,m):xs) = max (foldl max 0 m) $ max_nums_var xs

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
                            
                            
expectation_monomial :: Monomial -> Integer
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

exp_monomial :: Integer -> (Integer,Monomial) -> Integer
exp_monomial n (a,b) = n + a * (expectation_monomial b)

expectation :: Semipoly -> Integer
expectation sp = foldl exp_monomial 0 $ free_gen $ monomial $ expand sp

-- Sch\"utzenberger's algorithm

type OneVarPolynomial = [Integer]

instance Num OneVarPolynomial where
  (+) [] [] = []
  (+) [] ys = ys
  (+) xs [] = xs
  (+) (x:xs) (y:ys) = (x+y):((+) xs ys)
  (*) xs ys = if all (==0) xs
              then []
              else  (map (*(head xs)) ys) + (0 : ((tail xs)*ys))

type Matrix_OVP = [[OneVarPolynomial]]
type PAS = [Matrix_OVP]

bdd_mul :: Int->OneVarPolynomial->OneVarPolynomial->OneVarPolynomial
bdd_mul n xs ys = if all (==0) xs
                  then []
                  else (map (*(head xs)) $ take n ys) + (0 : (bdd_mul (n-1) (tail xs) ys))
                    
comm_add_sub :: [Integer]->Integer->Integer->Integer->Integer
comm_add_sub [] _ _ _ = 0
comm_add_sub (x:xs) const n m = (const^m)*(nck n m)*x + (comm_add_sub xs const n (m-1))

comm_add :: Integer->Integer->[Integer]->Integer -- calculate the n-th moment from 1,...,n-th moment of constant-free part
comm_add n const l = comm_add_sub l const n n

split_const :: Semipoly -> Integer -- valid only monomial_style Semipoly
split_const (Val n) = n
split_const (Add sp1 sp2) = let c1 = split_const sp1
                                c2 = split_const sp2
                            in c1+c2
split_const (Sub sp1 sp2) = let c1 = split_const sp1
                                c2 = split_const sp2
                            in c1-c2
split_const (Mul sp1 sp2) = let c1 = split_const sp1
                                c2 = split_const sp2
                            in c1*c2
split_const _ = 0

degree :: Semipoly -> Int -- valid only monomial_style Semipoly
degree (Val _) = 0
degree (Sc _) = 1
degree (Neg sp) = degree sp
degree (Add sp1 sp2) = let c1 = degree sp1
                           c2 = degree sp2
                       in max c1 c2
degree (Sub sp1 sp2) = let c1 = degree sp1
                           c2 = degree sp2
                       in max c1 c2
degree (Mul sp1 sp2) = let c1 = degree sp1
                           c2 = degree sp2
                       in c1+c2
degree _ = 0

make_zeros :: Int->Matrix_OVP
make_zeros size = [[[]|y<-[1..size]]|z<-[1..size]]

make_ones :: Int->Matrix_OVP
make_ones size = [[if y==z then [1] else []|y<-[1..size]]|z<-[1..size]]

mul_mat :: Matrix_OVP->Matrix_OVP->Matrix_OVP
mul_mat x y = let yt = transpose y
                  size = length x
              in [[foldl (+) [] [(x!!i!!k)*(yt!!j!!k)|k<-enumFromTo 0 $ size-1]|j<-enumFromTo 0 $ size-1]|i<-enumFromTo 0 $ size-1]

bdd_mul_mat :: Int->Matrix_OVP->Matrix_OVP->Matrix_OVP
bdd_mul_mat n x y = let yt = transpose y
                        size = length x
                    in [[foldl (+) [] [bdd_mul n (x!!i!!k) (yt!!j!!k)|k<-enumFromTo 0 $ size-1]|j<-enumFromTo 0 $ size-1]|i<-enumFromTo 0 $ size-1]

add_mat :: Matrix_OVP->Matrix_OVP->Matrix_OVP
add_mat x y = let size = length x in [[(x!!i!!j)+(y!!i!!j)|j<-enumFromTo 0 $ size-1]|i<-enumFromTo 0 $ size-1]

-- based on pas of P_semi in D.Shlyakhtenko's paper
iterate_pas :: PAS->Int->Int->Matrix_OVP->Matrix_OVP 
iterate_pas _ _ 0 x = x
iterate_pas z n iter x = let size = length x
                             ones = make_ones size
                             zeros = make_zeros size
                             var_num = length z
                             zxi = [bdd_mul_mat n (z!!i) $ add_mat x ones|i<-enumFromTo 0 $ var_num-1]
                         in iterate_pas z n (iter-1) $ foldl add_mat zeros [bdd_mul_mat n (zxi!!i) (zxi!!i)|i<-enumFromTo 0 $ var_num-1]

-- the core of algorithm for calculating monoid hom associated with the given rational element

pseudo_inverse :: (Int,PAS)->(Int,PAS)
pseudo_inverse (size,x) = (size,[[[if j==0 then x!!v!!i!!(size-1) else x!!v!!i!!j|j<-enumFromTo 0 $ size-1]|i<-enumFromTo 0 $ size-1]|v<-enumFromTo 0 $ length x-1])

pas_add_sub_ :: Matrix_OVP->Matrix_OVP->Int->Int->Int->Int->OneVarPolynomial
pas_add_sub_ x y n1 n2 i j
  | i>=1 && i<n1+1 && j>=1 && j<n1+1 = x!!(i-1)!!(j-1)
  | i>=1+n1 && i<n1+n2+1 && j>=1+n1 && j<n1+n2+1 = y!!(i-1-n1)!!(j-1-n1)
  | i==0 && j>=1 && j<n1+1 = x!!0!!(j-1)
  | i==0 && j>=1+n1 && j<n1+n2+1 = y!!0!!(j-1-n1)
  | j==n1+n2+1 && i>=1 && i<1+n1 = x!!(i-1)!!(n1-1)
  | j==n1+n2+1 && i>=1+n1 && i<n1+n2+1 = y!!(i-1-n1)!!(n2-1)
  | i==0 && j==n1+n2+1 = x!!0!!(n1-1) + y!!0!!(n2-1)
  | otherwise = []

pas_add_sub :: Matrix_OVP->Matrix_OVP->Matrix_OVP
pas_add_sub x y = let n1 = length x
                      n2 = length y
                  in [[pas_add_sub_ x y n1 n2 i j|j<-enumFromTo 0 $ n1+n2+1]|i<-enumFromTo 0 $ n1+n2+1]

pas_mul_sub :: Matrix_OVP->Matrix_OVP->Matrix_OVP
pas_mul_sub x y = let n1 = length x
                      n2 = length y
                  in [[if i<n1 && j<n1+1 then x!!i!!(min j $n1-1) else if i>=n1 && j>=n1 then y!!(i-n1)!!(j-n1) else []|j<-enumFromTo 0 $ n1+n2-1]|i<-enumFromTo 0 $ n1+n2-1]

pas_add :: Int->(Int,PAS)->(Int,PAS)->(Int,PAS)
pas_add vn (s1,x) (s2,y) = (s1+s2+2,[pas_add_sub (x!!v) (y!!v)|v<-enumFromTo 0 $vn-1])
pas_mul :: Int->(Int,PAS)->(Int,PAS)->(Int,PAS)
pas_mul vn (s1,x) (s2,y) = (s1+s2,[pas_mul_sub (x!!v) (y!!v)|v<-enumFromTo 0 $vn-1])

pas_monomial :: (Integer,Monomial)->Int->(Int,PAS)
pas_monomial (k,[sc_num]) vn = (2,[[[if v==sc_num && i==0 && j==1 then [0,k] else []|j<-[0,1]]|i<-[0,1]]|v<-enumFromTo 0 $vn-1])
pas_monomial (k,(x:xs)) vn = pas_mul vn (2,[[[if v==x && i==0 && j==1 then [1] else []|j<-[0,1]]|i<-[0,1]]|v<-enumFromTo 0 $vn-1]) (pas_monomial (k,xs) vn)

proper_algebraic_system_sub :: [(Integer,Monomial)]->Int->(Int,PAS)
proper_algebraic_system_sub [x] vn = pas_monomial x vn
proper_algebraic_system_sub (x:xs) vn = pas_add vn (pas_monomial x vn) $ proper_algebraic_system_sub xs vn

proper_algebraic_system :: [(Integer,Monomial)]->Int->(Int,PAS)
proper_algebraic_system fg vars_num = pseudo_inverse $ proper_algebraic_system_sub fg vars_num

const_cut :: [(Integer,Monomial)]->[(Integer,Monomial)]
const_cut [] = []
const_cut ((dum,[]):xs) = const_cut xs
const_cut (x:xs) = x:(const_cut xs)

schutzenberger_sub :: Semipoly -> Int -> Int -> Matrix_OVP
schutzenberger_sub sp n iter = let fg_sub = free_gen sp
                                   fg = const_cut fg_sub
                                   vars_num = max_nums_var fg + 1
                                   (size,mats) = proper_algebraic_system fg vars_num
                                   zeros = make_zeros size
                               in iterate_pas mats n iter zeros

schutzenberger :: Semipoly -> Integer -> Integer
schutzenberger sp n = let monomial_style = monomial $ expand sp
                          const = split_const monomial_style
                          iter = degree monomial_style
                          n_ = read (show n) :: Int
                          vars = schutzenberger_sub monomial_style (n_+1) $ iter*n_
                      in comm_add n const (1:(tail.last $ vars !! 0))

-- parsing

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- read <$> many1 digit :: Stream s m Char => ParsecT s u m Integer
  --xs <- many $ digitToInt <$> digit
  return $ Val xs
  --return $ Val $ foldl f 0 xs
  --where f x y = x*10+y

parens = do
  symbol "("
  result <- expr
  symbol ")"
  return result

variable = do
  symbol "a"
  symbol "_"
  xs <- many $ digitToInt <$> digit
  return $ Var $ foldl f 0 xs
  where f x y = x*10+y

semicircular = do
  symbol "s"
  symbol "_"
  xs <- many $ digitToInt <$> digit
  return $ Sc $ foldl f 0 xs
  where f x y = x*10+y

chebyshev_polynomial :: Semipoly -> Int -> Semipoly
chebyshev_polynomial s deg
  | deg == 0 = Val 1
  | deg == 1 = s
  | otherwise = Sub (Mul s (chebyshev_polynomial s (deg-1))) (chebyshev_polynomial s (deg-2))

chebyshev = do
  symbol "ch"
  symbol "("
  semi <- expr
  symbol ","
  xs <- many $ digitToInt <$> digit
  symbol ")"
  return $ chebyshev_polynomial semi $ foldl f 0 xs
  where f x y = x*10+y

term = try parens <|> try variable <|> try semicircular <|> try chebyshev <|> num

op0 = (const Expo <$> symbol "^")
op1 = (const Mul <$> symbol "*")
op2 = (const Add <$> symbol "+") <|> (const Sub  <$> symbol "-")

expr = do
  spaces
  term `chainl1` op0 `chainl1` op1 `chainl1` op2

fourth_cumulant :: Semipoly -> Integer
fourth_cumulant s = let exp = expectation $ s
                        centered_s = Sub s $ Val exp
                    in (expectation $ Expo centered_s $ Val 4) - 2*(expectation $ Expo centered_s $ Val 2)^2

unw :: Either ParseError Semipoly -> Semipoly
unw (Right sp) = sp

naive :: Semipoly -> Integer -> Integer
naive ast n = expectation $ Expo ast (Val n)

main :: IO ()
main = do
  poly <- getLine
  n <- getLine
  let ast = parse expr "" poly
  putStrLn $ "AST          :" ++ (show (unw ast))
  putStrLn . show $ schutzenberger (unw ast) (read n :: Integer)
  --putStrLn . show $ naive (unw ast) (read n :: Integer)
