> module DeepSeq where

$Id: DeepSeq.lhs,v 1.1 2003/07/22 22:33:10 eleganesh Exp $

Cut and paste from 
http://www.haskell.org/pipermail/haskell/2001-August/007712.html

> class  DeepSeq a  where
>   deepSeq :: a -> b -> b
>   deepSeq = seq			-- default, for simple cases

> infixr 0 `deepSeq`, $!!

> ($!!) :: (DeepSeq a) => (a -> b) -> a -> b
> f $!! x = x `deepSeq` f x


> instance  DeepSeq ()  where

> instance  (DeepSeq a) => DeepSeq [a]  where
>   deepSeq [] y = y
>   deepSeq (x:xs) y = deepSeq x $ deepSeq xs y

> instance  (DeepSeq a,DeepSeq b) => DeepSeq (a,b)  where
>   deepSeq (a,b) y = deepSeq a $ deepSeq b y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c) => DeepSeq (a,b,c)  where
>   deepSeq (a,b,c) y = deepSeq a $ deepSeq b $ deepSeq c y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) => DeepSeq (a,b,c,d) where
>   deepSeq (a,b,c,d) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e) => DeepSeq (a,b,c,d,e)  where
>   deepSeq (a,b,c,d,e) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f) => DeepSeq (a,b,c,d,e,f)  where
>   deepSeq (a,b,c,d,e,f) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f,DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)  where
>   deepSeq (a,b,c,d,e,f,g) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f $ deepSeq g y

> instance  DeepSeq Bool  where
> instance  DeepSeq Char  where

> instance  (DeepSeq a) => DeepSeq (Maybe a)  where
>   deepSeq Nothing y = y
>   deepSeq (Just x) y = deepSeq x y

> instance  (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)  where
>   deepSeq (Left a) y = deepSeq a y
>   deepSeq (Right b) y = deepSeq b y

> instance  DeepSeq Ordering  where

> instance  DeepSeq Integer  where
> instance  DeepSeq Int  where
> instance  DeepSeq Float  where
> instance  DeepSeq Double  where
