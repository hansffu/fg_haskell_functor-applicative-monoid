- [Functors](#orgbb6396c)
  - [Type](#org091db39)
  - [IO Functor](#org9aa7817)
  - [Å compose transformer-funksjonen er det samme som nested fmap](#orgb42edff)
  - [Funksjoner er functors](#org0414c57)
  - [Functor-lover](#orgd814e10)
    - [`fmap id = id`](#org2e9704b)
    - [`fmap (f . g) = fmap f . fmap g`](#orge7dfa6c)
- [Applicative functors](#org81baa1b)
  - [`pure`](#org634ede2)
  - [`(<*>)`](#orgfe09af8)
  - [Maybe](#org2cc7694)
  - [`(<$>)`](#org84452ec)
  - [Lister](#org15deacc)
  - [IO](#orgbe6355b)
  - [Funksjoner](#orgf620d91)
    - [`<*>`](#org2388c4c)
    - [Fungerer også med flere `<*>`](#orgf831fee)
  - [ZipList er også en applicative functor](#orgda66796)
  - [`liftA2`](#org6ac4280)
  - [Applicative-lover](#org3303954)
- [newtype](#orgf8fd3e9)
  - [Deriving](#org02bd94d)
- [Monoid](#org6822c54)
  - [Monoid-lover](#org2c73923)
  - [Liste](#orgab81394)
  - [Tall](#org7568f09)
    - [Product](#orgde4fb32)
    - [Sum](#org0ba41d9)
  - [Boolean](#org5834793)



<a id="orgbb6396c"></a>

# Functors

Functors er ting som kan mappes over. `Maybe` og `List` er eksempler på functors.

```haskell
fmap :: (a -> b) -> f a -> f b
```

Kan sees på som en boks som kan inneholde verdier, men noen functors kan bli litt for abstrakte. Så boken vil heller kalle det en `computational context`


<a id="org091db39"></a>

## Type

Functors har kind `* -> *` og tar derfor en konkret type for å bli en konkret type

```haskell
Maybe Int
IO String
IO ()
```

`Either` tar to typer `Either :: * -> * -> *` og er i seg selv ikke en functor og må derfor defineres som

```haskell
instance Functor (Either a) where
```


<a id="org9aa7817"></a>

## IO Functor

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

`fmap` kan brukes for å slippe å binde variabler

```haskell
main = do line <- getLine
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
```

Kan skrives som

```haskell
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
```


<a id="orgb42edff"></a>

## Å compose transformer-funksjonen er det samme som nested fmap

```haskell
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
```

```shell
echo "hello there" | runhaskell fmapping_io.hs
```


<a id="org0414c57"></a>

## Funksjoner er functors

-   Med prefix notasjon blir `r -> a` til `(->) r a`
-   Den kan derfor sees på som en type constructor med kind `* -> * -> *`
-   Som med Either kan den partially applies

```haskell
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
```

Med litt typemassasje

```haskell
fmap :: (a -> b) -> f a -> f b
# f = (->) r
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

Som er det samme som compose!

```haskell
instance Functor ((->) r) where
    fmap = (.)
```

```haskell
ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (*100) 1
"300"
```


<a id="orgd814e10"></a>

## Functor-lover

-   Alle functors må følge visse lover
-   Det gjør det lettere å lage generelle funksjoner for functors siden man vet akkurat hva de kan gjøre


<a id="org2e9704b"></a>

### `fmap id = id`

Om vi mapper `id` over en functor skal vi få tilbake samme functor

```haskell
ghci> fmap id (Just 3)
Just 3
ghci> id (Just 3)
Just 3
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
```


<a id="orge7dfa6c"></a>

### `fmap (f . g) = fmap f . fmap g`

Å compose to funksjoner for så å mappe den over en functor er det samme som å mappe en funksjon over en functor for så å mappe den andre over resultatet Det vil si at det ikke er noen side effects, som intern state i functors som endrer seg. Se boka for eksempel


<a id="org81baa1b"></a>

# Applicative functors

Utvider functor med en generell konstruktør og muligheten for at funksjonen vi mapper over en functor også er en functor

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```


<a id="org634ede2"></a>

## `pure`

Wrapper en verdi i en functor

```haskell
(pure 5) :: Maybe Int
```


<a id="orgfe09af8"></a>

## `(<*>)`

Som `fmap` men funksjonen vi sender inn kan også være en functor

```haskell
(<*>) (Just (* 3)) (Just 2) -- => Just 6
Just (* 3) <*> Just 2 -- => Just 6
Nothing <*> Just 2 -- => Nothing
```

Nyttig når man ønsker å mappe flere parametere

```haskell
fmap (*) (Just 3) <*> Just 2 -- => Just 6
```


<a id="org2cc7694"></a>

## Maybe

```haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```

```haskell
Just (+3) <*> Just 9 -- => Just 12
pure (+3) <*> Just 10 -- => Just 13
pure (+3) <*> Just 9 -- => Just 12
Just (++"hahah") <*> Nothing -- => Nothing
Nothing <*> Just "woot" -- => Nothing

pure (+) <*> Just 3 <*> Just 5 -- => Just 8
pure (+) <*> Just 3 <*> Nothing -- => Nothing
pure (+) <*> Nothing <*> Just 5 -- => Nothing
```


<a id="org84452ec"></a>

## `(<$>)`

Infiks versjon av `fmap`

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

Så slipper vi noen tegn&#x2026;

```haskell
(+) <$> Just 3 <*> Just 5 -- => Just 8
(+) <$> Just 3 <*> Nothing -- => Nothing
(+) <$> Nothing <*> Just 5 -- => Nothing
```


<a id="org15deacc"></a>

## Lister

```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

```haskell
[(*0),(+100),(^2)] <*> [1,2,3] -- => [0,0,0,101,102,103,1,4,9]
[(+),(*)] <*> [1,2] <*> [3,4] -- => [4,5,5,6,3,4,6,8]
(++) <$> ["ha","heh","hmm"] <*> ["?","!","."] -- => ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
```

Kan brukes som alternativ til list comprehensions

```haskell
[ x*y | x <- [2,5,10], y <- [8,10,11]] -- => [16,20,22,40,50,55,80,100,110]
(*) <$> [2,5,10] <*> [8,10,11] -- => [16,20,22,40,50,55,80,100,110]
```


<a id="orgbe6355b"></a>

## IO

```haskell
instance Applicative IO where
    pure = return
  --(<*>) :: IO (a -> b) -> IO a -> IO b
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

Her kan det også forenkles

```haskell
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction2 :: IO String
myAction2 = (++) <$> getLine <*> getLine
```

Siden resultatet er en IO kan det bindes og brukes videre

```haskell
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
```


<a id="orgf620d91"></a>

## Funksjoner

```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
```

-   `pure` returnerer en funksjon som alltid returnerer `x`
-   `(<*>)` Denne får vi prøve å finne ut av


<a id="org2388c4c"></a>

### `<*>`

```haskell
f <*> g = \x -> f x (g x)
```

```haskell
:t (+) <$> (+3) <*> (*100) -- => (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
(+) <$> (+3) <*> (*100) $ 5 -- => 508
```

Her gjøres `(+)` på resultatet av de to functor/funksjonene når de blir eksekvert med siste argument (`x`) Så med litt substitusjon i lambdaen i `f <*> g`

```haskell
(((+) (+3)) 5) ((*100) 5)
(+) ((+3) 5) ((*100) 5)
```

-   **Med IO eksempelet:** Gjør `(++)` på resultatet av `getLine` og `getLine`
-   **Med Function eksempel:** Gjør `(+)` på resultatet av `f` og `g`

Forskjellen er at med IO returnerer runtime resultatet, med med en fuksjon må vi applye den selv

```haskell
(++) <$> getLine <*> getLine
(+) <$> f <*> g
```


<a id="orgf831fee"></a>

### Fungerer også med flere `<*>`

```haskell
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5 -- => [8.0,10.0,2.5]
```


<a id="orgda66796"></a>

## ZipList er også en applicative functor

```haskell
instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

Shorthand for zipWith

```haskell
getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100] -- => [101,102,103]
getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..] -- => [101,102,103]
getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2] -- => [5,3,3,4]
getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat" -- => [('d','c','r'),('o','a','a'),('g','t','t')]
```


<a id="org6ac4280"></a>

## `liftA2`

Bare en shorthand med fint navn

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```


<a id="org3303954"></a>

## Applicative-lover

```haskell
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
```


<a id="orgf8fd3e9"></a>

# newtype

For å wrappe en eksisterende type inn i en ny type

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

Kan kun ha èn konstruktør og èn verdi. Ellers må man bruke `data`

```haskell
data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession
```


<a id="org02bd94d"></a>

## Deriving

Typen som wrappes må ha de typeklassene som derives

```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

CharList "this will be shown!"  -- => CharList {getCharList = "this will be shown!"}
CharList "benny" == CharList "benny"  -- => True
CharList "benny" == CharList "oisters"  -- => False
```


<a id="org6822c54"></a>

# Monoid

```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```


<a id="org2c73923"></a>

## Monoid-lover

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```


<a id="orgab81394"></a>

## Liste

```haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)
```


<a id="org7568f09"></a>

## Tall

Her er det flere muligheter


<a id="orgde4fb32"></a>

### Product

```haskell
newtype Product a =  Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
```


<a id="org0ba41d9"></a>

### Sum

```haskell
newtype Sum a =  Sum { getSum :: a } deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
```


<a id="org5834793"></a>

## Boolean

```haskell
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)
```