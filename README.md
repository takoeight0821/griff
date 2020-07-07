# griff

## design sketch 

### List.grf

```
data List a = Nil | Cons a (List a)
 
map f xs = xs |> { Nil -> Nil
                 | Cons x xs -> Cons (f x) (map f xs)
                 }
-- Desugared to:
-- FUN map f xs =
--   (\xs -> case xs of
--             Nil -> Nil
--             Cons x xs -> Cons (f x) (map f xs)
--   ) xs
map' f = { Nil -> Nil
         | Cons x xs -> Cons (f x) (map f xs)
         }

sum = { Nil -> 0
      | Cons x xs -> x + sum xs }
```

### List.grfi

```
type List a
map :: (a -> b) -> List a -> List b
sum :: List Int -> Int
```

### Prelude.grf

```
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

if :: Bool -> {a} -> {a} -> a
if c t f = c |> { True -> t!
                | False -> f!
                }
if' = { True t _ -> t!
      | False _ f -> f!
      }

forign import print_string :: String# -> ()
forign import newline :: () -> ()
putStrLn :: String -> ()
putStrLn = { String# str ->
  print_string str;
  newline ()
}
```

### Main.grf

```
module Map :: signature { example :: {name: String, age: Int} }
module Map {
  example :: { name: String, age: Int }
  example = { name: "Taro", age: 14 }
  taroName = example.name
}
import List
import Prelude

forign import print_int :: Int# -> ()
forign import newline :: () -> ()

main :: {()}
main = {
  let lst = Cons 1 <| Cons 2 <| Cons 3 Nil;
  sum (map (+ 1) lst) |> { Int# i -> print_int i };
  newline ();
  putStrLn Map.example.name
}
```

### Main.grfi

```
main :: {()}
```

### entry.c

```c
#include "Main.grfi.h"
int main(void) {
  grf_Main_main()
}
```