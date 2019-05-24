# Griff

## 例

```
fib n =
  if n == 0 then 1
  else fib (n - 1) + fib (n - 2)

type List a = Nil | Cons a (List a)

head : List a -> a
head lst =
  case lst of
    | Nil -> error "nil"
    | Cons x _ -> x

sum : List Int -> Int
sum lst =
  case lst of
    | Nil -> 0
    | Cons x xs = x + sum xs

sumAcc lst =
  let rec go lst acc =
    case lst of
    | Nil -> acc
    | Cons x xs -> go xs (x + acc)
  in go lst 0

evenOdd x =
  let rec even x =
    if x == 0 then True
    else odd (x - 1)
  and odd x =
    if x == 0 then False
    else even (x - 1)
  in (even x, odd x)

type Option a = None | Some a

safeHead : List a -> Option a
safeHead lst =
  case lst of
    | Nil -> None
    | Cons x _ -> Some x_
```
