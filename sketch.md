# Griff

## ファイル構造

Language.Griff
- IR
  + Syntax 構文木そのもの
  + HIR    desugerしたもの
  + MIR    ラムダ計算+αのレベルに落としたもの
  + LIR    Cと一対一対応する程度まで落としたもの
- Trans Syntax to SyntaxとかHIR to HIRとか
- Reducer Syntax to HIRとかHIR to MIRとか
  + CodeGen コード生成

## 例

```
fib n =
  if n == 0 then 1
  else fib (n - 1) + fib (n - 2)

type List a = <nil : () | cons : (a, (List a)) >

head : List a -> a
head lst =
  case lst of
    | <nil = _> -> error "nil"
    | <cons = (x, _)> -> x

sum : List Int -> Int
sum lst =
  case lst of
    | <nil = _> -> 0
    | <cons = (x, xs)> -> x + sum xs

sumAcc lst =
  let rec go lst acc =
    case lst of
    | <nil = _> -> acc
    | <cons = (x, xs)> -> go xs (x + acc)
  in go lst 0

evenOdd x =
  let rec even x =
    if x == 0 then true
    else odd (x - 1)
  and odd x =
    if x == 0 then false
    else even (x - 1)
  in (even x, odd x)

type Option a = <none : () | some : a>

None :: forall a. Option a
None = <none = ()> as Option a

Some :: forall a. a -> Option a
Some a = <some = a> as Option a

safeHead : forall a. List a -> Option a
safeHead lst =
  case lst of
    | <nil = _> -> <none = ()> as Option a
    | <cons = (x, _)> -> <some = x> as Option a
```
