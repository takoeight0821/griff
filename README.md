# griff

## 作りたいもの

* Haskell風の文法をもつ関数型プログラミング言語
* VM型インタプリタとネイティブコードコンパイラ
* Luaのように他の言語に組み込みやすいVM
* 列多相
* 代数的データ型

## griff-vm

### Value

* Integer(int)
* Closure(code, env)
* Block(tag, size, array of value)
* Epsilon

### Stacks

* ArgStack stack of arguments
* RetStack stack of return point
* Env      array of local environment

### Instructions
| INSTR        | cont | env   | stack                     | return      | cont' | env'             | stack'                           | return'    |
|--------------|------|-------|---------------------------|-------------|-------|------------------|----------------------------------|------------|
| LDI(x)       | c    | e     | s                         | r           | c     | e                | x : s                            | r          |
| ACCESS(i)    | c    | e     | s                         | r           | c     | e                | e[i] : s                         | r          |
| CLOSURE(f)   | c    | e     | s                         | r           | c     | e                | (f, e) : s                       | r          |
| LET          | c    | e     | x : s                     | r           | c     | x : e            | s                                | r          |
| ENDLET       | c    | _ : e | s                         | r           | c     | e                | s                                | r          |
| TEST(t, _)   | _    | e     | 1 : s                     | r           | t     | e                | s                                | r          |
| TEST(_, f)   | _    | e     | 0 : s                     | r           | f     | e                | s                                | r          |
| ADD          | c    | e     | x : y : s                 | r           | c     | e                | x+y : s                          | r          |
| EQ           | c    | e     | x : y : s                 | r           | c     | e                | x=y : s                          | r          |
| BLOCK(t, n)  | c    | e     | x_0 : x_1 : ... : x_n : s | r           | c     | e                | (t, n, [x_0, x_1, ..., x_n]) : s | r          |
| FIELD(i)     | c    | e     | (_, _, xs) : s            | r           | c     | e                | xs[i] : s                        | r          |
| INVOKE(t, k) | _    | e     | (t, i, xs) : s            | r           | k     | e                | (t, i, xs) : s                   | r          |
| INVOKE(t, _) | c    | e     | (t', i, xs) : s           | r           | c     | e                | (t', i, xs) : s                  | r          |
| APPLY        | c    | e     | (f, e') : v : s           | r           | f     | v : (f, e') : e' | s                                | (c, e) : r |
| TAILAPPY     | c    | e     | (f, e') : v : s           | r           | f     | v : (f, e') : e' | s                                | r          |
| PUSHMARK     | c    | e     | s                         | r           | c     | e                | E : s                            | r          |
| GRAB         | c    | e     | E : s                     | (f, e') : r | f     | e'               | (c, e) : s                       | r          |
| GRAB         | c    | e     | v : s                     | r           | c     | v : (c, e) : e   | s                                | r          |
| RETURN       | c    | e     | x : E : s                 | (f, e') : r | f     | e'               | x : s                            | r          |
| RETURN       | c    | e     | (f, e') : v : s           | r           | f     | v : (f, e') : e' | s                                | r          |

### compile Lambda to Instr

* x, f: variable
* n: positive integer
* b: boolean
* c: character
* s: string
* C: constructor
* e: expr
* op: binop

* C(x, env) = Access (position x env)
* C(n, env) = Ldi n
* C(b, env) = if b then Ldi 1 else Ldi 0
* C(c, env) = Ldi (ord c)
* C(s, env) = _
* C(e e1 .. eN, env) = PushMark; C(eN, env); ...; C(e1, env); C(e, env); Apply
* C(fun x -> e, env) = Grab; T(e, x:_:env)
* C(let x = e1 in e2, env) = C(e1, env); Let; C(e2, x:env); EndLet
* C(let rec f x = e1 in e2, env) = Closure T(e1, x:f:env); Let; C(e2, f:env); EndLet
* C(e1 op e2, env) = C(e2, env); C(e1, env); COp(op)
* C(Sel n e, env) = C(e, env); Field n
* C(Pack tag n, env) = Block tag n

* T(x, env) = Access (position x env); Return
* T(n, env) = Ldi n; Return
* T(b, env) = if b then Ldi 1 else Ldi 0; Return
* T(c, env) = Ldi (ord c); Return
* T(s, env) = _; Return
* T(e e1 .. eN, env) = C(eN, env); ...; C(e1, env); C(e, env); TailApply
* T(fun x -> e, env) = Grab; T(e, x:_:env)
* T(let x = e1 in e2, env) = C(e1, env); Let; T(e2, x:env)
* T(let rec f x = e1 in e2, env) = Closure T(e1, x:f:env); Let; T(e2, f:env)
* T(e1 op e2, env) = C(e2, env); C(e1, env); COp(op); Return
* T(Sel n e, env) = C(e, env); Field n; Return
* T(Pack tag n, env) = Block tag n; Return
