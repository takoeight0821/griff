# Griff VM

## Value

Integer(int)
Closure(code, env)
Block(tag, size, array of value)
Epsilon

## Stacks

ArgStack stack of arguments
RetStack stack of return point
Env      array of local environment

## Instructions
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