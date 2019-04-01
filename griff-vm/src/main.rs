use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum Value {
    IntVal(i64),
    BoolVal(bool),
    ClosVal(Code, Env),
}

type Env = Vec<Value>;

type Stack = Vec<Value>;

type Code = Vec<Instr>;

#[derive(Debug, Clone)]
pub enum Instr {
    Ldi(i64),
    Ldb(bool),
    Access(usize),
    Closure(Code),
    Apply,
    Return,
    Let,
    EndLet,
    Test(Code, Code),
    Add,
    Eq,
}

#[derive(Debug)]
pub struct Vm {
    code: Code,
    env: Env,
    stack: Stack,
}

impl Vm {
    pub fn new(mut code: Code) -> Self {
        code.reverse(); // popとpushはVecの末尾を変更するため、事前にcodeをreverseしておく必要がある
        Vm {
            code: code,
            env: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn execute(&mut self) {
        while !self.code.is_empty() {
            println!("DUMP");
            println!("CODE: {:?}", self.code);
            println!("ENV: {:?}", self.env);
            println!("STACK: {:?}", self.stack);
            println!("");
            self.next();
        }
    }

    fn next(&mut self) {
        use Instr::*;
        use Value::*;

        if let Some(instr) = self.code.pop() {
            match instr {
                Ldi(n) => self.stack.push(IntVal(n)),
                Ldb(b) => self.stack.push(BoolVal(b)),
                Access(i) => self.stack.push(self.env[self.env.len() - 1 - i].clone()),
                Closure(c) => self
                    .stack
                    .push(ClosVal(c.into_iter().rev().collect(), self.env.clone())),
                Apply => {
                    if let Some(ClosVal(c, env)) = self.stack.pop() {
                        if let Some(v) = self.stack.pop() {
                            self.stack
                                .push(ClosVal(self.code.clone(), self.env.clone()));
                            self.code = c.clone();
                            self.env = env.clone();
                            self.env.push(ClosVal(c, env));
                            self.env.push(v);
                        }
                    }
                }
                Return => {
                    if let Some(v) = self.stack.pop() {
                        if let Some(ClosVal(c, env)) = self.stack.pop() {
                            self.code = c;
                            self.env = env;
                            self.stack.push(v);
                        }
                    }
                }
                Let => {
                    if let Some(v) = self.stack.pop() {
                        self.env.push(v);
                    }
                }
                EndLet => {
                    self.env.pop();
                }
                Test(c1, c2) => {
                    if let Some(BoolVal(b)) = self.stack.pop() {
                        self.code
                            .append(&mut if b { c1 } else { c2 }.into_iter().rev().collect());
                    }
                }
                Add => {
                    if let Some(IntVal(n1)) = self.stack.pop() {
                        if let Some(IntVal(n2)) = self.stack.pop() {
                            self.stack.push(IntVal(n1 + n2));
                        }
                    }
                }
                Eq => {
                    if let Some(IntVal(n1)) = self.stack.pop() {
                        if let Some(IntVal(n2)) = self.stack.pop() {
                            self.stack.push(BoolVal(n1 == n2));
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    use Instr::*;
    let code = vec![
        Closure(vec![
            Ldi(1),
            Access(0),
            Eq,
            Test(
                vec![Ldi(1)],
                vec![Ldi(-1), Access(0), Add, Access(1), Apply, Access(0), Add],
            ),
            Return,
        ]),
        Let,
        Ldi(100),
        Access(0),
        Apply,
        EndLet,
    ];

    println!("{:?}", code);

    let mut vm = Vm::new(code);
    vm.execute();
    println!("{:?}", vm);
}
