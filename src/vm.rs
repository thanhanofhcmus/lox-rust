use derive_more::Display;
use thiserror::Error;

use crate::ast::Statement;

mod compiler;
mod runner;

type Index = usize;

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
}

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("generic compile error")]
    Compile,

    #[error("generic runtime error")]
    Runtime,

    #[error("instruction segment of out range, access {0} when only have {1}")]
    InstructionOutOfRange(usize, usize),

    #[error("data segment of out range, access {0} when only have {1}")]
    DataOutOfRange(usize, usize),

    #[error("Wrong instruction, got {} but want {:?}", .0, .1)]
    WrongInstruction(Instruction, Option<Instruction>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
#[repr(u8)]
pub enum Instruction {
    Return,
    Constant,
    Negate,
    Plus,

    // TODO: make this to be a bunch of bit
    Index(Index),
    // Minus,
    // Multiply,
    // Divide,
}

#[derive(Debug)]
struct Stack(Vec<Value>);

pub struct VM {
    instructions: Vec<Instruction>,
    constants: Vec<Value>,
    stack: Stack,
}

impl VM {
    pub fn new() -> Self {
        VM {
            constants: vec![],
            instructions: vec![],
            stack: Stack::new(),
        }
    }
}

impl Stack {
    fn new() -> Self {
        Stack(vec![])
    }

    fn push(&mut self, v: Value) {
        self.0.push(v)
    }

    fn pop(&mut self) -> Value {
        self.0.pop().expect("popped an empty stack")
    }
}

pub fn compile_and_run(vm: &mut VM, stmt: &Statement) -> Result<(), Error> {
    let mut ins = compiler::compile_stmt(vm, stmt)?;
    vm.instructions.append(&mut ins);
    runner::run_instruction(vm)?;
    Ok(())
}
