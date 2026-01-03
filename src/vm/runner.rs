use super::{Error, Instruction, Value, VM};
use log::trace;

pub fn run_instruction(vm: &mut VM) -> Result<(), Error> {
    let mut ip: usize = 0;

    while let Some(ins) = read_ins(vm, &mut ip) {
        use Instruction::*;
        trace!("INS: {} - {:?} - {:?}", ip, ins, vm.stack);

        match ins {
            Return => {
                println!("RETURN: {:?}", vm.stack.pop());
                return Ok(());
            }
            Constant => {
                let next_ins = try_read_ins(vm, &mut ip)?;
                let Instruction::Index(offset) = next_ins else {
                    return Err(Error::WrongInstruction(
                        next_ins,
                        Some(Instruction::Index(0)),
                    ));
                };
                let v = read_const(vm, offset)?;
                trace!("READ CONST: {:?}", v);
                vm.stack.push(v.to_owned());
            }
            Negate => {
                let r = op_negate(vm.stack.pop())?;
                vm.stack.push(r);
            }
            Plus => {
                let r = vm.stack.pop();
                let l = vm.stack.pop();
                vm.stack.push(op_plus(l, r)?);
            }
            _ => {
                todo!()
            }
        }
    }

    Ok(())
}

#[allow(irrefutable_let_patterns)]
fn op_negate(v: Value) -> Result<Value, Error> {
    if let Value::Number(f) = v {
        Ok(Value::Number(-f))
    } else {
        Err(Error::Runtime)
    }
}

#[allow(irrefutable_let_patterns)]
fn op_plus(l: Value, r: Value) -> Result<Value, Error> {
    if let (Value::Number(a), Value::Number(b)) = (l, r) {
        Ok(Value::Number(a + b))
    } else {
        Err(Error::Runtime)
    }
}

fn read_ins(vm: &VM, ip: &mut usize) -> Option<Instruction> {
    *ip += 1;
    vm.instructions.get(*ip - 1).copied()
}

fn try_read_ins(vm: &VM, ip: &mut usize) -> Result<Instruction, Error> {
    let old_ip = *ip;
    *ip += 1;
    match vm.instructions.get(old_ip) {
        None => Err(Error::InstructionOutOfRange(old_ip, vm.instructions.len())),
        Some(i) => Ok(*i),
    }
}

fn read_const(vm: &VM, offset: usize) -> Result<&Value, Error> {
    match vm.constants.get(offset) {
        None => Err(Error::DataOutOfRange(offset, vm.constants.len())),
        Some(v) => Ok(v),
    }
}
