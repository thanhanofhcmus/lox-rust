use super::environment::Environment;
use super::error::Error;
use super::value::Value;
use crate::ast::*;
use crate::interpret::value::{self, BuiltinFn, Function};
use crate::parse;
use crate::token::Token;
use std::path::Path;

const NUMBER_DELTA: f64 = 1e-10;

#[derive(Debug)]
struct StmtReturn {
    value: Option<Value>,
    should_bubble_up: bool,
}

impl StmtReturn {
    fn new(value: Value) -> Self {
        Self {
            value: Some(value),
            should_bubble_up: false,
        }
    }

    fn none() -> Self {
        Self {
            value: None,
            should_bubble_up: false,
        }
    }

    fn should_bubble_up(mut self) -> Self {
        self.should_bubble_up = true;
        self
    }
}

impl From<StmtReturn> for Option<Value> {
    fn from(ret: StmtReturn) -> Self {
        ret.value
    }
}

pub struct Interpreter<'cl, 'sl> {
    pub(super) environment: &'cl mut Environment,
    pub(super) input: &'sl str,
}

impl<'cl, 'sl> Interpreter<'cl, 'sl> {
    pub fn new(environment: &'cl mut Environment, input: &'sl str) -> Self {
        Self { environment, input }
    }

    pub fn interpret(&mut self, stmt: &'sl Statement) -> Result<Value, Error> {
        self.interpret_stmt(stmt)
            .map(|r| r.value.unwrap_or(Value::Nil))
    }

    fn interpret_stmt(&mut self, stmt: &Statement) -> Result<StmtReturn, Error> {
        match stmt {
            Statement::Import(node) => self.interpret_import_stmt(node),
            Statement::Declare(name, expr) => self.interpret_declare_stmt(name, expr),
            Statement::ReassignIden(name, expr) => self.interpret_reassign_id_stmt(name, expr),
            Statement::ReassignIndex(node) => self.interpret_reassign_index_stmt(node),
            Statement::Return(expr) => self
                .interpret_expr(expr)
                .map(StmtReturn::new)
                .map(StmtReturn::should_bubble_up),
            Statement::Expr(expr) => self.interpret_expr(expr).map(StmtReturn::new),
            Statement::While(node) => self.interpret_while_stmt(node),
            Statement::If(node) => self.interpret_if_stmt(node),
            Statement::Block(stmts) => {
                self.environment.push_scope();
                let res = self.interpret_stmt_list(stmts);
                self.environment.pop_scope();
                res
            }
            Statement::Global(stmts) => self.interpret_stmt_list(stmts),
        }
    }

    fn interpret_import_stmt(&mut self, node: &ImportNode) -> Result<StmtReturn, Error> {
        if !node.iden.is_simple() {
            // TODO: move this to the parser part
            panic!("import Identifier must be simple");
        }

        // check if we import this yet
        let module_id = node.iden.get_id();
        if self.environment.contains_module(module_id) {
            return Ok(StmtReturn::none());
        }

        let name = node.iden.name.string_from_source(self.input);
        let path = node.path.string_from_source(self.input);

        // read the file
        // TODO: move file loader to an interface
        let file_path = Path::new(&path);
        if !file_path.exists() && !file_path.is_file() {
            return Err(Error::ModuleNotFoundInPath(name, path));
        }

        let content = match std::fs::read_to_string(file_path) {
            Ok(v) => v,
            Err(err) => return Err(Error::ReadModuleFailed(name, path, err)),
        };

        // parse the file
        let tokens = match parse::lex(&content) {
            Ok(tokens) => tokens,
            Err(err) => return Err(Error::ParseModuleFailed(name, path, err)),
        };

        let statement = match parse::parse(&content, &tokens) {
            Ok(ast) => ast,
            Err(err) => return Err(Error::ParseModuleFailed(name, path, err)),
        };

        // interpret the file
        // we will kind of need to keep track of the current module stack
        // and the current scope stack
        // everything starts from a module
        self.environment.init_module(module_id);

        let mut itp = Interpreter::new(self.environment, &content);

        match itp.interpret(&statement) {
            Ok(_) => (),
            Err(err) => return Err(Error::InterpretModuleFailed(name, path, Box::new(err))),
        }

        self.environment.deinit_module();

        // TODO: detect circular dependency

        Ok(StmtReturn::none())
    }

    fn interpret_if_stmt(&mut self, node: &IfStmtNode) -> Result<StmtReturn, Error> {
        let cond = self.is_truthy(&node.cond)?;
        if !cond {
            return node
                .else_stmts
                .as_ref()
                .map(|stmts| self.interpret_stmt_list(stmts))
                .unwrap_or(Ok(StmtReturn::none()));
        }
        self.interpret_stmt_list(&node.if_stmts)
    }

    fn interpret_stmt_list(&mut self, stmts: &[Statement]) -> Result<StmtReturn, Error> {
        for stmt in stmts {
            let ret = self.interpret_stmt(stmt)?;
            if ret.should_bubble_up {
                return Ok(ret);
            }
        }
        Ok(StmtReturn::none())
    }

    fn interpret_declare_stmt(
        &mut self,
        iden: &IdentifierNode,
        expr: &Expression,
    ) -> Result<StmtReturn, Error> {
        if self.environment.get_variable_current_scope(iden).is_some() {
            return Err(Error::ReDeclareVariable(iden.create_name(self.input)));
        }
        let value = self.interpret_expr(expr)?;
        self.environment.insert_variable_current_scope(iden, value);
        Ok(StmtReturn::none())
    }

    fn interpret_reassign_id_stmt(
        &mut self,
        iden: &IdentifierNode,
        expr: &Expression,
    ) -> Result<StmtReturn, Error> {
        if self.environment.get_variable_current_scope(iden).is_none() {
            return match self.environment.get_variable_all_scope(iden) {
                Some(_) => Err(Error::VariableReadOnly(iden.create_name(self.input))),
                None => Err(Error::NotFoundVariable(iden.create_name(self.input))),
            };
        }
        let value = self.interpret_expr(expr)?;
        self.environment.insert_variable_current_scope(iden, value);
        Ok(StmtReturn::none())
    }

    fn interpret_reassign_index_stmt(
        &mut self,
        node: &ReAssignIndexNode,
    ) -> Result<StmtReturn, Error> {
        let indexee_val = self.interpret_expr(&node.indexee)?;
        let idx = prepare_indexee(&indexee_val)?;

        let value = self.interpret_expr(&node.expr)?;

        let Expression::Identifier(ref indexer_id) = node.indexer else {
            // TODO: more concrete error
            return Err(Error::InvalidExpressionType(
                "Identifier".to_string(),
                node.indexer.to_owned(),
            ));
        };
        let Some(indexer_arr) = self.environment.get_variable_current_scope_mut(indexer_id) else {
            return Err(Error::NotFoundVariable(indexer_id.create_name(self.input)));
        };
        let Value::Array(ref mut arr) = indexer_arr else {
            return Err(Error::ValueUnIndexable(indexer_arr.clone()));
        };

        if arr.len() < idx {
            return Err(Error::ArrayOutOfBound(
                indexer_id.create_name(self.input),
                arr.len(),
                idx,
            ));
        }

        arr[idx] = value;

        Ok(StmtReturn::none())
    }

    fn interpret_while_stmt(
        &mut self,
        WhileNode { cond, body }: &WhileNode,
    ) -> Result<StmtReturn, Error> {
        let mut result = StmtReturn::none();

        loop {
            let bin = self.is_truthy(cond)?;
            if !bin {
                break;
            }
            result = self.interpret_stmt_list(body)?;
        }

        Ok(result)
    }

    fn interpret_expr(&mut self, expr: &Expression) -> Result<Value, Error> {
        match expr {
            Expression::FnCall(node) => self.interpret_fn_call_expr(node),
            Expression::FnDecl(node) => Ok(Value::Function(Function {
                arg_ids: node.arg_names.iter().map(|a| a.get_id()).collect(),
                body: node.body.to_owned(),
            })),
            Expression::Index(node) => self.interpret_index_expr(node),
            Expression::When(nodes) => self.interpret_when_expr(nodes),
            Expression::Ternary(node) => self.interpret_ternary_expr(node),
            Expression::Identifier(node) => match self.environment.get_variable_all_scope(node) {
                Some(value) => Ok(value.to_owned()),
                None => Ok(Value::Nil),
            },
            Expression::Nil => Ok(Value::Nil),
            Expression::Str(v) => Ok(Value::Str(v.string_from_source(self.input))),
            Expression::Bool(v) => Ok(Value::Bool(*v)),
            Expression::Number(v) => Ok(Value::Number(*v)),
            Expression::ArrayList(exprs) => self.make_array_value(exprs),
            Expression::ArrayRepeat(node) => self.make_array_repeat(node),
            Expression::UnaryOp(expr, op) => self.interpret_unary_op(expr, *op),
            Expression::BinaryOp(node) => self.interpret_binary_op(node),
        }
    }

    fn interpret_ternary_expr(&mut self, node: &TernaryExprNode) -> Result<Value, Error> {
        let cond = self.is_truthy(&node.cond)?;
        if cond {
            self.interpret_expr(&node.true_expr)
        } else {
            self.interpret_expr(&node.false_expr)
        }
    }

    fn make_array_value(&mut self, exprs: &Vec<Expression>) -> Result<Value, Error> {
        let mut result = Vec::with_capacity(exprs.len());
        for expr in exprs {
            result.push(self.interpret_expr(expr)?);
        }
        Ok(Value::Array(result))
    }

    fn make_array_repeat(&mut self, node: &ArrayRepeatNode) -> Result<Value, Error> {
        let value = self.interpret_expr(&node.value)?;
        let repeat_val = self.interpret_expr(&node.repeat)?;
        let repeat = is_usize(&repeat_val)?;

        let mut result = Vec::with_capacity(repeat);
        for _ in 0..repeat {
            result.push(value.clone());
        }
        Ok(Value::Array(result))
    }

    fn interpret_index_expr(&mut self, node: &IndexExprNode) -> Result<Value, Error> {
        let indexer_val = self.interpret_expr(&node.indexer)?;
        let indexee_val = self.interpret_expr(&node.indexee)?;
        index(&indexer_val, &indexee_val)
    }

    fn interpret_fn_call_expr(&mut self, node: &FnCallNode) -> Result<Value, Error> {
        let Some(fn_value) = self.environment.get_variable_all_scope(&node.iden) else {
            return Err(Error::NotFoundVariable(node.iden.create_name(self.input)));
        };

        match fn_value {
            Value::Function(function) => {
                // TODO: remove this clone
                let function = function.clone();
                self.interpret_normal_fn_call_expr(node, &function)
            }
            Value::BuiltinFunction(function) => self.intepret_builtin_fn_call_expr(node, *function),
            _ => Err(Error::ValueNotCallable(fn_value.to_owned())),
        }
    }

    fn interpret_normal_fn_call_expr(
        &mut self,
        node: &FnCallNode,
        value::Function { arg_ids, body }: &Function,
    ) -> Result<Value, Error> {
        if arg_ids.len() != node.args.len() {
            return Err(Error::WrongNumberOfArgument(
                node.iden.create_name(self.input),
                arg_ids.len(),
                node.args.len(),
            ));
        }

        // create a new stack for the function call
        // currently, later args can reference sonner args
        // TODO: make this go away
        self.environment.push_scope();
        for (arg_id, arg_expr) in arg_ids.iter().zip(node.args.iter()) {
            let value = self.interpret_expr(arg_expr)?;
            self.environment
                .insert_variable_current_scope_by_id(*arg_id, value);
        }
        let result = self
            .interpret_stmt_list(body)
            .map(|r| r.value.unwrap_or(Value::Nil));
        self.environment.pop_scope();

        result
    }

    fn intepret_builtin_fn_call_expr(
        &mut self,
        node: &FnCallNode,
        function: BuiltinFn,
    ) -> Result<Value, Error> {
        let mut args = vec![];
        for expr in &node.args {
            let value = self.interpret_expr(expr)?;
            args.push(value);
        }
        function(self, args)
    }

    fn interpret_unary_op(&mut self, expr: &Expression, op: Token) -> Result<Value, Error> {
        let res = self.interpret_expr(expr)?;
        match op {
            Token::Bang => match res {
                Value::Bool(v) => Ok(Value::Bool(!v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            Token::Minus => match res {
                Value::Number(v) => Ok(Value::Number(-v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_binary_op(
        &mut self,
        BinaryOpNode { lhs, op, rhs }: &BinaryOpNode,
    ) -> Result<Value, Error> {
        let lhs = self.interpret_expr(lhs)?;
        let rhs = self.interpret_expr(rhs)?;
        let op = *op;

        match op {
            Token::Plus => add(&lhs, &rhs),
            Token::Minus => subtract(&lhs, &rhs),
            Token::Star => times(&lhs, &rhs),
            Token::Slash => divide(&lhs, &rhs),
            Token::Percentage => modulo(&lhs, &rhs),
            Token::And | Token::Or => and_or(&lhs, op, &rhs),
            Token::EqualEqual | Token::BangEqual => is_equal(&lhs, &rhs),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                compare(&lhs, op, &rhs)
            }
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_when_expr(&mut self, cases: &[CaseNode]) -> Result<Value, Error> {
        for CaseNode { cond, expr } in cases {
            let bin = self.is_truthy(cond)?;
            if bin {
                return self.interpret_expr(expr);
            }
        }
        Ok(Value::Nil)
    }

    fn is_truthy(&mut self, expr: &Expression) -> Result<bool, Error> {
        let cond_value = self.interpret_expr(expr)?;
        let Value::Bool(cond_bin) = cond_value else {
            return Err(Error::ConditionNotBool(cond_value));
        };
        Ok(cond_bin)
    }
} // Interpreter

fn compare(lhs: &Value, op: Token, rhs: &Value) -> Result<Value, Error> {
    let from_ord = |o: std::cmp::Ordering| match op {
        Token::Less => Ok(Value::Bool(o.is_lt())),
        Token::LessEqual => Ok(Value::Bool(o.is_le())),
        Token::Greater => Ok(Value::Bool(o.is_gt())),
        Token::GreaterEqual => Ok(Value::Bool(o.is_ge())),
        _ => Err(Error::UnknownOperation(op)),
    };
    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
        return from_ord(l.total_cmp(r));
    }
    if let (Value::Str(l), Value::Str(r)) = (lhs, rhs) {
        return from_ord(l.cmp(r));
    }
    Err(Error::InvalidOperationOnType(op, lhs.clone()))
}

fn and_or(lhs: &Value, op: Token, rhs: &Value) -> Result<Value, Error> {
    let &Value::Bool(l) = lhs else {
        return Err(Error::InvalidOperationOnType(op, lhs.clone()));
    };
    let &Value::Bool(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs.clone()));
    };

    match op {
        Token::And => Ok(Value::Bool(l && r)),
        Token::Or => Ok(Value::Bool(l || r)),
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn add(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    match lhs {
        Value::Number(l) => match rhs {
            Value::Number(r) => Ok(Value::Number(l + r)),
            _ => Err(Error::MismatchType(lhs.clone(), rhs.clone())),
        },
        Value::Str(l) => match rhs {
            Value::Str(r) => Ok(Value::Str(l.to_owned() + r)),
            _ => Err(Error::MismatchType(lhs.clone(), rhs.clone())),
        },
        _ => Err(Error::InvalidOperationOnType(Token::Plus, lhs.clone())),
    }
}

fn subtract(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let l = extract_number(lhs, Token::Minus)?;
    let r = extract_number(rhs, Token::Minus)?;
    Ok(Value::Number(l - r))
}

fn times(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let l = extract_number(lhs, Token::Star)?;
    let r = extract_number(rhs, Token::Star)?;
    Ok(Value::Number(l * r))
}

fn divide(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let l = extract_number(lhs, Token::Slash)?;
    let r = extract_number(rhs, Token::Slash)?;
    if r == 0.0 {
        return Err(Error::DivideByZero);
    }
    Ok(Value::Number(l / r))
}

fn modulo(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let l = extract_number(lhs, Token::Percentage)?;
    let r = extract_number(rhs, Token::Percentage)?;
    Ok(Value::Number(l % r))
}

fn is_equal(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    Ok(Value::Bool(match lhs {
        // TODO: compare function pointer
        Value::Function(_) => false,
        Value::BuiltinFunction(_) => false,
        Value::Nil => false,
        Value::Array(_) => false,
        Value::Bool(l) => match rhs {
            Value::Bool(r) => l == r,
            _ => false,
        },
        Value::Number(l) => match rhs {
            Value::Number(r) => f64::abs(*l - *r) < NUMBER_DELTA,
            _ => false,
        },
        Value::Str(l) => match rhs {
            Value::Str(r) => l == r,
            _ => false,
        },
    }))
}

fn is_usize(value: &Value) -> Result<usize, Error> {
    let Value::Number(idx) = value else {
        return Err(Error::ValueMustBeUsize(value.clone()));
    };
    let idx = *idx;

    // check if idx is a integer
    if idx < 0.0 || idx.fract() != 0.0 {
        return Err(Error::ValueMustBeUsize(value.clone()));
    }
    Ok(idx as usize)
}

fn prepare_indexee(indexee: &Value) -> Result<usize, Error> {
    is_usize(indexee)
}

fn index(indexer: &Value, indexee: &Value) -> Result<Value, Error> {
    let Value::Array(arr) = indexer else {
        return Err(Error::ValueUnIndexable(indexer.clone()));
    };
    let idx = prepare_indexee(indexee)?;
    Ok(arr.get(idx).map(Value::to_owned).unwrap_or(Value::Nil))
}

fn extract_number(v: &Value, token: Token) -> Result<f64, Error> {
    if let Value::Number(n) = v {
        Ok(*n)
    } else {
        Err(Error::InvalidOperationOnType(token, v.clone()))
    }
}
