use crate::ast::{Expression, Statement, StatementList};
use crate::token::Token;
use derive_more::Display;
use std::collections::HashMap;
use thiserror::Error;

const NUMBER_DELTA: f64 = 1e-10;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Variable or function of name `{0}` has been declared before")]
    ReDeclareVariable(String),

    #[error("Variable or function of name `{0} has not been declared but get re-assigned")]
    NotFoundVariable(String),

    #[error("Unknown operator `{0}`")]
    UnknownOperation(Token),

    #[error("Value `{1}` does not accept operator `{0}`")]
    InvalidOperationOnType(Token, Value),

    #[error("`{0}` and `{1} do not share the same type")]
    MismatchType(Value, Value),

    #[error("Condition evaluated to `{0}` which is not a boolean value")]
    CondNotBool(Value),

    #[error("Divide by 0")]
    DivideByZero,

    #[error("Value `{0}` is not a callable")]
    ValueNotCallable(Value),

    #[error("Callable `{0}` accept {1} number of arguments but receive {2}")]
    WrongNumberOfArgument(String, usize, usize),
}

#[derive(Display, Debug, Clone)]
pub enum Value {
    Nil,
    Str(String),
    Number(f64),
    Bool(bool),
    #[display(fmt = "{:?}", _0)]
    Array(Vec<Value>),

    #[display(fmt = "function")]
    Function(Vec<String>, StatementList),
}

impl Value {
    pub fn from_option(o: Option<Value>) -> Self {
        match o {
            Some(v) => v,
            None => Value::Nil,
        }
    }
}

pub struct Environment<'a> {
    values: HashMap<String, Value>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent<'b>(parent: &'b Environment<'b>) -> Self
    where
        'b: 'a,
    {
        Self {
            values: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get_local_only(&self, key: &'_ str) -> Option<&Value> {
        self.values.get(key)
    }

    pub fn get_include_parent(&self, key: &'_ str) -> Option<&Value> {
        if let Some(value) = self.get_local_only(key) {
            return Some(value);
        }
        self.parent.and_then(|p| p.get_include_parent(key))
    }

    pub fn insert(&mut self, key: String, value: Value) {
        let _ = self.values.insert(key.to_string(), value);
    }

    pub fn change_parent<'b>(&mut self, parent: &'b Environment<'b>)
    where
        'b: 'a,
    {
        self.parent = Some(parent);
    }
}

fn get_value(env: &Environment, name: &str) -> Value {
    env.get_include_parent(name)
        .map(|v| v.to_owned())
        .unwrap_or(Value::Nil)
}

pub fn interpret_stmt(env: &mut Environment, stmt: Statement) -> Result<Option<Value>, Error> {
    match stmt {
        Statement::Print(expr) => interpret_print_stmt(env, expr),
        Statement::Declare(name, expr) => interpret_declare_stmt(env, name, expr),
        Statement::Reassign(name, expr) => interpret_reassign_stmt(env, name, expr),
        Statement::Expr(expr) => interpret_expr(env, expr).map(Some),
        Statement::While(cond, stmts) => interpret_while_stmt(env, cond, stmts),
        Statement::Block(stmts) => interpret_stmt_list(&mut Environment::with_parent(env), stmts),
        Statement::Global(stmts) => interpret_stmt_list(env, stmts),
    }
}

fn interpret_print_stmt(env: &mut Environment, expr: Expression) -> Result<Option<Value>, Error> {
    let value = interpret_expr(env, expr)?;
    println!("{}", value);
    Ok(None)
}

fn interpret_stmt_list(
    env: &mut Environment,
    mut stmts: StatementList,
) -> Result<Option<Value>, Error> {
    if let Some(last) = stmts.pop() {
        for s in stmts {
            interpret_stmt(env, s)?;
        }
        interpret_stmt(env, last)
    } else {
        Ok(None)
    }
}

fn interpret_declare_stmt(
    env: &mut Environment,
    name: String,
    expr: Expression,
) -> Result<Option<Value>, Error> {
    if env.get_local_only(&name).is_some() {
        return Err(Error::ReDeclareVariable(name));
    }
    let value = interpret_expr(env, expr)?;
    env.insert(name, value);
    Ok(None)
}

fn interpret_reassign_stmt(
    env: &mut Environment,
    name: String,
    expr: Expression,
) -> Result<Option<Value>, Error> {
    if env.get_include_parent(&name).is_none() {
        return Err(Error::NotFoundVariable(name));
    }
    let value = interpret_expr(env, expr)?;
    env.insert(name, value.clone());
    Ok(Some(value))
}

fn interpret_while_stmt(
    env: &mut Environment,
    cond: Expression,
    stmts: StatementList,
) -> Result<Option<Value>, Error> {
    let mut result = Ok(None);

    loop {
        let cond_value = interpret_expr(env, cond.clone())?;
        let Value::Bool(bin) = cond_value else {
            return Err(Error::CondNotBool(cond_value));
        };
        if !bin {
            break;
        }
        result = interpret_stmt_list(env, stmts.to_vec());
    }

    result
}

fn interpret_expr(env: &mut Environment, expr: Expression) -> Result<Value, Error> {
    match expr {
        Expression::If(cond, if_stmts, else_stmts) => {
            interpret_if_expr(env, *cond, if_stmts, else_stmts)
        }
        Expression::FunctionCall(name, args) => interpret_function_call(env, name, args),
        Expression::FunctionDeclaration(args, stmts) => Ok(Value::Function(args, stmts)),
        Expression::Identifier(name) => Ok(get_value(env, name.as_str())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Str(v) => Ok(Value::Str(v.to_string())),
        Expression::Bool(v) => Ok(Value::Bool(v)),
        Expression::Number(v) => Ok(Value::Number(v)),
        Expression::Array(exprs) => get_array_value(env, exprs),
        Expression::UnaryOp(expr, op) => interpret_unary_op(env, *expr, op),
        Expression::BinaryOp(lhs, op, rhs) => interpret_binary_op(env, *lhs, op, *rhs),
    }
}

fn get_array_value(env: &mut Environment, exprs: Vec<Expression>) -> Result<Value, Error> {
    let mut result = vec![];
    for expr in exprs {
        result.push(interpret_expr(env, expr)?);
    }
    Ok(Value::Array(result))
}

fn interpret_if_expr(
    env: &mut Environment,
    cond: Expression,
    if_stmts: StatementList,
    else_stmts: Option<StatementList>,
) -> Result<Value, Error> {
    let cond_value = interpret_expr(env, cond)?;
    let Value::Bool(bin) = cond_value else {
        return  Err(Error::CondNotBool(cond_value));
    };
    if !bin {
        return else_stmts
            .map(|stmts| interpret_stmt_list(env, stmts))
            .unwrap_or(Ok(None))
            .map(Value::from_option);
    }

    Ok(Value::from_option(interpret_stmt_list(env, if_stmts)?))
}

fn interpret_function_call(
    env: &mut Environment,
    name: String,
    args: Vec<Expression>,
) -> Result<Value, Error> {
    let Some(fn_value) = env.get_include_parent(&name) else {
        return Err(Error::NotFoundVariable(name));
    };
    let fn_value = fn_value.to_owned();
    let Value::Function(arg_names, body) = fn_value else {
        return Err(Error::ValueNotCallable(fn_value));
    };

    if arg_names.len() != args.len() {
        return Err(Error::WrongNumberOfArgument(
            name,
            arg_names.len(),
            args.len(),
        ));
    }

    let mut local_env = Environment::new();
    for (arg_name, arg_expr) in arg_names.into_iter().zip(args.into_iter()) {
        let value = interpret_expr(env, arg_expr)?;
        local_env.insert(arg_name, value);
    }
    local_env.change_parent(env);

    interpret_stmt_list(&mut local_env, body).map(Value::from_option)
}

fn interpret_unary_op(env: &mut Environment, expr: Expression, op: Token) -> Result<Value, Error> {
    let res = interpret_expr(env, expr)?;
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
    env: &mut Environment,
    lhs: Expression,
    op: Token,
    rhs: Expression,
) -> Result<Value, Error> {
    let lhs = interpret_expr(env, lhs)?;
    let rhs = interpret_expr(env, rhs)?;

    match op {
        Token::Plus => add(&lhs, &rhs),
        Token::Minus => subtract(&lhs, &rhs),
        Token::Star => times(&lhs, &rhs),
        Token::Slash => divide(&lhs, &rhs),
        Token::And | Token::Or => and_or(&lhs, op, &rhs),
        Token::EqualEqual | Token::BangEqual => is_equal(&lhs, &rhs),
        Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
            compare(&lhs, op, &rhs)
        }
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn is_equal(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    Ok(Value::Bool(match lhs {
        // TODO: conpare function pointer
        Value::Function(_, _) => false,
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
        return Err(Error::InvalidOperationOnType(op, lhs.clone()))
    };
    let &Value::Bool(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs.clone()))
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
    let Value::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, lhs.clone()))
    };
    let Value::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    Ok(Value::Number(l - r))
}

fn times(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let (Value::Number(l), Value::Number(r)) = (lhs, rhs) else {
     return Err(Error::InvalidOperationOnType(Token::Star, lhs.clone()))
    };

    Ok(Value::Number(l * r))
}

fn divide(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    let Value::Number(l) = lhs else {
        return Err(Error::InvalidOperationOnType(Token::Slash, lhs.clone()))
    };
    let Value::Number(r) = rhs else {
        return Err(Error::InvalidOperationOnType(Token::Minus, rhs.clone()))
    };

    if *r == 0.0 {
        return Err(Error::DivideByZero);
    }

    Ok(Value::Number(l / r))
}
