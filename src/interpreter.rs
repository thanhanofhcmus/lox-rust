use crate::ast::{
    BinaryOpNode, CaseNode, Expression, FnCallNode, IfStmtNode, IndexExprNode, ReAssignIndexNode,
    Statement, StatementList, TernaryExprNode, WhileNode,
};
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

    #[error("Variable of name `{0}` is readonly in this scope")]
    VariableReadOnly(String),

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

    #[error("Value `{0}` is not of the type array of map, hence indexable")]
    ValueUnIndexabble(Value),

    #[error("Value `{0}` is can not be used as key for array or map")]
    ValueCanBeUsedAsKey(Value),

    #[error("Expect expresstion of type `{0}`, have type `{1:?}`")]
    InvalidExpresstionType(String, Expression),

    #[error("Array of name `{0}` has length `{1}` but receive index `{2}`")]
    ArrayOutOfBound(String, usize, usize),
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

struct Return {
    value: Option<Value>,
    should_bubble_up: bool,
}

impl Return {
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

impl From<Return> for Option<Value> {
    fn from(ret: Return) -> Self {
        ret.value
    }
}

pub struct Context<'cur, 'pa: 'cur> {
    variables: HashMap<String, Value>,
    parent: Option<&'cur Context<'pa, 'pa>>,
}

impl<'cur, 'pa> Context<'cur, 'pa> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: &'pa Context<'cur, 'pa>) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get_local_only(&self, key: &'_ str) -> Option<&Value> {
        self.variables.get(key)
    }

    pub fn get_mut_local_only(&mut self, key: &'_ str) -> Option<&mut Value> {
        self.variables.get_mut(key)
    }

    pub fn get_include_parent(&self, key: &'_ str) -> Option<&Value> {
        if let Some(value) = self.get_local_only(key) {
            return Some(value);
        }
        self.parent.and_then(|p| p.get_include_parent(key))
    }

    pub fn insert(&mut self, key: String, value: Value) {
        let _ = self.variables.insert(key.to_string(), value);
    }
}

fn get_value_owned(ctx: &Context, name: &str) -> Value {
    ctx.get_include_parent(name)
        .map(|v| v.to_owned())
        .unwrap_or(Value::Nil)
}

pub fn interpret(ctx: &mut Context, stmt: &Statement) -> Result<Value, Error> {
    interpret_stmt(ctx, stmt).map(|r| r.value.unwrap_or(Value::Nil))
}

fn interpret_stmt(ctx: &mut Context, stmt: &Statement) -> Result<Return, Error> {
    match stmt {
        Statement::Print(exprs) => interpret_print_stmt(ctx, exprs),
        Statement::Declare(name, expr) => interpret_declare_stmt(ctx, name, expr),
        Statement::ReassignIden(name, expr) => interpret_reassign_id_stmt(ctx, name, expr),
        Statement::ReassignIndex(node) => interpret_reassign_index_stmt(ctx, node),
        Statement::Return(expr) => interpret_expr(ctx, expr)
            .map(Return::new)
            .map(Return::should_bubble_up),
        Statement::Expr(expr) => interpret_expr(ctx, expr).map(Return::new),
        Statement::While(node) => interpret_while_stmt(ctx, node),
        Statement::If(node) => interpret_if_stmt(ctx, node),
        Statement::Block(stmts) => interpret_stmt_list(&mut Context::with_parent(ctx), stmts),
        Statement::Global(stmts) => interpret_stmt_list(ctx, stmts),
    }
}

fn interpret_print_stmt(ctx: &mut Context, exprs: &[Expression]) -> Result<Return, Error> {
    for expr in exprs {
        let value = interpret_expr(ctx, expr)?;
        print!("{} ", value);
    }
    println!();
    Ok(Return::none())
}

fn interpret_if_stmt(ctx: &mut Context, node: &IfStmtNode) -> Result<Return, Error> {
    let cond = is_truthy(ctx, &node.cond)?;
    if !cond {
        return node
            .else_stmts
            .as_ref()
            .map(|stmts| interpret_stmt_list(ctx, stmts))
            .unwrap_or(Ok(Return::none()));
    }
    interpret_stmt_list(ctx, &node.if_stmts)
}

fn interpret_stmt_list(ctx: &mut Context, stmts: &[Statement]) -> Result<Return, Error> {
    for stmt in stmts {
        let ret = interpret_stmt(ctx, stmt)?;
        if ret.should_bubble_up {
            return Ok(ret);
        }
    }
    Ok(Return::none())
}

fn interpret_declare_stmt(
    ctx: &mut Context,
    name: &str,
    expr: &Expression,
) -> Result<Return, Error> {
    let name = name.to_string();
    if ctx.get_local_only(&name).is_some() {
        return Err(Error::ReDeclareVariable(name));
    }
    let value = interpret_expr(ctx, expr)?;
    ctx.insert(name, value);
    Ok(Return::none())
}

fn interpret_reassign_id_stmt(
    ctx: &mut Context,
    name: &str,
    expr: &Expression,
) -> Result<Return, Error> {
    let name = name.to_string();
    if ctx.get_local_only(&name).is_none() {
        return if ctx.get_include_parent(&name).is_some() {
            Err(Error::VariableReadOnly(name))
        } else {
            Err(Error::NotFoundVariable(name))
        };
    }
    let value = interpret_expr(ctx, expr)?;
    ctx.insert(name, value);
    Ok(Return::none())
}

fn interpret_reassign_index_stmt(
    ctx: &mut Context,
    node: &ReAssignIndexNode,
) -> Result<Return, Error> {
    let indexee_val = interpret_expr(ctx, &node.indexee)?;
    let idx = prepare_indexee(&indexee_val)?;

    let value = interpret_expr(ctx, &node.expr)?;

    let Expression::Identifier(ref indexer_id) = node.indexer else {
        // TODO: more okey error
        return Err(Error::InvalidExpresstionType(
            "Identifier".to_string(),
            node.indexer.to_owned(),
        ));
    };
    let Some(indexer_arr) = ctx.get_mut_local_only(indexer_id) else {
        return Err(Error::NotFoundVariable(indexer_id.clone()));
    };
    let Value::Array(ref mut arr) = indexer_arr else {
        return Err(Error::ValueUnIndexabble(indexer_arr.clone()));
    };

    if arr.len() < idx {
        return Err(Error::ArrayOutOfBound(
            indexer_id.to_string(),
            arr.len(),
            idx,
        ));
    }

    arr[idx] = value;

    Ok(Return::none())
}

fn interpret_while_stmt(
    ctx: &mut Context,
    WhileNode { cond, body }: &WhileNode,
) -> Result<Return, Error> {
    let mut result = Ok(Return::none());

    loop {
        let bin = is_truthy(ctx, cond)?;
        if !bin {
            break;
        }
        result = interpret_stmt_list(ctx, body);
    }

    result
}

fn interpret_expr(ctx: &Context, expr: &Expression) -> Result<Value, Error> {
    match expr {
        Expression::FnCall(node) => interpret_function_call(ctx, node),
        Expression::FnDecl(node) => Ok(Value::Function(
            node.arg_names.to_owned(),
            node.body.to_owned(),
        )),
        Expression::Index(node) => interpret_index(ctx, node),
        Expression::When(nodes) => interpret_when(ctx, nodes),
        Expression::Ternary(node) => interpret_ternary_expr(ctx, node),
        Expression::Identifier(name) => Ok(get_value_owned(ctx, name.as_str())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Str(v) => Ok(Value::Str(v.to_string())),
        Expression::Bool(v) => Ok(Value::Bool(*v)),
        Expression::Number(v) => Ok(Value::Number(*v)),
        Expression::Array(exprs) => make_array_value(ctx, exprs),
        Expression::UnaryOp(expr, op) => interpret_unary_op(ctx, expr, *op),
        Expression::BinaryOp(node) => interpret_binary_op(ctx, node),
    }
}

fn interpret_ternary_expr(ctx: &Context, node: &TernaryExprNode) -> Result<Value, Error> {
    let cond_value = interpret_expr(ctx, &node.cond)?;
    let Value::Bool(cond_bin) = cond_value else {
        return Err(Error::CondNotBool(cond_value));
    };

    if cond_bin {
        interpret_expr(ctx, &node.true_expr)
    } else {
        interpret_expr(ctx, &node.false_expr)
    }
}

fn make_array_value(ctx: &Context, exprs: &Vec<Expression>) -> Result<Value, Error> {
    let mut result = vec![];
    for expr in exprs {
        result.push(interpret_expr(ctx, expr)?);
    }
    Ok(Value::Array(result))
}

fn interpret_index(ctx: &Context, node: &IndexExprNode) -> Result<Value, Error> {
    let indexer_val = interpret_expr(ctx, &node.indexer)?;
    let indexee_val = interpret_expr(ctx, &node.indexee)?;
    index(&indexer_val, &indexee_val)
}

fn interpret_function_call(
    ctx: &Context,
    FnCallNode { name, args }: &FnCallNode,
) -> Result<Value, Error> {
    let Some(fn_value) = ctx.get_include_parent(name) else {
        return Err(Error::NotFoundVariable(name.clone()));
    };
    let Value::Function(arg_names, body) = fn_value else {
        return Err(Error::ValueNotCallable(fn_value.to_owned()));
    };

    if arg_names.len() != args.len() {
        return Err(Error::WrongNumberOfArgument(
            name.clone(),
            arg_names.len(),
            args.len(),
        ));
    }

    let mut local_ctx = Context::with_parent(ctx);
    for (arg_name, arg_expr) in arg_names.iter().zip(args.iter()) {
        let value = interpret_expr(&local_ctx, arg_expr)?;
        local_ctx.insert(arg_name.to_string(), value);
    }

    interpret_stmt_list(&mut local_ctx, body).map(|r| r.value.unwrap_or(Value::Nil))
}

fn interpret_unary_op(ctx: &Context, expr: &Expression, op: Token) -> Result<Value, Error> {
    let res = interpret_expr(ctx, expr)?;
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
    ctx: &Context,
    BinaryOpNode { lhs, op, rhs }: &BinaryOpNode,
) -> Result<Value, Error> {
    let lhs = interpret_expr(ctx, lhs)?;
    let rhs = interpret_expr(ctx, rhs)?;
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

fn interpret_when(ctx: &Context, cases: &[CaseNode]) -> Result<Value, Error> {
    for CaseNode { cond, expr } in cases {
        let bin = is_truthy(ctx, cond)?;
        if bin {
            return interpret_expr(ctx, expr);
        }
    }
    Ok(Value::Nil)
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

fn is_truthy(ctx: &Context, expr: &Expression) -> Result<bool, Error> {
    let cond_value = interpret_expr(ctx, expr)?;
    let Value::Bool(cond_bin) = cond_value else {
        return Err(Error::CondNotBool(cond_value));
    };
    Ok(cond_bin)
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

fn prepare_indexee(indexee: &Value) -> Result<usize, Error> {
    let Value::Number(idx) = indexee else {
        return Err(Error::ValueCanBeUsedAsKey(indexee.clone()));
    };
    let idx = *idx;

    // check if idx is a interger
    if idx < 0.0 || idx.fract() != 0.0 {
        return Err(Error::ValueCanBeUsedAsKey(indexee.clone()));
    }
    Ok(idx as usize)
}

fn index(indexer: &Value, indexee: &Value) -> Result<Value, Error> {
    let Value::Array(arr) = indexer else {
        return Err(Error::ValueUnIndexabble(indexer.clone()));
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
