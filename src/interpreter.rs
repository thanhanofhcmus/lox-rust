use crate::ast::{
    ArrayRepeatNode, BinaryOpNode, CaseNode, Expression, FnCallNode, IdentifierNode, IfStmtNode,
    IndexExprNode, ModuleNode, ReAssignIndexNode, Statement, StatementList, TernaryExprNode,
    WhileNode,
};
use crate::id::{Category, Id};
use crate::token::Token;
use derive_more::Display;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
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
    ValueUnIndexable(Value),

    #[error("Value `{0}` is can not be used as key for array or map")]
    #[allow(dead_code)]
    ValueCannotBeUsedAsKey(Value),

    #[error("Value `{0}` is not of the type non-negative integer")]
    ValueMustBeUsize(Value),

    #[error("Expect expression of type `{0}`, have type `{1:?}`")]
    InvalidExpressionType(String, Expression),

    #[error("Array of name `{0}` has length `{1}` but receive index `{2}`")]
    ArrayOutOfBound(String, usize, usize),
}

#[derive(Display, Debug, Clone)]
pub enum Value {
    #[display(fmt = "nil")]
    Nil,

    // use ":?" to print the string in quotes
    #[display(fmt = "{:?}", _0)]
    Str(String),

    Number(f64),

    Bool(bool),

    #[display(fmt = "{:?}", _0)]
    Array(Vec<Value>),

    #[display(fmt = "function")]
    Function(Vec<String>, StatementList),
}

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

const CURRENT_MODULE_NAME: &str = "__current__";
const STACK_LIMIT: usize = 20;

struct StackEnvironment {
    variables: HashMap<Id, Value>,
    index: usize,
}

impl StackEnvironment {
    fn new(index: usize) -> Self {
        Self {
            variables: HashMap::new(),
            index,
        }
    }
}

struct ModuleEnvironment {
    variables: HashMap<Id, Value>,
    id: Id,
}

impl ModuleEnvironment {
    fn new(id: Id) -> Self {
        Self {
            variables: HashMap::new(),
            id,
        }
    }
}

struct Interpreter {
    stacks: Vec<StackEnvironment>,
    modules: HashMap<Id, ModuleEnvironment>,

    current_stack_index: usize,
    current_module_id: Id,

    print_writer: Rc<RefCell<dyn std::io::Write>>,
}

impl Interpreter {
    pub fn new(print_writer: Rc<RefCell<dyn std::io::Write>>) -> Self {
        let current_stack_index = 0;
        let current_module_id = Id::new(Category::Module, CURRENT_MODULE_NAME);

        let stacks = vec![StackEnvironment::new(current_stack_index)];
        let mut modules = HashMap::new();
        modules.insert(current_module_id, ModuleEnvironment::new(current_module_id));

        Self {
            stacks,
            modules,
            current_stack_index,
            current_module_id,

            print_writer,
        }
    }

    pub fn interpret(&mut self, stmt: &Statement) -> Result<Value, Error> {
        self.interpret_stmt(stmt)
            .map(|r| r.value.unwrap_or(Value::Nil))
    }

    // other functions

    fn push_stack(&mut self) {
        let new_stack_index = self.current_stack_index + 1;
        // TODO: check for max stack
        self.stacks.push(StackEnvironment::new(new_stack_index));
    }

    fn pop_stack(&mut self) {
        // TODO: check for min stack
        self.current_stack_index -= 1;
        self.stacks.pop();
    }

    fn get_identifier_from_current_stack(&self, id: Id) -> Option<&Value> {
        let current_stack = self.stacks.last().expect("stacks must have at least one block");
        current_stack.variables.get(&id)
    }


    // other functions

    fn interpret_stmt(&mut self, stmt: &Statement) -> Result<StmtReturn, Error> {
        match stmt {
            Statement::Module(node) => self.interpret_module_stmt(node),
            Statement::Print(exprs) => self.interpret_print_stmt(exprs),
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
                self.push_stack();
                let res = self.interpret_stmt_list(stmts);
                self.pop_stack();
                res
            }
            Statement::Global(stmts) => self.interpret_stmt_list(stmts),
        }
    }

    fn interpret_module_stmt(&mut self, node: &ModuleNode) -> Result<StmtReturn, Error> {
        let module_name = node.name.clone();
        self.push_module(module_name);
        let res = self.interpret_stmt_list(&node.body);
        self.pop_module();
        // TODO: Store module
        res
    }

    fn interpret_print_stmt(&mut self, exprs: &[Expression]) -> Result<StmtReturn, Error> {
        for expr in exprs {
            let value = self.interpret_expr(expr)?;
            let mut out = self.print_writer.borrow_mut();
            // TODO: handle error here
            write!(out, "{}", value).unwrap();
        }
        println!();
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
        if self.get_identifier_from_current_stack(iden).is_some() {
            return Err(Error::ReDeclareVariable(iden.join_dot()));
        }
        let value = self.interpret_expr(expr)?;
        self.insert_ident
        ctx.insert_identifier_to_current_stack(iden.clone(), value);
        Ok(StmtReturn::none())
    }

    fn interpret_reassign_id_stmt(
        &mut self,
        iden: &IdentifierNode,
        expr: &Expression,
    ) -> Result<StmtReturn, Error> {
        if ctx.get_local_only(iden).is_none() {
            return if ctx.get_include_parent(iden).is_some() {
                Err(Error::VariableReadOnly(iden.join_dot()))
            } else {
                Err(Error::NotFoundVariable(iden.join_dot()))
            };
        }
        let value = self.interpret_expr(expr)?;
        ctx.insert(iden.clone(), value);
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
        let Some(indexer_arr) = ctx.get_mut_local_only(indexer_id) else {
            return Err(Error::NotFoundVariable(indexer_id.join_dot()));
        };
        let Value::Array(ref mut arr) = indexer_arr else {
            return Err(Error::ValueUnIndexable(indexer_arr.clone()));
        };

        if arr.len() < idx {
            return Err(Error::ArrayOutOfBound(
                indexer_id.join_dot(),
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
            Expression::FnDecl(node) => Ok(Value::Function(
                node.arg_names.to_owned(),
                node.body.to_owned(),
            )),
            Expression::Index(node) => self.interpret_index_expr(node),
            Expression::When(nodes) => self.interpret_when_expr(nodes),
            Expression::Ternary(node) => self.interpret_ternary_expr(node),
            Expression::Identifier(node) => Ok(self.get_value_owned(node)),
            Expression::Nil => Ok(Value::Nil),
            Expression::Str(v) => Ok(Value::Str(v.to_string())),
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

    fn interpret_fn_call_expr(
        &mut self,
        FnCallNode { iden, args }: &FnCallNode,
    ) -> Result<Value, Error> {
        let Some(fn_value) = ctx.get_include_parent(iden) else {
            return Err(Error::NotFoundVariable(iden.join_dot()));
        };
        let Value::Function(arg_names, body) = fn_value else {
            return Err(Error::ValueNotCallable(fn_value.to_owned()));
        };

        if arg_names.len() != args.len() {
            return Err(Error::WrongNumberOfArgument(
                iden.join_dot(),
                arg_names.len(),
                args.len(),
            ));
        }

        let mut local_ctx = Context::with_parent(ctx);
        for (arg_name, arg_expr) in arg_names.iter().zip(args.iter()) {
            let value = interpret_expr(&local_ctx, arg_expr)?;
            local_ctx.insert(IdentifierNode::Simple(arg_name.clone()), value);
        }

        interpret_stmt_list(&mut local_ctx, body).map(|r| r.value.unwrap_or(Value::Nil))
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
            return Err(Error::CondNotBool(cond_value));
        };
        Ok(cond_bin)
    }
} // Interpreter

// a variable should be in a global scope
pub struct Context<'cur, 'pa: 'cur> {
    variables: HashMap<String, Value>,
    parent: Option<&'cur Context<'pa, 'pa>>,
    current_module: IdentifierNode,
    current_stack: usize,

    print_writer: Rc<RefCell<dyn std::io::Write>>,
}

impl<'cur, 'pa> Context<'cur, 'pa> {
    pub fn new(print_writer: Rc<RefCell<dyn std::io::Write>>) -> Self {
        Self {
            variables: HashMap::new(),
            current_module: IdentifierNode::Simple(CURRENT_MODULE_NAME.to_string()),
            parent: None,
            current_stack: 0,
            print_writer,
        }
    }

    pub fn with_parent(parent: &'pa Context<'cur, 'pa>) -> Self {
        if parent.current_stack + 1 > STACK_LIMIT {
            panic!("lox stack limit exceeded");
        }
        Self {
            variables: HashMap::new(),
            current_module: IdentifierNode::Simple(CURRENT_MODULE_NAME.to_string()),
            parent: Some(parent),
            current_stack: parent.current_stack + 1,
            print_writer: Rc::clone(&parent.print_writer),
        }
    }

    fn with_module_name(&self, key: &IdentifierNode) -> String {
        self.current_module.join_dot() + "." + key.join_dot().as_str()
    }

    pub fn get_local_only(&self, key: &IdentifierNode) -> Option<&Value> {
        if let Some(v) = self.variables.get(self.with_module_name(key).as_str()) {
            Some(v)
        } else {
            self.get_local_only_no_current(key)
        }
    }

    pub fn get_mut_local_only(&mut self, key: &IdentifierNode) -> Option<&mut Value> {
        self.variables.get_mut(&key.join_dot())
    }

    fn get_local_only_no_current(&self, key: &IdentifierNode) -> Option<&Value> {
        self.variables.get(key.join_dot().as_str())
    }

    pub fn get_include_parent(&self, key: &IdentifierNode) -> Option<&Value> {
        if let Some(value) = self.get_local_only(key) {
            return Some(value);
        }
        self.parent.and_then(|p| p.get_include_parent(key))
    }

    pub fn insert(&mut self, key: IdentifierNode, value: Value) {
        let _ = self.variables.insert(self.with_module_name(&key), value);
    }

    pub fn swap_current_module(&mut self, new_module: IdentifierNode) -> IdentifierNode {
        std::mem::replace(&mut self.current_module, new_module)
    }
}

fn get_value_owned(ctx: &Context, iden: &IdentifierNode) -> Value {
    ctx.get_include_parent(iden)
        .map(|v| v.to_owned())
        .unwrap_or(Value::Nil)
}

pub fn interpret(ctx: &mut Context, stmt: &Statement) -> Result<Value, Error> {
    interpret_stmt(ctx, stmt).map(|r| r.value.unwrap_or(Value::Nil))
}

fn interpret_stmt(ctx: &mut Context, stmt: &Statement) -> Result<StmtReturn, Error> {
    match stmt {
        Statement::Module(node) => interpret_module_stmt(ctx, node),
        Statement::Print(exprs) => interpret_print_stmt(ctx, exprs),
        Statement::Declare(name, expr) => interpret_declare_stmt(ctx, name, expr),
        Statement::ReassignIden(name, expr) => interpret_reassign_id_stmt(ctx, name, expr),
        Statement::ReassignIndex(node) => interpret_reassign_index_stmt(ctx, node),
        Statement::Return(expr) => interpret_expr(ctx, expr)
            .map(StmtReturn::new)
            .map(StmtReturn::should_bubble_up),
        Statement::Expr(expr) => interpret_expr(ctx, expr).map(StmtReturn::new),
        Statement::While(node) => interpret_while_stmt(ctx, node),
        Statement::If(node) => interpret_if_stmt(ctx, node),
        Statement::Block(stmts) => interpret_stmt_list(&mut Context::with_parent(ctx), stmts),
        Statement::Global(stmts) => interpret_stmt_list(ctx, stmts),
    }
}

fn interpret_module_stmt(ctx: &mut Context, node: &ModuleNode) -> Result<StmtReturn, Error> {
    let current_module = ctx.swap_current_module(node.name.clone());
    let res = interpret_stmt_list(ctx, &node.body);
    let _ = ctx.swap_current_module(current_module);
    res
}

fn interpret_print_stmt(ctx: &mut Context, exprs: &[Expression]) -> Result<StmtReturn, Error> {
    for expr in exprs {
        let value = interpret_expr(ctx, expr)?;
        let mut out = ctx.print_writer.borrow_mut();
        // TODO: handle error here
        write!(out, "{}", value).unwrap();
    }
    println!();
    Ok(StmtReturn::none())
}

fn interpret_if_stmt(ctx: &mut Context, node: &IfStmtNode) -> Result<StmtReturn, Error> {
    let cond = is_truthy(ctx, &node.cond)?;
    if !cond {
        return node
            .else_stmts
            .as_ref()
            .map(|stmts| interpret_stmt_list(ctx, stmts))
            .unwrap_or(Ok(StmtReturn::none()));
    }
    interpret_stmt_list(ctx, &node.if_stmts)
}

fn interpret_stmt_list(ctx: &mut Context, stmts: &[Statement]) -> Result<StmtReturn, Error> {
    for stmt in stmts {
        let ret = interpret_stmt(ctx, stmt)?;
        if ret.should_bubble_up {
            return Ok(ret);
        }
    }
    Ok(StmtReturn::none())
}

fn interpret_declare_stmt(
    ctx: &mut Context,
    iden: &IdentifierNode,
    expr: &Expression,
) -> Result<StmtReturn, Error> {
    if ctx.get_local_only(iden).is_some() {
        return Err(Error::ReDeclareVariable(iden.join_dot()));
    }
    let value = interpret_expr(ctx, expr)?;
    ctx.insert(iden.clone(), value);
    Ok(StmtReturn::none())
}

fn interpret_reassign_id_stmt(
    ctx: &mut Context,
    iden: &IdentifierNode,
    expr: &Expression,
) -> Result<StmtReturn, Error> {
    if ctx.get_local_only(iden).is_none() {
        return if ctx.get_include_parent(iden).is_some() {
            Err(Error::VariableReadOnly(iden.join_dot()))
        } else {
            Err(Error::NotFoundVariable(iden.join_dot()))
        };
    }
    let value = interpret_expr(ctx, expr)?;
    ctx.insert(iden.clone(), value);
    Ok(StmtReturn::none())
}

fn interpret_reassign_index_stmt(
    ctx: &mut Context,
    node: &ReAssignIndexNode,
) -> Result<StmtReturn, Error> {
    let indexee_val = interpret_expr(ctx, &node.indexee)?;
    let idx = prepare_indexee(&indexee_val)?;

    let value = interpret_expr(ctx, &node.expr)?;

    let Expression::Identifier(ref indexer_id) = node.indexer else {
        // TODO: more concrete error
        return Err(Error::InvalidExpressionType(
            "Identifier".to_string(),
            node.indexer.to_owned(),
        ));
    };
    let Some(indexer_arr) = ctx.get_mut_local_only(indexer_id) else {
        return Err(Error::NotFoundVariable(indexer_id.join_dot()));
    };
    let Value::Array(ref mut arr) = indexer_arr else {
        return Err(Error::ValueUnIndexable(indexer_arr.clone()));
    };

    if arr.len() < idx {
        return Err(Error::ArrayOutOfBound(
            indexer_id.join_dot(),
            arr.len(),
            idx,
        ));
    }

    arr[idx] = value;

    Ok(StmtReturn::none())
}

fn interpret_while_stmt(
    ctx: &mut Context,
    WhileNode { cond, body }: &WhileNode,
) -> Result<StmtReturn, Error> {
    let mut result = StmtReturn::none();

    loop {
        let bin = is_truthy(ctx, cond)?;
        if !bin {
            break;
        }
        result = interpret_stmt_list(ctx, body)?;
    }

    Ok(result)
}

fn interpret_expr(ctx: &Context, expr: &Expression) -> Result<Value, Error> {
    match expr {
        Expression::FnCall(node) => interpret_fn_call_expr(ctx, node),
        Expression::FnDecl(node) => Ok(Value::Function(
            node.arg_names.to_owned(),
            node.body.to_owned(),
        )),
        Expression::Index(node) => interpret_index_expr(ctx, node),
        Expression::When(nodes) => interpret_when_expr(ctx, nodes),
        Expression::Ternary(node) => interpret_ternary_expr(ctx, node),
        Expression::Identifier(node) => Ok(get_value_owned(ctx, node)),
        Expression::Nil => Ok(Value::Nil),
        Expression::Str(v) => Ok(Value::Str(v.to_string())),
        Expression::Bool(v) => Ok(Value::Bool(*v)),
        Expression::Number(v) => Ok(Value::Number(*v)),
        Expression::ArrayList(exprs) => make_array_value(ctx, exprs),
        Expression::ArrayRepeat(node) => make_array_repeat(ctx, node),
        Expression::UnaryOp(expr, op) => interpret_unary_op(ctx, expr, *op),
        Expression::BinaryOp(node) => interpret_binary_op(ctx, node),
    }
}

fn interpret_ternary_expr(ctx: &Context, node: &TernaryExprNode) -> Result<Value, Error> {
    let cond = is_truthy(ctx, &node.cond)?;
    if cond {
        interpret_expr(ctx, &node.true_expr)
    } else {
        interpret_expr(ctx, &node.false_expr)
    }
}

fn make_array_value(ctx: &Context, exprs: &Vec<Expression>) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(exprs.len());
    for expr in exprs {
        result.push(interpret_expr(ctx, expr)?);
    }
    Ok(Value::Array(result))
}

fn make_array_repeat(ctx: &Context, node: &ArrayRepeatNode) -> Result<Value, Error> {
    let value = interpret_expr(ctx, &node.value)?;
    let repeat_val = interpret_expr(ctx, &node.repeat)?;
    let repeat = is_usize(&repeat_val)?;

    let mut result = Vec::with_capacity(repeat);
    for _ in 0..repeat {
        result.push(value.clone());
    }
    Ok(Value::Array(result))
}

fn interpret_index_expr(ctx: &Context, node: &IndexExprNode) -> Result<Value, Error> {
    let indexer_val = interpret_expr(ctx, &node.indexer)?;
    let indexee_val = interpret_expr(ctx, &node.indexee)?;
    index(&indexer_val, &indexee_val)
}

fn interpret_fn_call_expr(
    ctx: &Context,
    FnCallNode { iden, args }: &FnCallNode,
) -> Result<Value, Error> {
    let Some(fn_value) = ctx.get_include_parent(iden) else {
        return Err(Error::NotFoundVariable(iden.join_dot()));
    };
    let Value::Function(arg_names, body) = fn_value else {
        return Err(Error::ValueNotCallable(fn_value.to_owned()));
    };

    if arg_names.len() != args.len() {
        return Err(Error::WrongNumberOfArgument(
            iden.join_dot(),
            arg_names.len(),
            args.len(),
        ));
    }

    let mut local_ctx = Context::with_parent(ctx);
    for (arg_name, arg_expr) in arg_names.iter().zip(args.iter()) {
        let value = interpret_expr(&local_ctx, arg_expr)?;
        local_ctx.insert(IdentifierNode::Simple(arg_name.clone()), value);
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

fn interpret_when_expr(ctx: &Context, cases: &[CaseNode]) -> Result<Value, Error> {
    for CaseNode { cond, expr } in cases {
        let bin = is_truthy(ctx, cond)?;
        if bin {
            return interpret_expr(ctx, expr);
        }
    }
    Ok(Value::Nil)
}

fn is_truthy(ctx: &Context, expr: &Expression) -> Result<bool, Error> {
    let cond_value = interpret_expr(ctx, expr)?;
    let Value::Bool(cond_bin) = cond_value else {
        return Err(Error::CondNotBool(cond_value));
    };
    Ok(cond_bin)
}

fn is_equal(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    Ok(Value::Bool(match lhs {
        // TODO: compare function pointer
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
