use super::environment::Environment;
use super::error::Error;
use super::value::Value;
use crate::ast::*;
use crate::interpret::value::{self, BuiltinFn, Function};
use crate::parse;
use crate::token::Token;
use std::collections::BTreeMap;
use std::panic;
use std::path::Path;

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

    pub fn interpret(&mut self, ast: &'sl AST) -> Result<Value, Error> {
        for import in &ast.imports {
            self.interpret_import_stmt(import)?;
        }

        // dbg!(&ast.global_stmts);

        self.interpret_stmt_list(&ast.global_stmts)
            .map(|r| r.value.unwrap_or(Value::Nil))
    }

    fn interpret_stmt(&mut self, stmt: &Statement) -> Result<StmtReturn, Error> {
        match stmt {
            Statement::Declare(name, expr) => self.interpret_declare_stmt(name, expr),
            Statement::ReassignIden(name, expr) => self.interpret_reassign_id_stmt(name, expr),
            Statement::Expr(expr) => self.interpret_expr(expr).map(StmtReturn::new),
            Statement::While(node) => self.interpret_while_stmt(node),
            Statement::If(node) => self.interpret_if_stmt(node),
            Statement::Return(expr) => self.interpret_return_stmt(expr),
            Statement::Block(stmts) => {
                // TODO: other scope like while and if and function should push scope as well
                self.environment.push_scope();
                let res = self.interpret_stmt_list(stmts);
                self.environment.pop_scope();
                res
            }
        }
    }

    fn interpret_import_stmt(&mut self, node: &ImportNode) -> Result<(), Error> {
        if !node.iden.is_simple() {
            // TODO: move this to the parser part
            panic!("import Identifier must be simple");
        }

        // check if we import this yet
        let module_id = node.iden.get_id();
        if self.environment.contains_module(module_id) {
            return Ok(());
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

        Ok(())
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

    fn interpret_return_stmt(
        &mut self,
        return_expr: &Option<Expression>,
    ) -> Result<StmtReturn, Error> {
        match return_expr {
            Some(expr) => self
                .interpret_expr(expr)
                .map(StmtReturn::new)
                .map(StmtReturn::should_bubble_up),
            None => Ok(StmtReturn::none().should_bubble_up()),
        }
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
            Expression::When(nodes) => self.interpret_when_expr(nodes),
            Expression::Clause(node) => self.interpret_clause_expr(node),
            Expression::Block(node) => self.interpret_block_node(node),
            Expression::IfChain(node) => self.interpret_if_chain_expr(node),
        }
    }

    fn interpret_block_node(&mut self, node: &BlockNode) -> Result<Value, Error> {
        // need return statement
        for stmt in &node.stmts {
            // TODO: handle return statement
            _ = self.interpret_stmt(stmt)?;
        }
        if let Some(expr) = &node.last_expr {
            return self.interpret_expr(expr);
        }

        // let stmt_return = self.interpret_stmt(last)?;
        // TODO: maybe use Value::never;
        Ok(Value::Unit)
    }

    fn interpret_if_chain_expr(&mut self, node: &IfChainNode) -> Result<Value, Error> {
        let cond = self.is_truthy(&node.if_node.cond)?;
        if cond {
            return self.interpret_block_node(&node.if_node.stmts);
        }
        for else_if_node in &node.else_if_nodes {
            let cond = self.is_truthy(&else_if_node.cond)?;
            if cond {
                return self.interpret_block_node(&else_if_node.stmts);
            }
        }
        let Some(else_stmts) = &node.else_stmts else {
            return Ok(Value::Unit);
        };
        self.interpret_block_node(else_stmts)
    }

    fn interpret_clause_expr(&mut self, node: &ClauseNode) -> Result<Value, Error> {
        match node {
            ClauseNode::Primary(node) => self.interpret_primary_expr(node),
            ClauseNode::UnaryOp(node, op) => self.interpret_unary_op(node, *op),
            ClauseNode::BinaryOp(node) => self.interpret_binary_op(node),
            ClauseNode::Chaining(node) => self.interpret_chaining_expr(node),
        }
    }

    fn interpret_primary_expr(&mut self, node: &PrimaryNode) -> Result<Value, Error> {
        match node {
            PrimaryNode::Nil => Ok(Value::Nil),
            PrimaryNode::Str(v) => Ok(Value::Str(v.string_from_source(self.input))),
            PrimaryNode::Bool(v) => Ok(Value::Bool(*v)),
            PrimaryNode::Integer(v) => Ok(Value::Integer(*v)),
            PrimaryNode::Floating(v) => Ok(Value::Floating(*v)),
            PrimaryNode::ArrayLiteral(node) => self.interpret_array_literal(node),
            PrimaryNode::MapLiteral(node) => self.interpret_map_literal(node),
            PrimaryNode::FnDecl(node) => Ok(Value::Function(Function {
                arg_ids: node.arg_names.iter().map(|a| a.get_id()).collect(),
                body: node.body.to_owned(),
            })),
        }
    }

    fn interpret_array_literal(&mut self, node: &ArrayLiteralNode) -> Result<Value, Error> {
        match node {
            ArrayLiteralNode::List(clauses) => {
                let mut result = Vec::with_capacity(clauses.len());
                for expr in clauses {
                    result.push(self.interpret_clause_expr(expr)?);
                }
                Ok(Value::Array(result))
            }
            ArrayLiteralNode::Repeat(node) => {
                let value = self.interpret_clause_expr(&node.value)?;
                let repeat_val = self.interpret_clause_expr(&node.repeat)?;
                let repeat = to_index(&repeat_val)?;

                let mut result = Vec::with_capacity(repeat);
                for _ in 0..repeat {
                    result.push(value.clone());
                }
                Ok(Value::Array(result))
            }
        }
    }

    fn interpret_map_literal(&mut self, node: &MapLiteralNode) -> Result<Value, Error> {
        let mut m = BTreeMap::new();
        for kv in &node.nodes {
            let key = self.interpret_primary_expr(&kv.key)?;
            let value = self.interpret_expr(&kv.value)?;
            m.insert(key, value);
        }
        Ok(Value::Map(m))
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

    fn interpret_unary_op(&mut self, node: &ClauseNode, op: Token) -> Result<Value, Error> {
        let res = self.interpret_clause_expr(node)?;
        match op {
            Token::Not => match res {
                Value::Bool(v) => Ok(Value::Bool(!v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            Token::Minus => match res {
                Value::Floating(v) => Ok(Value::Floating(-v)),
                _ => Err(Error::InvalidOperationOnType(op, res)),
            },
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_binary_op(
        &mut self,
        BinaryOpNode { lhs, op, rhs }: &BinaryOpNode,
    ) -> Result<Value, Error> {
        let lhs = self.interpret_clause_expr(lhs)?;
        let rhs = self.interpret_clause_expr(rhs)?;
        let op = *op;

        match op {
            Token::Plus => add(&lhs, &rhs),
            Token::Minus => subtract(&lhs, &rhs),
            Token::Star => times(&lhs, &rhs),
            Token::Slash => divide(&lhs, &rhs),
            Token::Percentage => modulo(&lhs, &rhs),
            Token::And | Token::Or => and_or(&lhs, op, &rhs),
            Token::EqualEqual | Token::BangEqual => Ok(Value::Bool(lhs == rhs)),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                ordering(&lhs, op, &rhs)
            }
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_when_expr(&mut self, cases: &[WhenArmNode]) -> Result<Value, Error> {
        for WhenArmNode { cond, expr } in cases {
            let bin = self.is_truthy(cond)?;
            if bin {
                return self.interpret_clause_expr(expr);
            }
        }
        Ok(Value::Nil)
    }

    fn interpret_chaining_expr(&mut self, node: &ChainingNode) -> Result<Value, Error> {
        // TODO: we need to make this ref if we want bring back re-assignment ability
        let mut value = match &node.base {
            ChainingBase::Primary(v) => self.interpret_primary_expr(v)?,
            ChainingBase::Group(v) => self.interpret_expr(v)?,
            ChainingBase::Identifier(v) => self.lookup_all_scope(v),
        };
        for follow in &node.follows {
            value = match follow {
                ChainingFollow::Identifier(node) => self.lookup_all_scope(node),
                ChainingFollow::FnCall(node) => {
                    match value {
                        Value::Function(function) => {
                            // TODO: remove this clone
                            let function = function.clone();
                            self.interpret_normal_fn_call_expr(node, &function)?
                        }
                        Value::BuiltinFunction(function) => {
                            self.intepret_builtin_fn_call_expr(node, function)?
                        }
                        _ => return Err(Error::ValueNotCallable(value)),
                    }
                }
                ChainingFollow::Index(indexee_expr) => {
                    let indexee = self.interpret_expr(indexee_expr)?;
                    index(&value, &indexee)?
                }
            }
        }
        Ok(value)
    }

    fn lookup_all_scope(&self, node: &IdentifierNode) -> Value {
        // TODO: make this a ref
        self.environment
            .get_variable_all_scope(node)
            .map(|v| v.to_owned())
            .to_owned()
            .unwrap_or(Value::Nil)
    }

    fn is_truthy(&mut self, expr: &ClauseNode) -> Result<bool, Error> {
        let cond_value = self.interpret_clause_expr(expr)?;
        let Value::Bool(cond_bin) = cond_value else {
            return Err(Error::ConditionNotBool(cond_value));
        };
        Ok(cond_bin)
    }
}

fn ordering(lhs: &Value, op: Token, rhs: &Value) -> Result<Value, Error> {
    let from_ord = |o: std::cmp::Ordering| match op {
        Token::Less => Ok(Value::Bool(o.is_lt())),
        Token::LessEqual => Ok(Value::Bool(o.is_le())),
        Token::Greater => Ok(Value::Bool(o.is_gt())),
        Token::GreaterEqual => Ok(Value::Bool(o.is_ge())),
        _ => Err(Error::UnknownOperation(op)),
    };
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => from_ord(l.cmp(r)),
        (Floating(l), Integer(r)) => from_ord(l.total_cmp(&(*r as f64))),
        (Integer(l), Floating(r)) => from_ord((*l as f64).total_cmp(r)),
        (Floating(l), Floating(r)) => from_ord(l.total_cmp(r)),
        (Str(l), Str(r)) => from_ord(l.cmp(r)),
        _ => Err(Error::InvalidOperationOnType(op, lhs.clone())),
    }
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
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l + r)),
        (Floating(l), Integer(r)) => Ok(Floating(l + (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) + r)),
        (Floating(l), Floating(r)) => Ok(Floating(l + r)),
        (Str(l), Str(r)) => Ok(Str(l.to_owned() + r)),
        _ => Err(Error::InvalidOperationOnType(Token::Plus, lhs.clone())),
    }
}

fn subtract(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l - r)),
        (Floating(l), Integer(r)) => Ok(Floating(l - (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) - r)),
        (Floating(l), Floating(r)) => Ok(Floating(l - r)),
        _ => Err(Error::InvalidOperationOnType(Token::Minus, lhs.clone())),
    }
}

fn times(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l * r)),
        (Floating(l), Integer(r)) => Ok(Floating(l * (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) * r)),
        (Floating(l), Floating(r)) => Ok(Floating(l * r)),
        _ => Err(Error::InvalidOperationOnType(Token::Star, lhs.clone())),
    }
}

fn divide(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match rhs {
        Integer(v) if *v == 0 => return Err(Error::DivideByZero),
        Floating(v) if *v == 0.0 => return Err(Error::DivideByZero),
        _ => {}
    }
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => {
            if l % r == 0 {
                Ok(Integer(l / r))
            } else {
                Ok(Floating(*l as f64 / *r as f64))
            }
        }
        (Floating(l), Integer(r)) => Ok(Floating(l / (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) / r)),
        (Floating(l), Floating(r)) => Ok(Floating(l / r)),
        _ => Err(Error::InvalidOperationOnType(Token::Star, lhs.clone())),
    }
}

fn modulo(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l % r)),
        (Floating(l), Integer(r)) => Ok(Floating(l % (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) % r)),
        (Floating(l), Floating(r)) => Ok(Floating(l % r)),
        _ => Err(Error::InvalidOperationOnType(
            Token::Percentage,
            lhs.clone(),
        )),
    }
}

fn to_index(value: &Value) -> Result<usize, Error> {
    match value {
        Value::Integer(i) => {
            if *i < 0 {
                return Err(Error::ValueMustBeUsize(value.clone()));
            }
            // Try into usize to handle platforms where usize < i64
            usize::try_from(*i).map_err(|_| Error::ValueMustBeUsize(value.clone()))
        }
        Value::Floating(f) => {
            if *f < 0.0 || f.fract() != 0.0 {
                return Err(Error::ValueMustBeUsize(value.clone()));
            }
            // We use 'as' for the final cast, but check bounds first
            if *f > usize::MAX as f64 {
                return Err(Error::ValueMustBeUsize(value.clone()));
            }
            Ok(*f as usize)
        }
        _ => Err(Error::ValueMustBeUsize(value.clone())),
    }
}

fn index(indexer: &Value, indexee: &Value) -> Result<Value, Error> {
    match indexer {
        Value::Array(arr) => {
            let idx = to_index(indexee)?;
            // This one is return a new value, maybe return a ref instead
            Ok(arr.get(idx).map(Value::to_owned).unwrap_or(Value::Nil))
        }
        Value::Map(m) => Ok(m.get(indexee).map(Value::to_owned).unwrap_or(Value::Nil)),
        _ => Err(Error::ValueUnIndexable(indexer.clone())),
    }
}
