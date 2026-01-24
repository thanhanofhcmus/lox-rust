use super::environment::Environment;
use super::error::Error;
use super::value::Value;
use crate::ast::*;
use crate::interpret::gc::{GcHandle, GcObject};
use crate::interpret::value::{BuiltinFn, Function};
use crate::parse;
use crate::token::Token;
use std::collections::BTreeMap;
use std::panic;
use std::path::Path;

#[derive(Debug)]
struct ValueReturn {
    value: Option<Value>,
    should_bubble_up: bool,
}

impl ValueReturn {
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

    fn get_or_unit(self) -> Value {
        self.value.unwrap_or(Value::Unit)
    }

    fn get_or_error(self) -> Result<Value, Error> {
        match self.value {
            Some(v) => Ok(v),
            None => Err(Error::UseUnitValue),
        }
    }

    fn should_bubble_up(mut self) -> Self {
        self.should_bubble_up = true;
        self
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

        for stmt in &ast.global_stmts {
            let ret = self.interpret_stmt(stmt)?;
            // At the global level, we catch the bubble and return the value
            if ret.should_bubble_up {
                return Ok(ret.get_or_unit());
            }
        }

        Ok(Value::Unit)
    }

    fn interpret_stmt(&mut self, stmt: &Statement) -> Result<ValueReturn, Error> {
        match stmt {
            Statement::Declare(name, expr) => self.interpret_declare_stmt(name, expr),
            Statement::ReassignIden(name, expr) => self.interpret_reassign_id_stmt(name, expr),
            Statement::Expr(expr) => self.interpret_expr(expr),
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

        let content = std::fs::read_to_string(file_path)
            .map_err(|err| Error::ReadModuleFailed(name.clone(), path.clone(), err))?;

        let tokens = parse::lex(&content)
            .map_err(|err| Error::ParseModuleFailed(name.clone(), path.clone(), err))?;

        let statement = parse::parse(&content, &tokens)
            .map_err(|err| Error::ParseModuleFailed(name.clone(), path.clone(), err))?;

        // interpret the file
        // we will kind of need to keep track of the current module stack
        // and the current scope stack
        // everything starts from a module
        self.environment.init_module(module_id);
        let mut itp = Interpreter::new(self.environment, &content);

        // We treat the module evaluation as a top-level interpret call
        itp.interpret(&statement)
            .map_err(|err| Error::InterpretModuleFailed(name, path, Box::new(err)))?;

        self.environment.deinit_module();

        // TODO: detect circular dependency

        Ok(())
    }

    fn interpret_declare_stmt(
        &mut self,
        iden: &IdentifierNode,
        expr: &Expression,
    ) -> Result<ValueReturn, Error> {
        if self.environment.get_variable_current_scope(iden).is_some() {
            return Err(Error::ReDeclareVariable(iden.create_name(self.input)));
        }
        let res = self.interpret_expr(expr)?;
        if res.should_bubble_up {
            return Ok(res);
        }

        let val = res.get_or_error()?;
        self.environment.insert_variable_current_scope(iden, val);
        Ok(ValueReturn::none())
    }

    fn interpret_reassign_id_stmt(
        &mut self,
        iden: &IdentifierNode,
        expr: &Expression,
    ) -> Result<ValueReturn, Error> {
        if self.environment.get_variable_current_scope(iden).is_none() {
            return match self.environment.get_variable_all_scope(iden) {
                Some(_) => Err(Error::VariableReadOnly(iden.create_name(self.input))),
                None => Err(Error::NotFoundVariable(iden.create_name(self.input))),
            };
        }
        let res = self.interpret_expr(expr)?;
        if res.should_bubble_up {
            return Ok(res);
        }

        let val = res.get_or_error()?;
        self.environment.insert_variable_current_scope(iden, val);
        Ok(ValueReturn::none())
    }

    fn interpret_expr(&mut self, expr: &Expression) -> Result<ValueReturn, Error> {
        match expr {
            Expression::Return(expr) => self.interpret_return_expr(expr),
            Expression::When(nodes) => self.interpret_when_expr(nodes),
            Expression::Clause(node) => self.interpret_clause_expr(node),
            Expression::Block(node) => self.interpret_block_node(node),
            Expression::While(node) => self.interpret_while_expr(node),
            Expression::IfChain(node) => self.interpret_if_chain_expr(node),
        }
    }

    fn interpret_return_expr(
        &mut self,
        return_expr: &Option<Box<Expression>>,
    ) -> Result<ValueReturn, Error> {
        let value = match return_expr {
            Some(expr) => self.interpret_expr(expr)?.get_or_unit(),
            None => Value::Unit,
        };
        Ok(ValueReturn::new(value).should_bubble_up())
    }

    fn interpret_block_node(&mut self, node: &BlockNode) -> Result<ValueReturn, Error> {
        for stmt in &node.stmts {
            let res = self.interpret_stmt(stmt)?;
            if res.should_bubble_up {
                return Ok(res);
            }
        }
        if let Some(expr) = &node.last_expr {
            return self.interpret_expr(expr);
        }

        Ok(ValueReturn::none())
    }

    fn interpret_while_expr(
        &mut self,
        WhileNode { cond, body }: &WhileNode,
    ) -> Result<ValueReturn, Error> {
        loop {
            if !self.is_truthy(cond)? {
                break;
            }
            let res = self.interpret_block_node(body)?;
            // If the block contains a 'return', stop the loop and bubble it up
            // TODO: while shoud disallow yeilding value
            if res.should_bubble_up {
                return Ok(res);
            }
        }

        Ok(ValueReturn::none())
    }

    fn interpret_if_chain_expr(&mut self, node: &IfChainNode) -> Result<ValueReturn, Error> {
        if self.is_truthy(&node.if_node.cond)? {
            return self.interpret_block_node(&node.if_node.stmts);
        }
        for else_if_node in &node.else_if_nodes {
            if self.is_truthy(&else_if_node.cond)? {
                return self.interpret_block_node(&else_if_node.stmts);
            }
        }
        if let Some(else_stmts) = &node.else_stmts {
            return self.interpret_block_node(else_stmts);
        }
        Ok(ValueReturn::none())
    }

    fn interpret_clause_expr(&mut self, node: &ClauseNode) -> Result<ValueReturn, Error> {
        match node {
            ClauseNode::UnaryOp(node, op) => {
                self.interpret_unary_op(node, *op).map(ValueReturn::new)
            }
            ClauseNode::BinaryOp(node) => self.interpret_binary_op(node).map(ValueReturn::new),
            ClauseNode::Chaining(node) => self.interpret_chaining_expr(node).map(ValueReturn::new),
        }
    }

    fn interpret_primary_expr(&mut self, node: &PrimaryNode) -> Result<ValueReturn, Error> {
        let val = match node {
            PrimaryNode::Nil => Value::Nil,
            PrimaryNode::Str(v) => Value::Str(v.string_from_source(self.input)),
            PrimaryNode::Bool(v) => Value::Bool(*v),
            PrimaryNode::Integer(v) => Value::Integer(*v),
            PrimaryNode::Floating(v) => Value::Floating(*v),
            PrimaryNode::ArrayLiteral(node) => {
                let res = self.interpret_array_literal(node)?;
                return Ok(ValueReturn::new(res));
            }
            PrimaryNode::MapLiteral(node) => {
                let res = self.interpret_map_literal(node)?;
                return Ok(ValueReturn::new(res));
            }
            PrimaryNode::FnDecl(node) => {
                let res = self.interpret_fn_decl(node)?;
                return Ok(ValueReturn::new(res));
            }
        };
        Ok(ValueReturn::new(val))
    }

    fn interpret_array_literal(&mut self, node: &ArrayLiteralNode) -> Result<Value, Error> {
        match node {
            ArrayLiteralNode::List(clauses) => {
                let mut result = Vec::with_capacity(clauses.len());
                for expr in clauses {
                    let res = self.interpret_clause_expr(expr)?;
                    // Even in literals, we must check if an expression bubbled a return
                    // though usually return isn't allowed here syntactically.
                    result.push(res.get_or_error()?);
                }
                Ok(Value::Array(result))
            }
            ArrayLiteralNode::Repeat(node) => {
                let val_res = self.interpret_clause_expr(&node.value)?;
                let rep_res = self.interpret_clause_expr(&node.repeat)?;

                let value = val_res.get_or_error()?;
                let repeat = to_index(&rep_res.get_or_error()?)?;

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
            let key = self.interpret_primary_expr(&kv.key)?.get_or_error()?;
            let value = self.interpret_expr(&kv.value)?.get_or_error()?;
            m.insert(key, value);
        }
        Ok(Value::Map(m))
    }

    fn interpret_fn_decl(&mut self, node: &FnDeclNode) -> Result<Value, Error> {
        let object = GcObject::Function(Function {
            arg_ids: node.arg_names.iter().map(|a| a.get_id()).collect(),
            body: node.body.to_owned(),
        });

        let handle = self.environment.insert_gc_object(object);

        Ok(Value::Function(handle))
    }

    fn interpret_normal_fn_call_expr(
        &mut self,
        node: &FnCallNode,
        handle: GcHandle,
        // value::Function { arg_ids, body }: &Function,
    ) -> Result<Value, Error> {
        let Some(value) = self.environment.get_gc_object(handle) else {
            // TODO:
            return Err(Error::NotFoundVariable("TODO".to_string()));
        };

        let GcObject::Function(func) = value else {
            // TODO:
            return Err(Error::ValueNotCallable(Value::Unit));
        };

        // TODO:
        let Function { arg_ids, body } = func.clone();

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
            let res = self.interpret_expr(arg_expr)?.get_or_error()?;
            self.environment
                .insert_variable_current_scope_by_id(*arg_id, res);
        }

        let result = self.interpret_block_node(&body);
        self.environment.pop_scope();

        // Convert the ValueReturn back to a raw Value.
        // If it was bubbling, it stops bubbling here because the function has returned.
        result.map(|vr| vr.get_or_unit())
    }

    fn intepret_builtin_fn_call_expr(
        &mut self,
        node: &FnCallNode,
        function: BuiltinFn,
    ) -> Result<Value, Error> {
        let mut args = vec![];
        for expr in &node.args {
            let res = self.interpret_expr(expr)?.get_or_error()?;
            args.push(res);
        }
        function(self, args)
    }

    fn interpret_unary_op(&mut self, node: &ClauseNode, op: Token) -> Result<Value, Error> {
        let res = self.interpret_clause_expr(node)?.get_or_error()?;
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
        let lhs_val = self.interpret_clause_expr(lhs)?.get_or_error()?;
        let rhs_val = self.interpret_clause_expr(rhs)?.get_or_error()?;
        let op = *op;

        match op {
            Token::Plus => add(&lhs_val, &rhs_val),
            Token::Minus => subtract(&lhs_val, &rhs_val),
            Token::Star => times(&lhs_val, &rhs_val),
            Token::Slash => divide(&lhs_val, &rhs_val),
            Token::Percentage => modulo(&lhs_val, &rhs_val),
            Token::And | Token::Or => and_or(&lhs_val, op, &rhs_val),
            Token::EqualEqual | Token::BangEqual => Ok(Value::Bool(lhs_val == rhs_val)),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                ordering(&lhs_val, op, &rhs_val)
            }
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_when_expr(&mut self, cases: &[WhenArmNode]) -> Result<ValueReturn, Error> {
        for WhenArmNode { cond, expr } in cases {
            if self.is_truthy(cond)? {
                return self.interpret_clause_expr(expr);
            }
        }
        Ok(ValueReturn::new(Value::Unit))
    }

    fn interpret_chaining_expr(&mut self, node: &ChainingNode) -> Result<Value, Error> {
        // TODO: we need to make this ref if we want bring back re-assignment ability
        let mut value = match &node.base {
            ChainingBase::Primary(v) => self.interpret_primary_expr(v)?.get_or_error()?,
            ChainingBase::Group(v) => self.interpret_expr(v)?.get_or_error()?,
            ChainingBase::Identifier(v) => self.lookup_all_scope(v),
        };
        for follow in &node.follows {
            value = match follow {
                ChainingFollow::Identifier(node) => self.lookup_all_scope(node),
                ChainingFollow::FnCall(node) => match value {
                    Value::Function(handle) => self.interpret_normal_fn_call_expr(node, handle)?,
                    Value::BuiltinFunction(function) => {
                        self.intepret_builtin_fn_call_expr(node, function)?
                    }
                    _ => return Err(Error::ValueNotCallable(value)),
                },
                ChainingFollow::Index(indexee_expr) => {
                    let indexee = self.interpret_expr(indexee_expr)?.get_or_error()?;
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
        let res = self.interpret_clause_expr(expr)?;
        let cond_value = res.get_or_error()?;
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
        _ => Err(Error::MismatchType(Token::Plus, lhs.clone(), rhs.clone())),
    }
}

fn subtract(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l - r)),
        (Floating(l), Integer(r)) => Ok(Floating(l - (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) - r)),
        (Floating(l), Floating(r)) => Ok(Floating(l - r)),
        _ => Err(Error::MismatchType(Token::Minus, lhs.clone(), rhs.clone())),
    }
}

fn times(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l * r)),
        (Floating(l), Integer(r)) => Ok(Floating(l * (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) * r)),
        (Floating(l), Floating(r)) => Ok(Floating(l * r)),
        _ => Err(Error::MismatchType(Token::Star, lhs.clone(), rhs.clone())),
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
        _ => Err(Error::MismatchType(Token::Slash, lhs.clone(), rhs.clone())),
    }
}

fn modulo(lhs: &Value, rhs: &Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l % r)),
        (Floating(l), Integer(r)) => Ok(Floating(l % (*r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((*l as f64) % r)),
        (Floating(l), Floating(r)) => Ok(Floating(l % r)),
        _ => Err(Error::MismatchType(
            Token::PercentLPointParent,
            lhs.clone(),
            rhs.clone(),
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
            match arr.get(idx) {
                None => Err(Error::ArrayOutOfBound(arr.len(), idx)),
                Some(v) => Ok(v.to_owned()),
            }
        }
        Value::Map(m) => Ok(m.get(indexee).map(Value::to_owned).unwrap_or(Value::Nil)),
        _ => Err(Error::ValueUnIndexable(indexer.clone())),
    }
}
