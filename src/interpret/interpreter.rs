use super::environment::Environment;
use super::error::Error;
use super::values::Value;
use crate::ast::*;
use crate::interpret::heap::GcHandle;
use crate::interpret::values::{BuiltinFn, Function, Map, MapKey, Number, Scalar};
use crate::parse;
use crate::token::Token;
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
            // TODO: This is not valid, return should not be in the top level
            if ret.should_bubble_up {
                return Ok(ret.get_or_unit());
            }
        }

        Ok(Value::Unit)
    }

    fn interpret_stmt(&mut self, stmt: &Statement) -> Result<ValueReturn, Error> {
        match stmt {
            Statement::Declare(name, expr) => self.interpret_declare_stmt(name, expr),
            Statement::ReassignIden(node, expr) => self.interpret_reassign_id_stmt(node, expr),
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
        node: &ChainingReassignTargetNode,
        expr: &Expression,
    ) -> Result<ValueReturn, Error> {
        // evaluate right hand side first, in case it returns
        let res = self.interpret_expr(expr)?;
        if res.should_bubble_up {
            return Ok(res);
        }
        let reassigning_value = res.get_or_error()?;

        // evaluate left hand side
        let mut index_chain = Vec::with_capacity(node.follows.len());
        for expr in &node.follows {
            let value = self.interpret_clause_expr(expr)?;
            index_chain.push(value);
        }

        let iden = &node.base;

        let Some(current_root_value) = self.environment.get_variable_current_scope(iden) else {
            return match self.environment.get_variable_all_scope(iden) {
                Some(_) => Err(Error::VariableReadOnly(iden.create_name(self.input))),
                None => Err(Error::NotFoundVariable(iden.create_name(self.input))),
            };
        };

        match index_chain.split_last() {
            Some((last, chain)) => {
                let new_root_value = self.environment.heap.deep_copy_reassign_object(
                    current_root_value,
                    *last,
                    chain,
                    reassigning_value,
                )?;
                self.environment
                    .replace_variable_current_scope(iden, new_root_value)
            }
            None => self
                .environment
                .replace_variable_current_scope(iden, reassigning_value),
        };

        Ok(ValueReturn::none())
    }

    fn interpret_expr(&mut self, expr: &Expression) -> Result<ValueReturn, Error> {
        match expr {
            Expression::Return(expr) => self.interpret_return_expr(expr),
            Expression::When(nodes) => self.interpret_when_expr(nodes),
            Expression::Clause(node) => self.interpret_clause_expr(node).map(ValueReturn::new),
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
        // we cannot push_scope and then pop_scope here
        // since if a variable is create outside of an if-else or while scope
        // we want it to be not-readonly here
        // but we still want gc to delete values created in this scope
        // when the scope ends
        // TODO: find a better way for scope

        for stmt in &node.stmts {
            let res = self.interpret_stmt(stmt)?;
            if res.should_bubble_up {
                return Ok(res);
            }
        }

        match &node.last_expr {
            Some(expr) => self.interpret_expr(expr),
            None => Ok(ValueReturn::none()),
        }
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

    fn interpret_clause_expr(&mut self, node: &ClauseNode) -> Result<Value, Error> {
        match node {
            ClauseNode::Unary(node, op) => self.interpret_unary_op(node, *op),
            ClauseNode::Binary(node) => self.interpret_binary_op(node),
            ClauseNode::RawValue(node) => self.interpret_raw_value_node(node),
            ClauseNode::Identifier(node) => Ok(self.lookup_all_scope(node)),
            ClauseNode::Group(node) => self.interpret_clause_expr(node),
            ClauseNode::FnCall(node) => self.interpret_fn_call(node),
            ClauseNode::Subscription(node) => self.interpret_subscription(node),
        }
    }

    fn interpret_raw_value_node(&mut self, node: &RawValueNode) -> Result<Value, Error> {
        let val = match node {
            RawValueNode::Scalar(node) => self.interpret_scalar_expr(node),
            RawValueNode::ArrayLiteral(node) => self.interpret_array_literal(node)?,
            RawValueNode::MapLiteral(node) => self.interpret_map_literal(node)?,
            RawValueNode::FnDecl(node) => self.interpret_fn_decl(node)?,
        };
        Ok(val)
    }

    fn interpret_scalar_expr(&mut self, node: &ScalarNode) -> Value {
        match node {
            ScalarNode::Nil => Value::make_nil(),
            ScalarNode::Bool(v) => Value::make_bool(*v),
            ScalarNode::Integer(v) => Value::make_number(Number::Integer(*v)),
            ScalarNode::Floating(v) => Value::make_number(Number::Floating(*v)),
            ScalarNode::Str(v) => self
                .environment
                .insert_string_variable(v.string_from_source(self.input)),
        }
    }

    fn interpret_array_literal(&mut self, node: &ArrayLiteralNode) -> Result<Value, Error> {
        match node {
            ArrayLiteralNode::List(clauses) => {
                let mut arr = Vec::with_capacity(clauses.len());
                for expr in clauses {
                    let res = self.interpret_clause_expr(expr)?;
                    arr.push(res);
                }
                Ok(self.environment.insert_array_variable(arr))
            }
            ArrayLiteralNode::Repeat(node) => {
                let value = self.interpret_clause_expr(&node.value)?;
                let repeat = self.interpret_clause_expr(&node.repeat)?.to_index()?;
                Ok(self.environment.insert_array_variable(vec![value; repeat]))
            }
        }
    }

    fn interpret_map_literal(&mut self, node: &MapLiteralNode) -> Result<Value, Error> {
        let mut map = Map::new();
        for kv in &node.nodes {
            let raw_key = self.interpret_scalar_expr(&kv.key);
            let key = MapKey::convert_from_value(raw_key)?;
            let value = self.interpret_clause_expr(&kv.value)?;
            map.insert(key, value);
        }
        Ok(self.environment.insert_map_variable(map))
    }

    fn interpret_fn_decl(&mut self, node: &FnDeclNode) -> Result<Value, Error> {
        Ok(self.environment.insert_function_variable(Function {
            arg_ids: node.arg_names.iter().map(|a| a.get_id()).collect(),
            body: node.body.to_owned(),
        }))
    }

    fn interpret_subscription(&mut self, node: &SubscriptionNode) -> Result<Value, Error> {
        let indexer = self.interpret_clause_expr(&node.indexer)?;
        let indexee = self.interpret_clause_expr(&node.indexee)?;
        indexer.get_at_index(indexee, self.environment)
    }

    fn interpret_fn_call(&mut self, node: &FnCallNode) -> Result<Value, Error> {
        let value = self.interpret_clause_expr(&node.caller)?;
        match value {
            Value::Function(handle) => self.interpret_normal_fn_call_expr(handle, &node.args),
            Value::BuiltinFunction(function) => {
                self.intepret_builtin_fn_call_expr(function, &node.args)
            }
            _ => Err(Error::ValueNotCallable(value)),
        }
    }

    fn interpret_normal_fn_call_expr(
        &mut self,
        handle: GcHandle,
        args: &[Expression],
    ) -> Result<Value, Error> {
        let func = self.environment.get_function(handle)?;

        // Technically, we does not need the clone for args_ids and body here
        // since when intepreting functions, we cannot create reassign it to an new value
        // but we don't have a way in rust to formally express this
        // TODO: find a way to express this
        let Function { arg_ids, body } = func.clone();

        if arg_ids.len() != args.len() {
            return Err(Error::WrongNumberOfArgument(
                Value::Function(handle),
                arg_ids.len(),
                args.len(),
            ));
        }

        // create a new stack for the function call
        // currently, later args can reference sonner args
        // TODO: make this go away
        self.environment.push_scope();
        for (arg_id, arg_expr) in arg_ids.iter().zip(args.iter()) {
            let res = self.interpret_expr(arg_expr)?.get_or_error()?;
            self.environment.insert_variable_current_scope(*arg_id, res);
        }

        let result = self.interpret_block_node(&body);
        self.environment.pop_scope();

        // Convert the ValueReturn back to a raw Value.
        // If it was bubbling, it stops bubbling here because the function has returned.
        result.map(|vr| vr.get_or_unit())
    }

    fn intepret_builtin_fn_call_expr(
        &mut self,
        function: BuiltinFn,
        args: &[Expression],
    ) -> Result<Value, Error> {
        let args = args
            .iter()
            .map(|expr| self.interpret_expr(expr).and_then(|v| v.get_or_error()))
            .collect::<Result<_, _>>()?;
        function(self, args)
    }

    fn interpret_unary_op(&mut self, node: &ClauseNode, op: Token) -> Result<Value, Error> {
        let res = self.interpret_clause_expr(node)?;
        match op {
            Token::Not => match res.get_bool() {
                Some(v) => Ok(Value::make_bool(!v)),
                None => Err(Error::InvalidOperationOnType(op, res)),
            },
            Token::Minus => match res.get_number() {
                Some(v) => Ok(Value::make_number(-v)),
                None => Err(Error::InvalidOperationOnType(op, res)),
            },
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_binary_op(
        &mut self,
        BinaryOpNode { lhs, op, rhs }: &BinaryOpNode,
    ) -> Result<Value, Error> {
        let lhs_val = self.interpret_clause_expr(lhs)?;
        let rhs_val = self.interpret_clause_expr(rhs)?;
        let op = *op;

        match op {
            Token::Plus => self.interpret_add(lhs_val, rhs_val),
            Token::And | Token::Or => and_or(lhs_val, op, rhs_val),
            Token::Minus | Token::Star | Token::Slash | Token::Percentage => {
                binary_number(lhs_val, op, rhs_val)
            }
            Token::EqualEqual | Token::BangEqual => {
                let cmp = lhs_val.deep_eq(&rhs_val, self.environment)?;
                let ret = cmp == (op == Token::EqualEqual);
                Ok(Value::make_bool(ret))
            }
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                self.interpret_ordering(lhs_val, op, rhs_val)
            }
            _ => Err(Error::UnknownOperation(op)),
        }
    }

    fn interpret_when_expr(&mut self, cases: &[WhenArmNode]) -> Result<ValueReturn, Error> {
        for WhenArmNode { cond, expr } in cases {
            if self.is_truthy(cond)? {
                return self.interpret_clause_expr(expr).map(ValueReturn::new);
            }
        }
        Ok(ValueReturn::new(Value::Unit))
    }

    fn interpret_add(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        match (lhs, rhs) {
            (Value::Scalar(Scalar::Number(l)), Value::Scalar(Scalar::Number(r))) => {
                Ok(Value::make_number(l + r))
            }
            (Value::Str(l_id), Value::Str(r_id)) => {
                let l_str = self.environment.get_string(l_id)?;
                let r_str = self.environment.get_string(r_id)?;
                Ok(self
                    .environment
                    .insert_string_variable(l_str.to_owned() + r_str))
            }
            _ => Err(Error::MismatchType(Token::Plus, lhs, rhs)),
        }
    }

    fn interpret_ordering(&mut self, lhs: Value, op: Token, rhs: Value) -> Result<Value, Error> {
        let from_ord = |o: std::cmp::Ordering| match op {
            Token::Less => Ok(Value::make_bool(o.is_lt())),
            Token::LessEqual => Ok(Value::make_bool(o.is_le())),
            Token::Greater => Ok(Value::make_bool(o.is_gt())),
            Token::GreaterEqual => Ok(Value::make_bool(o.is_ge())),
            _ => Err(Error::UnknownOperation(op)),
        };
        match (lhs, rhs) {
            (Value::Scalar(l), Value::Scalar(r)) => from_ord(l.cmp(&r)),
            (Value::Str(l_handle), Value::Str(r_handle)) => {
                let l_str = self.environment.get_string(l_handle)?;
                let r_str = self.environment.get_string(r_handle)?;
                from_ord(l_str.cmp(r_str))
            }
            _ => Err(Error::InvalidOperationOnType(op, lhs)),
        }
    }

    fn lookup_all_scope(&self, node: &IdentifierNode) -> Value {
        self.environment
            .get_variable_all_scope(node)
            .unwrap_or(Value::make_nil())
    }

    fn is_truthy(&mut self, expr: &ClauseNode) -> Result<bool, Error> {
        let b = self.interpret_clause_expr(expr)?;
        match b.get_bool() {
            Some(v) => Ok(v),
            None => Err(Error::ConditionNotBool(b)),
        }
    }
}

fn and_or(lhs: Value, op: Token, rhs: Value) -> Result<Value, Error> {
    let Some(l) = lhs.get_bool() else {
        return Err(Error::InvalidOperationOnType(op, lhs));
    };
    let Some(r) = rhs.get_bool() else {
        return Err(Error::InvalidOperationOnType(op, rhs));
    };

    match op {
        Token::And => Ok(Value::make_bool(l && r)),
        Token::Or => Ok(Value::make_bool(l || r)),
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn binary_number(lhs: Value, op: Token, rhs: Value) -> Result<Value, Error> {
    let (Some(l), Some(r)) = (lhs.get_number(), rhs.get_number()) else {
        return Err(Error::MismatchType(Token::Minus, lhs, rhs));
    };

    let v = match op {
        Token::Plus => l + r,
        Token::Minus => l - r,
        Token::Star => l * r,
        Token::Percentage => l % r,
        Token::Slash => {
            if r.is_zero() {
                return Err(Error::DivideByZero);
            }
            l / r
        }
        _ => return Err(Error::UnknownOperation(op)),
    };
    Ok(Value::make_number(v))
}
