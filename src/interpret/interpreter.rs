use super::environment::Environment;
use super::error::Error;
use super::value::Value;
use crate::ast::*;
use crate::interpret::heap::GcHandle;
use crate::interpret::helper_values::MapKey;
use crate::interpret::value::{BuiltinFn, Function, VMap};
use crate::parse;
use crate::span::Span;
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
            let value = self.interpret_expr(expr)?.get_or_error()?;
            index_chain.push(value);
        }

        let iden = &node.base;

        let Some(current_root_value) = self.environment.get_variable_current_scope(iden) else {
            return match self.environment.get_variable_all_scope(iden) {
                Some(_) => Err(Error::VariableReadOnly(iden.create_name(self.input))),
                None => Err(Error::NotFoundVariable(iden.create_name(self.input))),
            };
        };

        if index_chain.is_empty() {
            self.environment
                .replace_variable_current_scope(iden, reassigning_value);
            return Ok(ValueReturn::none());
        }

        let new_root_value = self.environment.heap.deep_copy_reassign_object(
            current_root_value,
            &index_chain,
            reassigning_value,
        )?;
        self.environment
            .replace_variable_current_scope(iden, new_root_value);

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
        // we cannot push_scope and then pop_scope here
        // since if a variable is create outside of an if-else or while scope
        // we want it to be not-readonly here
        // but we still want gc to delete values created in this scope
        // when the scope ends
        // TODO: find a better way for scope

        for stmt in &node.stmts {
            let res = self.interpret_stmt(stmt)?;
            if res.should_bubble_up {
                // self.environment.pop_scope();
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
            PrimaryNode::Bool(v) => Value::Bool(*v),
            PrimaryNode::Integer(v) => Value::Integer(*v),
            PrimaryNode::Floating(v) => Value::Floating(*v),
            PrimaryNode::Str(v) => self.interpret_string_literal(*v)?,
            PrimaryNode::ArrayLiteral(node) => self.interpret_array_literal(node)?,
            PrimaryNode::MapLiteral(node) => self.interpret_map_literal(node)?,
            PrimaryNode::FnDecl(node) => self.interpret_fn_decl(node)?,
        };
        Ok(ValueReturn::new(val))
    }

    fn interpret_string_literal(&mut self, span: Span) -> Result<Value, Error> {
        Ok(self
            .environment
            .insert_string_variable(span.string_from_source(self.input)))
    }

    fn interpret_array_literal(&mut self, node: &ArrayLiteralNode) -> Result<Value, Error> {
        match node {
            ArrayLiteralNode::List(clauses) => {
                let mut arr = Vec::with_capacity(clauses.len());
                for expr in clauses {
                    let res = self.interpret_clause_expr(expr)?;
                    // Even in literals, we must check if an expression bubbled a return
                    // though usually return isn't allowed here syntactically.
                    arr.push(res.get_or_error()?);
                }
                Ok(self.environment.insert_array_variable(arr))
            }
            ArrayLiteralNode::Repeat(node) => {
                let val_res = self.interpret_clause_expr(&node.value)?;
                let rep_res = self.interpret_clause_expr(&node.repeat)?;

                let value = val_res.get_or_error()?;
                let repeat = rep_res.get_or_error()?.to_index()?;

                let mut arr = Vec::with_capacity(repeat);
                for _ in 0..repeat {
                    arr.push(value);
                }
                Ok(self.environment.insert_array_variable(arr))
            }
        }
    }

    fn interpret_map_literal(&mut self, node: &MapLiteralNode) -> Result<Value, Error> {
        let mut map = VMap::new();
        for kv in &node.nodes {
            let raw_key = self.interpret_primary_expr(&kv.key)?.get_or_error()?;
            let key = MapKey::convert_from_value(raw_key, self.environment)?;
            let value = self.interpret_expr(&kv.value)?.get_or_error()?;
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

    fn interpret_normal_fn_call_expr(
        &mut self,
        node: &FnCallNode,
        handle: GcHandle,
        // value::Function { arg_ids, body }: &Function,
    ) -> Result<Value, Error> {
        let func = self.environment.get_function(handle)?;

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
            Token::Plus => self.interpret_add(lhs_val, rhs_val),
            Token::Minus => subtract(lhs_val, rhs_val),
            Token::Star => times(lhs_val, rhs_val),
            Token::Slash => divide(lhs_val, rhs_val),
            Token::Percentage => modulo(lhs_val, rhs_val),
            Token::And | Token::Or => and_or(lhs_val, op, rhs_val),
            Token::EqualEqual | Token::BangEqual => {
                self.interpret_cmp(lhs_val, op, rhs_val).map(Value::Bool)
            }
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                ordering(lhs_val, op, rhs_val)
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
        let mut value = match &node.base {
            ChainingBase::Primary(v) => self.interpret_primary_expr(v)?.get_or_error()?,
            ChainingBase::Group(v) => self.interpret_expr(v)?.get_or_error()?,
            ChainingBase::Identifier(v) => self.lookup_all_scope(v),
        };
        for follow in &node.follows {
            value = match follow {
                // TODO: this one is not working right?, this is for a.b.c.d, should be lookup on the value's scope
                ChainingFollow::Identifier(_node) => unimplemented!(),
                ChainingFollow::FnCall(node) => match value {
                    Value::Function(handle) => self.interpret_normal_fn_call_expr(node, handle)?,
                    Value::BuiltinFunction(function) => {
                        self.intepret_builtin_fn_call_expr(node, function)?
                    }
                    _ => return Err(Error::ValueNotCallable(value)),
                },
                ChainingFollow::Index(indexee_expr) => {
                    let indexee = self.interpret_expr(indexee_expr)?.get_or_error()?;
                    value.get_at_index(indexee, self.environment)?
                }
            }
        }
        Ok(value)
    }

    fn interpret_cmp(&self, lhs: Value, op: Token, rhs: Value) -> Result<bool, Error> {
        let cmp = lhs.deep_eq(&rhs, self.environment)?;
        Ok(cmp == (op == Token::EqualEqual))
    }

    fn interpret_add(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        use Value::*;
        match (&lhs, &rhs) {
            (Integer(l), Integer(r)) => Ok(Integer(l + r)),
            (Floating(l), Integer(r)) => Ok(Floating(l + (*r as f64))),
            (Integer(l), Floating(r)) => Ok(Floating((*l as f64) + r)),
            (Floating(l), Floating(r)) => Ok(Floating(l + r)),
            (Str(l), Str(r)) => self.interpret_add_string(*l, *r),
            _ => Err(Error::MismatchType(Token::Plus, lhs, rhs)),
        }
    }

    fn interpret_add_string(&mut self, lhs: GcHandle, rhs: GcHandle) -> Result<Value, Error> {
        let l_str = self.environment.get_string(lhs)?;
        let r_str = self.environment.get_string(rhs)?;
        Ok(self
            .environment
            .insert_string_variable(l_str.to_owned() + r_str))
    }

    fn lookup_all_scope(&self, node: &IdentifierNode) -> Value {
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

fn ordering(lhs: Value, op: Token, rhs: Value) -> Result<Value, Error> {
    let from_ord = |o: std::cmp::Ordering| match op {
        Token::Less => Ok(Value::Bool(o.is_lt())),
        Token::LessEqual => Ok(Value::Bool(o.is_le())),
        Token::Greater => Ok(Value::Bool(o.is_gt())),
        Token::GreaterEqual => Ok(Value::Bool(o.is_ge())),
        _ => Err(Error::UnknownOperation(op)),
    };
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => from_ord(l.cmp(&r)),
        (Floating(l), Integer(r)) => from_ord(l.total_cmp(&(r as f64))),
        (Integer(l), Floating(r)) => from_ord((l as f64).total_cmp(&r)),
        (Floating(l), Floating(r)) => from_ord(l.total_cmp(&r)),

        // TODO: fix this
        (Str(l), Str(r)) => from_ord(l.cmp(&r)),
        _ => Err(Error::InvalidOperationOnType(op, lhs)),
    }
}

fn and_or(lhs: Value, op: Token, rhs: Value) -> Result<Value, Error> {
    let Value::Bool(l) = lhs else {
        return Err(Error::InvalidOperationOnType(op, lhs));
    };
    let Value::Bool(r) = rhs else {
        return Err(Error::InvalidOperationOnType(op, rhs));
    };

    match op {
        Token::And => Ok(Value::Bool(l && r)),
        Token::Or => Ok(Value::Bool(l || r)),
        _ => Err(Error::UnknownOperation(op)),
    }
}

fn subtract(lhs: Value, rhs: Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l - r)),
        (Floating(l), Integer(r)) => Ok(Floating(l - (r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((l as f64) - r)),
        (Floating(l), Floating(r)) => Ok(Floating(l - r)),
        _ => Err(Error::MismatchType(Token::Minus, lhs, rhs)),
    }
}

fn times(lhs: Value, rhs: Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l * r)),
        (Floating(l), Integer(r)) => Ok(Floating(l * (r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((l as f64) * r)),
        (Floating(l), Floating(r)) => Ok(Floating(l * r)),
        _ => Err(Error::MismatchType(Token::Star, lhs, rhs)),
    }
}

fn divide(lhs: Value, rhs: Value) -> Result<Value, Error> {
    use Value::*;
    match rhs {
        Integer(0) => return Err(Error::DivideByZero),
        Floating(0.0) => return Err(Error::DivideByZero),
        _ => {}
    }
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => {
            if l % r == 0 {
                Ok(Integer(l / r))
            } else {
                Ok(Floating(l as f64 / r as f64))
            }
        }
        (Floating(l), Integer(r)) => Ok(Floating(l / (r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((l as f64) / r)),
        (Floating(l), Floating(r)) => Ok(Floating(l / r)),
        _ => Err(Error::MismatchType(Token::Slash, lhs, rhs)),
    }
}

fn modulo(lhs: Value, rhs: Value) -> Result<Value, Error> {
    use Value::*;
    match (lhs, rhs) {
        (Integer(l), Integer(r)) => Ok(Integer(l % r)),
        (Floating(l), Integer(r)) => Ok(Floating(l % (r as f64))),
        (Integer(l), Floating(r)) => Ok(Floating((l as f64) % r)),
        (Floating(l), Floating(r)) => Ok(Floating(l % r)),
        _ => Err(Error::MismatchType(Token::PercentLPointParent, lhs, rhs)),
    }
}
