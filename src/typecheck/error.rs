use crate::{
    identifier_registry::{Identifier, IdentifierRegistry},
    token::Token,
    types::{TypeId, TypeInterner},
};

#[derive(Debug)]
pub enum TypecheckError {
    ExplicitTypeMismatch(Identifier, TypeId, TypeId),
    ExplicitFnReturnTypeMismatch(TypeId, TypeId),
    BinaryOpTypeMismatch(Token, TypeId, TypeId),
    UnaryOpTypeMismatch(Token, TypeId),
    IndexOpTypeMismatch(TypeId, TypeId),
    TypeCannotBeUsedAsMapKey(TypeId),
    TypeIsNotCallable(TypeId),
    WrongNumberOfArgument(TypeId, usize, usize),
    WrongArgumentTypes(TypeId, Vec<(usize, TypeId, TypeId)>),
    ExpectedType(TypeId, TypeId),
    TypeIdNotStructInLiteral(TypeId),
    UndefinedVariableIdentifier(Identifier),
    UndefinedTypeIdentifier(Identifier),
    DuplicateDeclaration(Identifier),
    TypeIsNotDeclaredInternal(TypeId),
    TypeIsNotStruct(TypeId),
    StructFieldCountMismatch(TypeId, usize, usize),
    StructFieldNotExist(TypeId, Identifier),
    StructFieldTypeMismatch(TypeId, TypeId, TypeId),
    DuplicateStructLiteralField(Identifier),
    TupleIndexOutOfBound(TypeId, usize, usize),
    CannotDestructureAsTuple(TypeId),
    TupleDestructureArityMismatch { expected: usize, actual: usize },
}

impl TypecheckError {
    pub fn generate_user_facing_error(
        &self,
        source_name: Option<&str>,
        input: &str,
        ir: &IdentifierRegistry,
        interner: &TypeInterner,
    ) -> String {
        let line_position = if let Some((line, col)) = self.get_source_start(input) {
            let source_name = source_name.map(|v| v.to_string()).unwrap_or_default();
            let source_line = input.lines().nth(line.saturating_sub(1)).unwrap_or("");
            let line_label = line.to_string();
            let gutter = " ".repeat(line_label.len());
            let pointer = " ".repeat(col.saturating_sub(1));
            format!(
                "\n
                 --> {source_name}:{line}:{col}\n\
                 {gutter} |\n\
                 {line_label} | {source_line}\n\
                 {gutter} | {pointer}^--- here"
            )
        } else {
            "".to_string()
        };

        // Resolve the technical error into a readable description
        let description = self.resolve_description(ir, interner);

        format!("Type Error: {description}{line_position}")
    }

    /// Internal helper to turn TypeIds into names like "Array<Number>"
    pub fn resolve_description(&self, ir: &IdentifierRegistry, interner: &TypeInterner) -> String {
        match self {
            Self::ExplicitTypeMismatch(node, declared, actual) => {
                format!(
                    "The value assigned to '{}' is declared as type `{}` but got assigned value of type `{}`.",
                    ir.get_or_unknown(node.id),
                    interner.generate_readable_name(ir, *declared),
                    interner.generate_readable_name(ir, *actual)
                )
            }

            Self::ExplicitFnReturnTypeMismatch(expected, actual) => format!(
                "The function was expected to return `{}`, but it returns `{}`.",
                interner.generate_readable_name(ir, *expected),
                interner.generate_readable_name(ir, *actual)
            ),

            Self::BinaryOpTypeMismatch(op, left, right) => format!(
                "The operator `{op}` is not defined for `{}` and `{}`.",
                interner.generate_readable_name(ir, *left),
                interner.generate_readable_name(ir, *right)
            ),

            Self::IndexOpTypeMismatch(indexer_type_id, indexee_type_id) => format!(
                "The type `{}` cannot be indexed by the type `{}`.",
                interner.generate_readable_name(ir, *indexer_type_id),
                interner.generate_readable_name(ir, *indexee_type_id)
            ),

            Self::TypeCannotBeUsedAsMapKey(type_id) => format!(
                "The type `{}` is cannot be used as map key.",
                interner.generate_readable_name(ir, *type_id)
            ),

            Self::TypeIsNotCallable(caller_type_id) => format!(
                "The type `{}` is not a function or built-in function.",
                interner.generate_readable_name(ir, *caller_type_id)
            ),

            Self::WrongNumberOfArgument(caller_type_id, expected_size, actual_size) => {
                format!(
                    "The function of type `{}` is expected to received {} of arguments received {} instead.",
                    interner.generate_readable_name(ir, *caller_type_id),
                    *expected_size,
                    *actual_size,
                )
            }

            Self::WrongArgumentTypes(caller_type_id, wrong_types) => {
                let wrong_type_str = wrong_types
                    .iter()
                    .map(|(index, expected, actual)| {
                        format!(
                            "argument at position {} expect type `{}` but got type `{}`",
                            *index,
                            interner.generate_readable_name(ir, *expected),
                            interner.generate_readable_name(ir, *actual)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",\n");

                format!(
                    "The function of type `{}` received wrong argument type(s):\n{}",
                    interner.generate_readable_name(ir, *caller_type_id),
                    wrong_type_str
                )
            }

            Self::TypeIdNotStructInLiteral(type_id) => format!(
                "The type `{}` used in a struct literal is not a struct type.",
                interner.generate_readable_name(ir, *type_id),
            ),

            Self::UnaryOpTypeMismatch(op, type_id) => format!(
                "The operator `{op}` cannot be applied to type `{}`.",
                interner.generate_readable_name(ir, *type_id)
            ),

            Self::ExpectedType(expected, actual) => format!(
                "Expected type `{}`, but found `{}`.",
                interner.generate_readable_name(ir, *expected),
                interner.generate_readable_name(ir, *actual)
            ),

            Self::UndefinedVariableIdentifier(node) => {
                format!(
                    "The variable '{}' is not defined in the visible scope.",
                    ir.get_or_unknown(node.id),
                )
            }

            Self::UndefinedTypeIdentifier(node) => {
                format!(
                    "The type '{}' is not defined in the visible scope.",
                    ir.get_or_unknown(node.id),
                )
            }

            Self::DuplicateDeclaration(node) => {
                format!(
                    "The identifier '{}' has already been declared.",
                    ir.get_or_unknown(node.id),
                )
            }

            Self::TypeIsNotDeclaredInternal(type_id) => format!(
                "Type with id {} should have been declared but not found. This is an internal error.",
                interner.generate_readable_name(ir, *type_id),
            ),

            Self::TypeIsNotStruct(type_id) => format!(
                "The type `{}` is not a struct.",
                interner.generate_readable_name(ir, *type_id)
            ),

            Self::StructFieldCountMismatch(struct_type_id, expected, actual) => format!(
                "Struct `{}` expects {} field(s) but {} were provided.",
                interner.generate_readable_name(ir, *struct_type_id),
                expected,
                actual,
            ),

            Self::StructFieldNotExist(struct_type_id, node) => format!(
                "Struct `{}` has no field '{:?}'.",
                interner.generate_readable_name(ir, *struct_type_id),
                ir.get_or_unknown(node.id),
            ),

            Self::StructFieldTypeMismatch(struct_type_id, expected, actual) => format!(
                "Field of struct `{}` expects type `{}` but got `{}`.",
                interner.generate_readable_name(ir, *struct_type_id),
                interner.generate_readable_name(ir, *expected),
                interner.generate_readable_name(ir, *actual),
            ),

            Self::DuplicateStructLiteralField(node) => format!(
                "Field '{:?}' is set more than once in this struct literal.",
                ir.get_or_unknown(node.id),
            ),

            Self::TupleIndexOutOfBound(struct_type_id, expected, actual) => format!(
                "Tuple `{}` has {} members(s) but an index at {} were provided.",
                interner.generate_readable_name(ir, *struct_type_id),
                expected,
                actual,
            ),

            Self::CannotDestructureAsTuple(type_id) => format!(
                "Type `{}` cannot be destructured as a tuple.",
                interner.generate_readable_name(ir, *type_id),
            ),

            Self::TupleDestructureArityMismatch { expected, actual } => {
                format!("Tuple destructuring expected {expected} member(s) but type has {actual}.",)
            }
        }
    }

    fn get_source_start(&self, input: &str) -> Option<(usize, usize)> {
        match self {
            Self::ExplicitTypeMismatch(node, _, _)
            | Self::UndefinedVariableIdentifier(node)
            | Self::UndefinedTypeIdentifier(node)
            | Self::DuplicateDeclaration(node)
            | Self::StructFieldNotExist(_, node)
            | Self::DuplicateStructLiteralField(node) => Some(node.span.to_start_row_col(input)),
            _ => None,
        }
    }
}
