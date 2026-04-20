use thiserror::Error;

use crate::{
    ast::IdentifierNode,
    token::Token,
    types::{TypeId, TypeInterner},
};

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error(
        "Type mismatch in declaration of '{:?}': assigned value does not match specified type."
        , _0.id)]
    ExplicitTypeMismatch(IdentifierNode, TypeId, TypeId),

    #[error("Function return type mismatch.")]
    ExplicitFnReturnTypeMismatch(TypeId, TypeId),

    #[error("Binary operator `{0:?}` cannot be applied to types `{1:?}` and `{2:?}`.")]
    BinaryOpTypeMismatch(Token, TypeId, TypeId),

    #[error("Unary operator `{0:?}` cannot be applied to type `{1:?}`.")]
    UnaryOpTypeMismatch(Token, TypeId),

    #[error("The type `{0:?}` cannot be indexed by the type`{1:?}`.")]
    IndexOpTypeMismatch(TypeId, TypeId),

    #[error("The type `{0:?}` is not a callable")]
    TypeIsNoCallable(TypeId),

    #[error("Callable `{0:?}` accepts {1} number of arguments but received {2}")]
    WrongNumberOfArgument(TypeId, usize, usize),

    #[error("Callable `{0:?}` received wrong argument types {1:?}")]
    WrongArgumentTypes(TypeId, Vec<(usize, TypeId, TypeId)>),

    #[error("Expected type `{0:?}`, but found `{1:?}`.")]
    ExpectedType(TypeId, TypeId),

    #[error("The type `{0:?}` is not struct and used in struct literal")]
    TypeIdNotStructInLiteral(TypeId),

    #[error("Undefined name: '{:?}' is not defined in this scope.", _0.id)]
    UndefinedIdentifier(IdentifierNode),

    #[error("Name collision: '{:?}' is already declared in this scope.", _0.id)]
    DuplicateDeclaration(IdentifierNode),

    #[error("Internal: Type with id {0:?} should have been declared but not found")]
    TypeIsNotDeclared(TypeId),

    #[error("Struct `{0:?}` expects {1} field(s) but {2} were provided")]
    StructFieldCountMismatch(TypeId, usize, usize),

    #[error("Struct `{0:?}` has no field '{:?}'", _1.id)]
    StructFieldNameMismatch(TypeId, IdentifierNode),

    #[error("Field of struct `{0:?}` expects type `{1:?}` but got `{2:?}`")]
    StructFieldTypeMismatch(TypeId, TypeId, TypeId),
}

impl TypecheckError {
    pub fn generate_user_facing_error(
        &self,
        source_name: Option<&str>,
        input: &str,
        interner: &TypeInterner,
    ) -> String {
        let (line, col) = self.get_source_start(input);

        // Resolve the technical error into a readable description
        let description = self.resolve_description(interner);

        let source_name = source_name.map(|v| v.to_string()).unwrap_or_default();
        let source_line = input.lines().nth(line.saturating_sub(1)).unwrap_or("");
        let line_label = line.to_string();
        let gutter = " ".repeat(line_label.len());
        let pointer = " ".repeat(col.saturating_sub(1));

        format!(
            "Type Error: {description}\n\
               --> {source_name}:{line}:{col}\n\
             {gutter} |\n\
             {line_label} | {source_line}\n\
             {gutter} | {pointer}^--- here"
        )
    }

    /// Internal helper to turn TypeIds into names like "Array<Number>"
    fn resolve_description(&self, interner: &TypeInterner) -> String {
        match self {
            Self::ExplicitTypeMismatch(node, declared, actual) => {
                format!(
                    "The value assigned to '{:?}' is declared as type `{}` but got assigned value of type `{}`.",
                    node.id,
                    interner.generate_readable_name(*declared),
                    interner.generate_readable_name(*actual)
                )
            }

            Self::ExplicitFnReturnTypeMismatch(expected, actual) => format!(
                "The function was expected to return `{}`, but it returns `{}`.",
                interner.generate_readable_name(*expected),
                interner.generate_readable_name(*actual)
            ),

            Self::BinaryOpTypeMismatch(op, left, right) => format!(
                "The operator `{:?}` is not defined for `{}` and `{}`.",
                op,
                interner.generate_readable_name(*left),
                interner.generate_readable_name(*right)
            ),

            Self::IndexOpTypeMismatch(indexer_type_id, indexee_type_id) => format!(
                "The type `{}` cannot be indexed by the type `{}`.",
                interner.generate_readable_name(*indexer_type_id),
                interner.generate_readable_name(*indexee_type_id)
            ),

            Self::TypeIsNoCallable(caller_type_id) => format!(
                "The type `{}` is not a function or built-in function.",
                interner.generate_readable_name(*caller_type_id)
            ),

            Self::WrongNumberOfArgument(caller_type_id, expected_size, actual_size) => {
                format!(
                    "The function of type `{}` is expected to received {} of arguments received {} instead.",
                    interner.generate_readable_name(*caller_type_id),
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
                            interner.generate_readable_name(*expected),
                            interner.generate_readable_name(*actual)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",\n");

                format!(
                    "The function of type `{}` received wrong argument type(s):\n{}",
                    interner.generate_readable_name(*caller_type_id),
                    wrong_type_str
                )
            }

            Self::TypeIdNotStructInLiteral(type_id) => format!(
                "The type `{}` used in a struct literal is not a struct type.",
                interner.generate_readable_name(*type_id),
            ),

            Self::UnaryOpTypeMismatch(op, type_id) => format!(
                "The operator `{:?}` cannot be applied to type `{}`.",
                op,
                interner.generate_readable_name(*type_id)
            ),

            Self::ExpectedType(expected, actual) => format!(
                "Expected type `{}`, but found `{}`.",
                interner.generate_readable_name(*expected),
                interner.generate_readable_name(*actual)
            ),

            Self::UndefinedIdentifier(node) => {
                format!(
                    "The identifier '{:?}' is not defined in the current scope.",
                    node.id
                )
            }

            Self::DuplicateDeclaration(node) => {
                format!("The identifier '{:?}' has already been declared.", node.id)
            }

            Self::TypeIsNotDeclared(_) => self.to_string(),

            Self::StructFieldCountMismatch(struct_type_id, expected, actual) => format!(
                "Struct `{}` expects {} field(s) but {} were provided.",
                interner.generate_readable_name(*struct_type_id),
                expected,
                actual,
            ),

            Self::StructFieldNameMismatch(struct_type_id, node) => format!(
                "Struct `{}` has no field '{:?}'.",
                interner.generate_readable_name(*struct_type_id),
                node.id,
            ),

            Self::StructFieldTypeMismatch(struct_type_id, expected, actual) => format!(
                "Field of struct `{}` expects type `{}` but got `{}`.",
                interner.generate_readable_name(*struct_type_id),
                interner.generate_readable_name(*expected),
                interner.generate_readable_name(*actual),
            ),
        }
    }

    fn get_source_start(&self, input: &str) -> (usize, usize) {
        match self {
            Self::ExplicitTypeMismatch(node, _, _) => node.span.to_start_row_col(input),
            Self::UndefinedIdentifier(node) => node.span.to_start_row_col(input),
            Self::DuplicateDeclaration(node) => node.span.to_start_row_col(input),
            Self::StructFieldNameMismatch(_, node) => node.span.to_start_row_col(input),
            // Default to start of file if the variant doesn't carry a specific span
            _ => (1, 1),
        }
    }
}
