use thiserror::Error;

use crate::{
    ast::ScalarNode,
    id::Id,
    span::Span,
    token::Token,
    types::{TypeId, TypeInterner},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error(
        "Type mismatch in declaration of '{0:?}': assigned value does not match specified type."
    )]
    ExplicitTypeMismatch(Id, Span, TypeId, TypeId),

    #[error("Function return type mismatch.")]
    ExplicitFnReturnTypeMismatch(TypeId, TypeId),

    #[error("Binary operator `{0:?}` cannot be applied to types `{1:?}` and `{2:?}`.")]
    BinaryOpTypeMismatch(Token, TypeId, TypeId),

    #[error("Unary operator `{0:?}` cannot be applied to type `{1:?}`.")]
    UnaryOpTypeMismatch(Token, TypeId),

    #[error("The type `{0:?}` cannot be indexed by the type`{1:?}`.")]
    IndexOpTypeMismatch(TypeId, TypeId),

    #[error("Expected type `{0:?}`, but found `{1:?}`.")]
    ExpectedType(TypeId, TypeId),

    #[error("Invalid map key: keys must be strings, but found `{0:?}`.")]
    MapKeyMustBeString(ScalarNode),

    #[error("Undefined name: '{1:?}' is not defined in this scope.")]
    UndefinedIdentifier(Span, Id),

    #[error("Name collision: '{1:?}' is already declared in this scope.")]
    DuplicateDeclaration(Span, Id),
}

impl Error {
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
            Self::ExplicitTypeMismatch(id, _, declared, actual) => {
                format!(
                    "The value assigned to '{:?}' declared type `{}` but got assigned value of type `{}`.",
                    id,
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

            Self::MapKeyMustBeString(_) => {
                "Map keys must be strings. The provided key has an incompatible type.".to_string()
            }

            Self::UndefinedIdentifier(_, id) => {
                format!("The identifier '{id:?}' is not defined in the current scope.")
            }

            Self::DuplicateDeclaration(_, id) => {
                format!("The identifier '{id:?}' has already been declared.")
            }
        }
    }

    fn get_source_start(&self, input: &str) -> (usize, usize) {
        match self {
            Self::ExplicitTypeMismatch(_, s, _, _) => s.to_start_row_col(input),
            Self::UndefinedIdentifier(s, _) => s.to_start_row_col(input),
            Self::DuplicateDeclaration(s, _) => s.to_start_row_col(input),
            // Default to start of file if the variant doesn't carry a specific span
            _ => (1, 1),
        }
    }
}
