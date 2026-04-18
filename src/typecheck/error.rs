use thiserror::Error;

use crate::{ast::ScalarNode, id::Id, span::Span, token::Token, types::TypeId};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Type with name id {0:?} and position {1:?} have an explicit type mismatch")]
    // (Type span, Expected type, actual Type for now)
    // TODO: Update to be actual type and error message later if we we have a /resolved type id/things
    ExplitcitTypeDeclartionMismatchActualType(Id, Span),

    #[error("Function have an explicit return type mismatch")]
    // TODO: Make this mention function name
    ExplicitFnReturnTypeDeclMismatch(TypeId, TypeId),

    #[error("Binary operator {0:?} cannot apply to types {1:?} and {2:?}")]
    BinaryOpTypeMismatch(Token, TypeId, TypeId),

    #[error("Unary operator {0:?} cannot apply to type {1:?}")]
    UnaryOpTypeMismatch(Token, TypeId),

    #[error("Expected type {0:?}, got {1:?}")]
    ExpectedType(TypeId, TypeId),

    #[error("Map keys must be strings; got {0:?}")]
    MapKeyMustBeString(ScalarNode),

    #[error("Identifier {1:?} at {0:?} is not defined in scope")]
    UndefinedIdentifier(Span, Id),

    #[error("Identifier {1:?} at {0:?} is already declared in this scope")]
    DuplicateDeclaration(Span, Id),
}
