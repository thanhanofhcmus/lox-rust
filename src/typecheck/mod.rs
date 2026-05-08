mod environment;
mod error;
mod typechecker;

pub use environment::{Environment, Module, ModuleRegistry};
pub use error::TypecheckError;
pub use typechecker::TypeChecker;
