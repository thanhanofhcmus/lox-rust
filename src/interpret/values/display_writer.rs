use crate::identifier_registry::IdentifierRegistry;
use crate::interpret::{Environment, error::InterpretError};

/// Render a value to a writer using read-only access to the runtime
/// environment (for heap lookups on strings / arrays / maps / structs /
/// tuples) and the identifier registry (for struct and field names).
///
/// Takes the two stores directly rather than the full `BorrowContext` so the
/// error-formatting path can reuse the same rendering without needing a
/// writable interpreter context on hand.
pub trait DisplayWriter {
    fn write_display(
        self,
        env: &Environment,
        sb: &IdentifierRegistry,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError>;
}
