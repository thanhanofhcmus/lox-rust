use crate::interpret::{error::InterpretError, interpreter::BorrowContext};

pub trait DisplayWriter {
    fn write_display(
        self,
        ctx: &BorrowContext,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError>;
}
