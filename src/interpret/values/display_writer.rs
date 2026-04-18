use crate::interpret::{Environment, error::InterpretError};

pub trait DisplayWriter {
    fn write_display(
        self,
        env: &Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError>;
}
