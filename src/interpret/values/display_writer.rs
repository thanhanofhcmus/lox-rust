use crate::interpret::{error::Error, Environment};

pub trait DisplayWriter {
    fn write_display(self, env: &Environment, w: &mut dyn std::io::Write) -> Result<(), Error>;
}
