use std::hash::{DefaultHasher, Hash, Hasher};

const DEBUG_NAME_SIZE: usize = 32;

#[derive(derive_more::Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[debug("Id({hash:020}{})", self.debug_name())]
pub struct Id {
    hash: u64,
    #[cfg(debug_assertions)]
    name: [u8; DEBUG_NAME_SIZE],
}

impl Id {
    pub fn new(name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let hash = hasher.finish();

        #[cfg(debug_assertions)]
        let name_bytes = {
            let mut buf = [0u8; DEBUG_NAME_SIZE];
            let bytes = name.as_bytes();
            let len = bytes.len().min(DEBUG_NAME_SIZE);
            buf[..len].copy_from_slice(&bytes[..len]);
            buf
        };

        Id {
            hash,
            #[cfg(debug_assertions)]
            name: name_bytes,
        }
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn debug_name(&self) -> String {
        {
            let end = self
                .name
                .iter()
                .position(|&b| b == 0)
                .unwrap_or(DEBUG_NAME_SIZE);
            let s = std::str::from_utf8(&self.name[..end]).unwrap_or("invalid");
            format!(", \"{s}\"")
        }
    }
}
