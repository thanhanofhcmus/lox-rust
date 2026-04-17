use std::hash::{DefaultHasher, Hash, Hasher};

const DEBUG_NAME_SIZE: usize = 16;

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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn equal_names_produce_equal_ids() {
        assert_eq!(Id::new("foo"), Id::new("foo"));
    }

    #[test]
    fn different_names_produce_different_ids() {
        assert_ne!(Id::new("foo"), Id::new("bar"));
    }

    #[test]
    fn empty_name_is_stable() {
        assert_eq!(Id::new(""), Id::new(""));
    }

    #[test]
    fn ids_are_copy() {
        // compile-time check: Id: Copy means we can use an id twice without clone
        let a = Id::new("x");
        let b = a;
        let c = a;
        assert_eq!(b, c);
    }

    #[cfg(debug_assertions)]
    #[test]
    fn debug_includes_short_name() {
        let formatted = format!("{:?}", Id::new("foo"));
        assert!(
            formatted.contains("foo"),
            "expected debug output to contain the name snippet, got: {formatted}"
        );
    }

    #[cfg(debug_assertions)]
    #[test]
    fn debug_truncates_long_name_to_16_bytes() {
        // DEBUG_NAME_SIZE = 16
        let long = "abcdefghijklmnopqrstuvwxyz";
        let formatted = format!("{:?}", Id::new(long));
        assert!(
            formatted.contains("abcdefghijklmnop"),
            "expected first 16 bytes in debug output, got: {formatted}"
        );
        assert!(
            !formatted.contains("abcdefghijklmnopq"),
            "expected debug output truncated at 16 bytes, got: {formatted}"
        );
    }
}
