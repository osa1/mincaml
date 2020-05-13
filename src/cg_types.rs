use std::fmt;

use crate::type_check::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepType {
    Word,
    Float,
}

impl fmt::Display for RepType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RepType::Word => "W".fmt(f),
            RepType::Float => "F".fmt(f),
        }
    }
}

impl From<&Type> for RepType {
    fn from(ty: &Type) -> RepType {
        match ty {
            Type::Var(_) => panic!("Type variable in RepType::from"),
            Type::Float => RepType::Float,
            _ => RepType::Word,
        }
    }
}
