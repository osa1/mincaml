use crate::type_check::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepType {
    Word,
    Float,
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
