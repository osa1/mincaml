use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::rc::Rc;

// TODO: Should really be an abstract type but we have to expose the field to be able to create
// fresh ones in another module
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Uniq(pub NonZeroU32);

impl fmt::Display for Uniq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x")?;
        fmt::LowerHex::fmt(&self.0, f)
    }
}

// NOTE: Display outputs start with '#' for non-user variables (generated, builtin, external)

#[derive(Debug, Clone)]
pub enum Var {
    User(UserVar),
    Generated(GeneratedVar),
    Builtin(BuiltinVar),
    External(ExternalVar),
}

impl Var {
    pub fn get_uniq(&self) -> Uniq {
        match self {
            Var::User(var) => var.get_unique(),
            Var::Generated(var) => var.get_unique(),
            Var::Builtin(var) => var.get_unique(),
            Var::External(var) => var.get_unique(),
        }
    }

    pub fn new_user(name: &str, uniq: Uniq) -> Var {
        Var::User(UserVar {
            name: name.into(),
            uniq,
        })
    }

    pub fn new_generated(phase: CompilerPhase, uniq: Uniq) -> Var {
        Var::Generated(GeneratedVar::new(phase, uniq))
    }

    pub fn new_builtin(name: &str, uniq: Uniq) -> Var {
        Var::Builtin(BuiltinVar {
            name: name.into(),
            uniq,
        })
    }

    pub fn name(&self) -> Rc<str> {
        match self {
            Var::User(var) => var.name(),
            Var::Generated(var) => var.name(),
            Var::Builtin(var) => var.name(),
            Var::External(var) => var.name(),
        }
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.get_uniq() == other.get_uniq()
    }
}

impl Eq for Var {}

impl Hash for Var {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_uniq().hash(state)
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::User(var) => var.fmt(f),
            Var::Generated(var) => var.fmt(f),
            Var::Builtin(var) => var.fmt(f),
            Var::External(var) => var.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserVar {
    name: Rc<str>,
    uniq: Uniq,
}

impl fmt::Display for UserVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.uniq.0)
    }
}

impl PartialEq<UserVar> for UserVar {
    fn eq(&self, other: &Self) -> bool {
        self.uniq == other.uniq
    }
}

impl Eq for UserVar {}

impl Hash for UserVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uniq.hash(state)
    }
}

impl UserVar {
    fn get_unique(&self) -> Uniq {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct GeneratedVar {
    name: Rc<str>,
    phase: CompilerPhase,
    uniq: Uniq,
}

impl GeneratedVar {
    fn new(phase: CompilerPhase, uniq: Uniq) -> GeneratedVar {
        GeneratedVar {
            name: format!("#{}_{:#X}", phase.display_str(), uniq.0).into(),
            phase,
            uniq,
        }
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}

impl fmt::Display for GeneratedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)
    }
}

impl PartialEq<GeneratedVar> for GeneratedVar {
    fn eq(&self, other: &Self) -> bool {
        self.uniq == other.uniq
    }
}

impl Eq for GeneratedVar {}

impl Hash for GeneratedVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uniq.hash(state)
    }
}

impl GeneratedVar {
    fn get_unique(&self) -> Uniq {
        self.uniq
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum CompilerPhase {
    Parser,
    // TyChecker,
    KNormal,
    // ANormal,
    ClosureConvert,
}

impl CompilerPhase {
    fn display_str(&self) -> &'static str {
        use CompilerPhase::*;
        match self {
            Parser => "p",
            // TyChecker => "tc",
            KNormal => "kn",
            // ANormal => "an",
            ClosureConvert => "cc",
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinVar {
    name: Rc<str>,
    uniq: Uniq,
}

impl fmt::Display for BuiltinVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#builtin[{}]", self.name)
    }
}

impl PartialEq<BuiltinVar> for BuiltinVar {
    fn eq(&self, other: &Self) -> bool {
        self.uniq == other.uniq
    }
}

impl Eq for BuiltinVar {}

impl Hash for BuiltinVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uniq.hash(state)
    }
}

impl BuiltinVar {
    fn get_unique(&self) -> Uniq {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ExternalVar {
    name: Rc<str>,
    uniq: Uniq,
}

impl fmt::Display for ExternalVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#ext[{}]", self.name)
    }
}

impl PartialEq<ExternalVar> for ExternalVar {
    fn eq(&self, other: &Self) -> bool {
        self.uniq == other.uniq
    }
}

impl Eq for ExternalVar {}

impl Hash for ExternalVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.uniq.hash(state)
    }
}

impl ExternalVar {
    fn get_unique(&self) -> Uniq {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}
