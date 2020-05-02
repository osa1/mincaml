use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Hash, Clone)]
pub enum Var {
    User(UserVar),
    Generated(GeneratedVar),
    Builtin(BuiltinVar),
    External(ExternalVar),
}

// NOTE: Not thread-safe!
static mut NEXT_UNIQ: u32 = 0;

fn fresh_uniq() -> u32 {
    let uniq = unsafe { NEXT_UNIQ };
    unsafe {
        NEXT_UNIQ += 1;
    }
    uniq
}

impl Var {
    fn get_unique(&self) -> u32 {
        match self {
            Var::User(var) => var.get_unique(),
            Var::Generated(var) => var.get_unique(),
            Var::Builtin(var) => var.get_unique(),
            Var::External(var) => var.get_unique(),
        }
    }

    pub fn user(name: &str) -> Var {
        Var::User(UserVar {
            name: name.into(),
            uniq: fresh_uniq(),
        })
    }

    pub fn fresh(phase: CompilerPhase) -> Var {
        Var::Generated(GeneratedVar {
            phase,
            uniq: fresh_uniq(),
        })
    }

    pub fn builtin(name: &str) -> Var {
        Var::Builtin(BuiltinVar {
            name: name.into(),
            uniq: fresh_uniq(),
        })
    }

    pub fn name(&self) -> Rc<str> {
        match self {
            Var::User(var) => var.name(),
            Var::Generated(var) => {
                panic!("Generated variables don't have names");
            }
            Var::Builtin(var) => var.name(),
            Var::External(var) => var.name(),
        }
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.get_unique() == other.get_unique()
    }
}

impl Eq for Var {}

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
    uniq: u32,
}

impl fmt::Display for UserVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.uniq)
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
    fn get_unique(&self) -> u32 {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct GeneratedVar {
    phase: CompilerPhase,
    uniq: u32,
}

impl fmt::Display for GeneratedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}_{:#X}", self.phase.display_str(), self.uniq)
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
    fn get_unique(&self) -> u32 {
        self.uniq
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum CompilerPhase {
    Parser,
    TyChecker,
    KNormal,
    ANormal,
    ClosureConvert,
}

impl CompilerPhase {
    fn display_str(&self) -> &'static str {
        use CompilerPhase::*;
        match self {
            Parser => "p",
            TyChecker => "tc",
            KNormal => "kn",
            ANormal => "an",
            ClosureConvert => "cc",
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct BuiltinVar {
    name: Rc<str>,
    uniq: u32,
}

impl fmt::Display for BuiltinVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#builtin[{}]", self.name)
    }
}

impl BuiltinVar {
    fn get_unique(&self) -> u32 {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ExternalVar {
    name: Rc<str>,
    uniq: u32,
}

impl fmt::Display for ExternalVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#ext[{}]", self.name)
    }
}

impl ExternalVar {
    fn get_unique(&self) -> u32 {
        self.uniq
    }

    fn name(&self) -> Rc<str> {
        self.name.clone()
    }
}
