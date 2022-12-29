use crate::{hlir::Condition, index};

#[derive(Debug, PartialEq, Eq)]
pub enum Register {
    GeneralPurpose { index: u8 },
    StackPointer,
    FramePointer,
    LinkRegister,
}

crate::impl_self_dependency_eq!(Register);

impl core::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::GeneralPurpose { index } => write!(f, "w{}", index),
            Register::StackPointer => f.write_str("sp"),
            Register::FramePointer => f.write_str("fp"),
            Register::LinkRegister => f.write_str("lr"),
        }
    }
}

#[derive(Debug)]
pub enum C<'a> {
    Label(&'a str),
    U(u64),
}

impl core::fmt::Display for C<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            C::Label(s) => f.write_str(s),
            C::U(n) => n.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum CanBeConstant<'a> {
    Constant(C<'a>),
    Register(Register),
}

impl core::fmt::Display for CanBeConstant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CanBeConstant::Constant(c) => c.fmt(f),
            CanBeConstant::Register(r) => r.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Op<'a> {
    Ret,
    Mov {
        target: Register,
        source: CanBeConstant<'a>,
    },
    MvN {
        target: Register,
        source: CanBeConstant<'a>,
    },
    Cmp {
        register: Register,
        data: CanBeConstant<'a>,
    },
    Cset {
        target: Register,
        condition: Condition,
    },
    Add {
        target: Register,
        lhs: Register,
        rhs: CanBeConstant<'a>,
    },
    Sub {
        target: Register,
        lhs: Register,
        rhs: CanBeConstant<'a>,
    },
}
