use std::ops::Range;

use crate::index;

pub trait Architecture {
    fn index_from_register(name: &str) -> Option<index::Register>;
    fn capabilities_set() -> RegisterCapabilitiesSet;
}

// TODO: callee/caller reserved registers

pub struct RegisterCapabilitiesSet {
    pub regular_registers: Range<index::Register>,
    pub stack_pointer: Option<index::Register>,
    pub frame_pointer: Option<index::Register>,
    pub flags_register: Option<index::Register>,
}

impl RegisterCapabilitiesSet {
    pub const fn new(regular_registers: Range<index::Register>) -> Self {
        Self {
            regular_registers,
            stack_pointer: None,
            frame_pointer: None,
            flags_register: None,
        }
    }
    pub const fn with_stack_pointer(mut self, stack_pointer: index::Register) -> Self {
        self.stack_pointer = Some(stack_pointer);
        self
    }
    pub const fn with_frame_pointer(mut self, frame_pointer: index::Register) -> Self {
        self.frame_pointer = Some(frame_pointer);
        self
    }
    pub const fn with_flags_register(mut self, flags_register: index::Register) -> Self {
        self.flags_register = Some(flags_register);
        self
    }
}

pub struct X86_64Nasm;

mod x86_64_nasm {
    use crate::index;
    #[derive(PartialEq, PartialOrd, Clone, Copy)]
    #[repr(u8)]
    pub enum Register {
        Rsp,
        Rbp,
        Rax,
        Rbx,
        Rcx,
        Rdx,
        Rsi,
        Rdi,
        R9,
        R10,
        R11,
        R12,
        R13,
        R14,
        R15,
    }

    // some fills to use ranges with it
    impl Register {
        pub const COUNT: usize = 15;
        const fn from_number(num: usize) -> Option<Self> {
            if num > Self::COUNT {
                None
            } else {
                Some(unsafe { std::mem::transmute(num as u8) })
            }
        }
        pub const fn as_index(self) -> index::Register {
            unsafe { index::Register::from_index(self as usize) }
        }
    }

    impl std::iter::Step for Register {
        fn steps_between(start: &Self, end: &Self) -> Option<usize> {
            (*end as usize).checked_sub(*start as usize)
        }

        fn forward_checked(start: Self, count: usize) -> Option<Self> {
            (start as usize)
                .checked_add(count)
                .and_then(Self::from_number)
        }

        fn backward_checked(start: Self, count: usize) -> Option<Self> {
            (start as usize)
                .checked_sub(count)
                .and_then(Self::from_number)
        }
    }
}

impl Architecture for X86_64Nasm {
    fn index_from_register(name: &str) -> Option<index::Register> {
        use x86_64_nasm::Register::*;
        let e = match name {
            "rsp" => Rsp,
            "rbp" => Rbp,
            "rax" => Rax,
            "rbx" => Rbx,
            "rcx" => Rcx,
            "rdx" => Rdx,
            "rsi" => Rsi,
            "rdi" => Rdi,
            "r9" => R9,
            "r10" => R10,
            "r11" => R11,
            "r12" => R12,
            "r13" => R13,
            "r14" => R14,
            "r15" => R15,
            _ => return None,
        };
        Some(e.as_index())
    }
    fn capabilities_set() -> RegisterCapabilitiesSet {
        use x86_64_nasm::Register;
        RegisterCapabilitiesSet::new(Register::Rax.as_index()..Register::R15.as_index())
            .with_stack_pointer(Register::Rsp.as_index())
            .with_frame_pointer(Register::Rbp.as_index())
    }
}
