use bitflags::bitflags;

use crate::index;
use crate::optir::Op;
use crate::PackedSlice;

// TODO: move x86_64_nasm to its own implementation module
// TODO: use &mut Vec<u8> instead of &mut Write in assemble() so I can build my own ELF sections

use self::x86_64_nasm::Register;

pub trait Architecture {
    fn index_from_register(name: &str) -> Option<index::Register>;
    fn register_set() -> RegisterSet;
    fn assemble<W: std::io::Write>(
        ops: PackedSlice<Op>,
        registers: PackedSlice<index::Register>,
        writer: &mut W,
    ) -> std::io::Result<()>;
}

// TODO: callee/caller reserved registers

pub struct RegisterSet {
    /// Registers that are used without any speacial meaning.
    /// They're called "general purpose" (abbreviated to gp)
    /// registers.
    pub gp_registers: index::RegisterRange,
    /// The stack pointer. Points to the "top" of the memory stack,
    /// that is, the minimum memory address valid for the current routine's
    /// frame (which is the top of the running stack)
    pub stack_pointer: Option<index::Register>,
    /// The frame pointer is similar to the stack pointer, except it points
    /// to the "next" frame from the top of the stack, or the bottom of the current
    /// routine's frame. It's useful to generate stacktraces, but if specified we
    /// can also use it to need a smaller offset for an address (smaller in absolute value).
    /// At least the frame pointer or the stack pointer is needed to enable static memory
    /// allocation, since the place of one can be inferred from the other and the frame size
    pub frame_pointer: Option<index::Register>,
    /// CPU status flags that are written in some circumstances.
    /// They are currently four: **N**egative, **C**arry, O**V**erflow, **Z**ero.
    /// They are used by the "flag allocator", which looks at the operation that
    /// defines a binding, and if it knows e.g that the binding is just used to branch,
    /// it will reserve a "it's in a branching flag" state for the binding, so that in
    /// practice we won't need more compare instrucions to branch.
    pub status_flags: FlagSet,
    /// The flags register contains the bits for the CPU status flags. It is currently
    /// not considered for storing values, but if present it will enable
    /// storing certain checks to be reused by further conditional set/branch
    /// instructions.
    ///
    /// Note that not having this register specified doesn't disable the flag-based
    /// allocator, which is toggled by the [`status_flags`](RegisterSet::status_flags)
    /// property. It does remove the possibility of overwriting the flags for even cheaper
    /// conditional sets.
    pub flags_register: Option<index::Register>,
}

bitflags! {
    pub struct FlagSet: u8 {
        const NEGATIVE = 0b0001;
        const CARRY = 0b0010;
        const OVERFLOW = 0b0100;
        const ZERO = 0b1000;
    }
}

impl RegisterSet {
    pub const fn new(gp_registers: index::RegisterRange) -> Self {
        Self {
            gp_registers,
            stack_pointer: None,
            frame_pointer: None,
            flags_register: None,
            status_flags: FlagSet::empty(),
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
    pub fn with_status_flags(mut self, status_flags: FlagSet) -> Self {
        self.status_flags |= status_flags;
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
        pub const COUNT: u8 = 15;
        pub const fn from_number(num: u8) -> Option<Self> {
            if num > Self::COUNT {
                None
            } else {
                Some(unsafe { std::mem::transmute(num as u8) })
            }
        }
        pub const fn as_index(self) -> index::Register {
            unsafe { index::Register::from_index(self as u8) }
        }
        pub const fn name(&self) -> &str {
            match self {
                Register::Rsp => "rsp",
                Register::Rbp => "rbp",
                Register::Rax => "rax",
                Register::Rbx => "rbx",
                Register::Rcx => "rcx",
                Register::Rdx => "rdx",
                Register::Rsi => "rsi",
                Register::Rdi => "rdi",
                Register::R9 => "r9",
                Register::R10 => "r10",
                Register::R11 => "r11",
                Register::R12 => "r12",
                Register::R13 => "r13",
                Register::R14 => "r14",
                Register::R15 => "r15",
            }
        }
    }

    impl std::fmt::Debug for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.name())
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
    fn register_set() -> RegisterSet {
        RegisterSet::new(index::RegisterRange {
            start: Register::Rax as u8,
            end: Register::R15 as u8,
        })
        .with_stack_pointer(Register::Rsp.as_index())
        .with_frame_pointer(Register::Rbp.as_index())
        .with_status_flags(FlagSet::all())
    }
    fn assemble<W: std::io::Write>(
        _ops: PackedSlice<Op>,
        registers: PackedSlice<index::Register>,
        _writer: &mut W,
    ) -> std::io::Result<()> {
        // first, let's convert each register index to an actual register of ours
        let mapped_registers = unsafe {
            let mut uninit_registers = Box::new_uninit_slice(registers.elements.len());
            for (target, source) in uninit_registers
                .iter_mut()
                .zip(registers.elements.iter().copied())
            {
                target.write(
                    Register::from_number(source.as_index())
                        .expect("register index out of assembly range"),
                );
            }
            uninit_registers.assume_init()
        };

        for index in 0..registers.ranges.len() {
            eprintln!(
                "registers for block {}:\n{:?}",
                index,
                &mapped_registers[registers.ranges[index].clone()]
            );
        }

        Ok(())
    }
}
