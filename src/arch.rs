use bitflags::bitflags;

use crate::index;
use crate::llir::Input;

// TODO: move x86_64_nasm to its own implementation module
// TODO: use &mut Vec<u8> instead of &mut Write in assemble() so I can build my own ELF sections

use self::x86_64_nasm::AssemblyOp;
use self::x86_64_nasm::DataSource;
use self::x86_64_nasm::Register;

pub trait Architecture {
    fn index_from_register(name: &str) -> Option<index::Register>;
    fn register_set() -> RegisterSet;
    fn assemble<'label, W: std::io::Write>(
        ir: crate::llir::IR,
        label_map: &[&'label str],
        output: &mut W,
    ) -> std::io::Result<()>;
}

// TODO: callee/caller reserved registers
#[derive(Clone, Copy)]
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
    #[derive(PartialEq, PartialOrd, Clone, Copy, Hash, Eq)]
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

    crate::impl_self_dependency_eq!(Register);

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

        pub fn expect_from_number(num: u8) -> Self {
            Self::from_number(num).unwrap_or_else(move || {
                panic!(
                    "virtual regitser index out of range for x86_64 architecture: {}",
                    num
                )
            })
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

    #[derive(Debug)]
    pub enum DataSource<'a> {
        Constant(u64),
        Register(Register),
        Label(&'a str),
    }
    // TODO: support memory addressing
    // TODO: support calls through registers
    #[derive(Debug)]
    pub enum AssemblyOp<'a> {
        Mov {
            dest: Register,
            source: DataSource<'a>,
        },
        Add {
            lhs: Register,
            rhs: DataSource<'a>,
        },
        Sub {
            lhs: Register,
            rhs: DataSource<'a>,
        },
        // XXX: I can't use offsets for x86_64 until I can control how many bytes is each
        // instruction (sized constants might also play here). That's what you get from variable
        // length instructions.
        Call {
            label: &'a str,
        },
        Ret,
    }

    impl std::fmt::Display for DataSource<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                DataSource::Constant(c) => c.fmt(f),
                DataSource::Register(r) => f.write_str(r.name()),
                DataSource::Label(l) => f.write_str(l),
            }
        }
    }

    impl std::fmt::Display for AssemblyOp<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Mov { dest, source } => write!(f, "mov {}, {}", dest.name(), source),
                Self::Add { lhs, rhs } => write!(f, "add {}, {}", lhs.name(), rhs),
                Self::Call { label } => write!(f, "call {}", label),
                Self::Ret => f.write_str("ret"),
                AssemblyOp::Sub { lhs, rhs } => write!(f, "add {}, {}", lhs.name(), rhs),
            }
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
    fn assemble<'label, W: std::io::Write>(
        mut ir: crate::llir::IR,
        exported_labels: &[&'label str],
        output: &mut W,
    ) -> std::io::Result<()> {
        let constant_to_ds = |c: &crate::optir::Constant| match c {
            crate::optir::Constant::Numeric(n) => DataSource::Constant(*n),
            crate::optir::Constant::Label(l) => {
                DataSource::Label(exported_labels[unsafe { l.to_index() } as usize])
            }
        };
        let input_to_ds = |inp: &crate::llir::Input| match inp {
            crate::llir::Input::Constant(c) => constant_to_ds(c),
            crate::llir::Input::Register(r) => {
                DataSource::Register(Register::expect_from_number(*r))
            }
        };

        let mut assembly = Vec::with_capacity(ir.ops.len());
        let mut last_start = 0;
        let mut next_offset = 0;
        let mut dummy_offset = ir.ops.len() as u16; // dummy offset so we can make another iteration.
        for start in ir.label_offsets.iter_mut().chain(Some(&mut dummy_offset)) {
            let last_offset = core::mem::replace(&mut next_offset, 0);
            for op in &ir.ops[last_start as usize..*start as usize] {
                let pushed_op = match op {
                    crate::llir::Op::CopyRegister { target, source } => AssemblyOp::Mov {
                        dest: Register::expect_from_number(*target),
                        source: DataSource::Register(Register::expect_from_number(*source)),
                    },
                    crate::llir::Op::SetValue { target, value } => AssemblyOp::Mov {
                        dest: Register::expect_from_number(*target),
                        source: constant_to_ds(value),
                    },
                    crate::llir::Op::USub { target, lhs, rhs } => {
                        let (lhs, rhs) = if target == lhs {
                            (Register::expect_from_number(*lhs), input_to_ds(rhs))
                        } else {
                            if let Input::Register(r) = rhs {
                                if r == target {
                                    (
                                        Register::expect_from_number(*r),
                                        DataSource::Register(Register::expect_from_number(*lhs)),
                                    )
                                } else {
                                    let target = Register::expect_from_number(*target);
                                    assembly.push(AssemblyOp::Mov {
                                        dest: target,
                                        source: DataSource::Register(Register::expect_from_number(
                                            *r,
                                        )),
                                    });
                                    next_offset += 1;

                                    (
                                        target,
                                        DataSource::Register(Register::expect_from_number(*lhs)),
                                    )
                                }
                            } else {
                                let target = Register::expect_from_number(*target);
                                writeln!(
                                    output,
                                    "\t{}",
                                    AssemblyOp::Mov {
                                        dest: target,
                                        source: input_to_ds(rhs)
                                    }
                                )?;

                                (
                                    target,
                                    DataSource::Register(Register::expect_from_number(*lhs)),
                                )
                            }
                        };

                        AssemblyOp::Sub { lhs, rhs }
                    }
                    crate::llir::Op::UAdd { target, lhs, rhs } => {
                        let (lhs, rhs) = if target == lhs {
                            (Register::expect_from_number(*lhs), input_to_ds(rhs))
                        } else {
                            if let Input::Register(r) = rhs {
                                if r == target {
                                    (
                                        Register::expect_from_number(*r),
                                        DataSource::Register(Register::expect_from_number(*lhs)),
                                    )
                                } else {
                                    let target = Register::expect_from_number(*target);
                                    assembly.push(AssemblyOp::Mov {
                                        dest: target,
                                        source: DataSource::Register(Register::expect_from_number(
                                            *r,
                                        )),
                                    });
                                    next_offset += 1;

                                    (
                                        target,
                                        DataSource::Register(Register::expect_from_number(*lhs)),
                                    )
                                }
                            } else {
                                let target = Register::expect_from_number(*target);
                                writeln!(
                                    output,
                                    "\t{}",
                                    AssemblyOp::Mov {
                                        dest: target,
                                        source: input_to_ds(rhs)
                                    }
                                )?;

                                (
                                    target,
                                    DataSource::Register(Register::expect_from_number(*lhs)),
                                )
                            }
                        };

                        AssemblyOp::Add { lhs, rhs }
                    }
                    crate::llir::Op::Call { label } => AssemblyOp::Call {
                        label: &ir.label_names[unsafe { label.to_index() } as usize],
                    },
                    crate::llir::Op::Ret => AssemblyOp::Ret,
                };

                assembly.push(pushed_op);
            }
            *start += last_offset;
            last_start = *start;
        }

        output.write(b".text\n.intel_syntax noprefix\n")?;

        for label in exported_labels {
            writeln!(output, ".global {}", label)?;
        }

        let mut last_start = 0;
        for (label, start) in ir
            .label_names
            .iter()
            .zip(crate::BoxIntoIter::new(ir.label_offsets))
        {
            for op in &assembly[last_start as usize..start as usize] {
                writeln!(output, "\t{}", op)?;
            }
            writeln!(output, "{}:", label)?;
            last_start = start;
        }

        for op in &assembly[last_start as usize..] {
            writeln!(output, "\t{}", op)?;
        }

        Ok(())
    }
}
