//! Thin `u32` wrappers that have their explicit meaning.
//! They're always convertible back to a `u32` but they
//! require using `unsafe` so that using them as bare indices to anything
//! becomes a burde.

use crate::arch;

// NOTE: might be able to lower binding indices to `u16`

// We keep registers as indices into
// a register table (to be free in terms of
// defining architectures)
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Register(u8); // no way your machine has more that 256 fixed registers

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RegisterRange {
    pub start: u8,
    pub end: u8,
}

impl RegisterRange {
    pub const fn len(self) -> u8 {
        self.end.saturating_sub(self.start)
    }

    pub const unsafe fn get_unchecked(self, offset: u8) -> Register {
        Register(self.start + offset)
    }
}

impl Register {
    pub fn from<A: arch::Architecture>(name: &str) -> Option<Self> {
        A::index_from_register(name)
    }
    #[inline]
    pub const unsafe fn from_index(index: u8) -> Self {
        Self(index)
    }
    pub const unsafe fn as_index(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Label(u16); // no more than 65536 labels allowed
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Binding(u16);

impl std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("%")?;
        self.0.fmt(f)
    }
}

impl std::fmt::Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@")?;
        self.0.fmt(f)
    }
}

impl Binding {
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct bindings have distinct indices.
    #[inline]
    pub const unsafe fn from_index(index: u16) -> Self {
        Self(index)
    }
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct bindings have distinct indices.
    pub const unsafe fn to_index(self) -> u16 {
        self.0
    }
}

impl Label {
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct labels have distinct indices.
    #[inline]
    pub const unsafe fn from_index(index: u16) -> Self {
        Self(index)
    }

    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct labels have distinct indices.
    #[inline]
    pub const unsafe fn to_index(self) -> u16 {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Id {
    Binding(Binding),
    Label(Label),
}
