//! Thin `u32` wrappers that have their explicit meaning.
//! They're always convertible back to a `u32` but they
//! require using `unsafe` so that using them as bare indices to anything
//! becomes a burde.

use crate::arch;

// NOTE: might be able to lower binding indices to `u16`

// We keep registers as indices into
// a register table (to be free in terms of
// defining architectures)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Register(u8); // no way your machine has more that 256 fixed registers

impl Register {
    pub fn from<A: arch::Architecture>(name: &str) -> Option<Self> {
        A::index_from_register(name)
    }
    #[inline]
    pub const unsafe fn from_index(index: u8) -> Self {
        Self(index)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Label(u16); // no more than 65536 labels allowed
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Binding(u32);

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
    pub const unsafe fn from_index(index: u32) -> Self {
        Self(index)
    }
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct bindings have distinct indices.
    pub const unsafe fn to_index(self) -> u32 {
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
