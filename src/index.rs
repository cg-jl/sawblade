//! Thin `usize` wrappers that have their explicit meaning.
//! They're always convertible back to a `usize` but they
//! require using `unsafe` so that using them as bare indices to anything
//! becomes a burde.

use crate::arch;

// We keep registers as indices into
// a register table (to be free in terms of
// defining architectures)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct Register(usize);

impl Register {
    pub fn from<A: arch::Architecture>(name: &str) -> Option<Self> {
        A::index_from_register(name)
    }
    #[inline]
    pub unsafe fn from_index(index: usize) -> Self {
        Self(index)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Label(usize);
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Binding(usize);

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
    pub const unsafe fn from_index(index: usize) -> Self {
        Self(index)
    }
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct bindings have distinct indices.
    pub const unsafe fn to_index(self) -> usize {
        self.0
    }
}

impl Label {
    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct labels have distinct indices.
    #[inline]
    pub const unsafe fn from_index(index: usize) -> Self {
        Self(index)
    }

    /// # Safety
    /// The index must have been assigned by an entity which
    /// keeps two distinct labels have distinct indices.
    #[inline]
    pub const unsafe fn to_index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Id {
    Binding(Binding),
    Label(Label),
}
