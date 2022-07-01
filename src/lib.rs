#![deny(unsafe_op_in_unsafe_fn)]
#![feature(is_some_with)]
pub mod arch;
pub mod ast;
pub mod hlir;
pub mod index;

/// OPTimizer Intermediate Representation.
///
/// This representation is meant to be easily
/// modifiable by an optimizer, which means that
/// the operations themselves are separated from
/// their dependencies, so that the optimizer passes
/// can have a global insight from what dependencies they've
/// met in terms of constants and memory usage.
///
/// It is also meant to favor further analysis from allocators.
/// In particular, the `bucket::Definition`s that we're using here
/// are just a member short of the bindings that an allocator will
/// use, which has a global insight on all the bindings used by all
/// the blocks and how those bindings are used, e.g when passing bindings
/// between blocks using calls (explicit) or by using phi nodes (implicit).
///
/// After optimization & register allocation passes have gone
/// through, this IR is moved to codegen where it is transformed
/// into architecture-specific representation (i.e assembly), with
/// label linkage information which is kept from HLIR.
pub mod optir {
    // NOTE: should I look into "data flow graphs"? Since phi nodes
    // here are pretty much not easy to analyze, maybe I need some sort
    // of graph that connects the blocks directly in terms of the data
    // they share between them. The `exported_bindings` property seems
    // like a step in the right direction to create such a graph, if it
    // helps to build a correct and optimized allocator.
    use super::index;
    use std::collections::HashMap;

    // NOTE: most of the `Vec`s here can be converted to arrays that are
    // allocated and deallocated exactly once. All the pushing was made
    // in HLIR. All the sizes are constant now.

    /// Binding storage related data.
    ///
    /// Bindings are stored into imaginary *buckets*, which
    /// represent... well, the place they are stored in *physically*.
    /// These physical places might be registers, memory slots, inside the CPU's flag register,
    /// certain bits of a register...
    /// A *bucket* may be shared by more than one binding if the results
    /// they represent are proven to not exist at the same time (e.g each
    /// binding was computed through exclusive branches).
    /// These *buckets* can be used by allocators to easily build an
    /// insight into aliasing between bindings.
    ///
    ///
    /// # Notes
    /// - Buckets themselves are not represented here since
    /// they are of no use, only the indices of the positions of
    /// and inside a bucket are appropiate.
    ///
    /// - All `usize`s and `u8`s here just represent
    /// a unique identifier, they don't have any special meaning for the optimizer
    /// or allocators, which don't really change those around. They are treated
    /// by indices just by the assembly pass that needs to track what *real*
    /// physical spaces contain those results.
    pub mod bucket {
        #[derive(Debug, Clone, Copy)]
        pub enum UsageKind {
            /// Requires a place that must **exclusively**
            /// hold that result in any situation that executes
            /// the statement where this usage applies.
            Exclusive,
            /// Requires a *bucket* where the place reserved to that
            /// bucket might be used by other results since selecting
            /// those means this binding doesn't exist, i.e used in selecting
            /// branch results through phi nodes.
            Selective { selection_bucket: u8 },
        }
        /// Describes how and where a binding is used.
        #[derive(Debug, Clone, Copy)]
        pub struct Usage {
            pub usage_kind: UsageKind,
            pub statement_index: usize,
        }

        /// Definition bucket
        #[derive(Debug, Clone, Copy)]
        pub struct Definition {
            /// The statement index in which this binding was defined
            pub statement_index: usize,
            /// A secondary index to pin down the assignment order
            pub statement_definition_index: usize,
        }
    }

    pub struct Block {
        /// Definition information where each binding is an index into
        /// the Vec.
        pub binding_defs: Vec<bucket::Definition>,
        /// Usage information where each binding is an index into the outer
        /// Vec. Each binding might have multiple usages, which represent
        /// the need for the physical place that holds the binding to keep
        /// holding it.
        pub binding_usages: Vec<Vec<bucket::Usage>>,
        /// List of operations that describe *what* is the work being done.
        ///
        /// This is only used by the folding/optimization passes, because
        /// allocators don't really care about what is being done, but rather
        /// how the data flows between control breaks.
        ///
        /// Each `Op` does not have a separate copy of the bindings they declare
        /// because that information is already available in the binding buckets.
        pub operations: Vec<Op>,
        pub end: CFTransfer,
    }

    /// A control end structure. Has a map of exported bindings so that
    /// the allocator may use this for directly mapping two bindings from
    /// distinct blocks to the same *bucket*.
    pub struct CFTransfer {
        /// A set of bindings that are used in other phi statements.
        /// In the source code phi nodes are identified by:
        /// ```abism
        /// %a = phi @from-1:[%hello] @from-2:[%world] ...
        /// ```
        /// Those are converted to exported bindings here, and they may have
        /// a (small) vec of blocks they are used in.
        pub exported_bindings: HashMap<index::Binding, Vec<index::Label>>,
        pub ty: CFTransferType,
    }

    /// Description of how control ends for this block (i.e is transferred
    /// to other block). HLIR blocks with a `BlockIsEmpty` end will be marked
    /// as empty blocks and inline wherever they're used to a no-op. They
    /// don't return anything so a checker pass will catch anything that is
    /// bound to them.
    pub enum CFTransferType {
        /// Return a set of values back to the caller.
        /// It's like a direct branch, except the target label
        /// is dynamic.
        Return,
        /// a jump. It just jumps into a label. Nothing fancy here.
        DirectBranch { target: index::Label },
        ConditionalBranch {
            condition_source: index::Binding,
            target_if_true: index::Label,
            target_if_false: index::Label,
        },
    }

    pub struct PhiSelector {
        /// (small) list of bindings that the phi
        /// descriptor might consume, depending on the
        /// block that it comes from.
        pub used_bindings: Vec<index::Binding>,
        /// The block that may be selected by this Phi node.
        pub block_from: index::Label,
    }

    /// An operation. Describes *what* the machine
    /// is going to do with the information we give it.
    ///
    /// There's no constants here, except the `Pure` operation that just means
    /// "assign a constant here". In order to remove the use of a binding,
    /// the optimizer will have to reduce all its uses to constants
    /// (maybe through partially inlined calls, if needed).
    ///
    /// Since a lot of assemblies support the use of constants in some
    /// instructions (to optimize for use and space), a map of what bindings
    /// are set to constants is handed to the Architecture's codegen
    /// implementation.
    pub enum Op {
        Pure(u64),
        Phi(Vec<PhiSelector>),
        Call {
            label: index::Label,
            args: Vec<index::Binding>,
            /// Out of the values that the block returns, what it is used
            /// from this call?
            unpacked_indices: Vec<usize>,
        },
        // Currently `add` is the only opcode I support.
        // TODO: more opcodes
        Add {
            lhs: index::Binding,
            rhs: index::Binding,
        },
    }

    // NOTE: specs aren't used by optimizers, they're used by allocators.
    // Therefore they are not defined here on purpose. There are plans
    // to include a non-generic Spec that the allocators may use in their
    // passes but the Architecture type will only be erased when invoking the
    // allocators when their output is passed to the correct codegen for
    // the selected Architecture.
}
