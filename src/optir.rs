//! OPTimizer Intermediate Representation.
//!
//! This representation is meant to be easily
//! modifiable by an optimizer, which means that
//! the operations themselves are separated from
//! their dependencies, so that the optimizer passes
//! can have a global insight from what dependencies they've
//! met in terms of constants and memory usage.
//!
//! It is also meant to favor further analysis from allocators.
//! In particular, the `bucket::Definition`s that we're using here
//! are just a member short of the bindings that an allocator will
//! use, which has a global insight on all the bindings used by all
//! the blocks and how those bindings are used, e.g when passing bindings
//! between blocks using calls (explicit) or by using phi nodes (implicit).
//!
//! After optimization & register allocation passes have gone
//! through, this IR is moved to codegen where it is transformed
//! into architecture-specific representation (i.e assembly), with
//! label linkage information which is kept from HLIR.

use crate::hlir::Condition;

// NOTE: should I look into "data flow graphs"? Since phi nodes
// here are pretty much not easy to analyze, maybe I need some sort
// of graph that connects the blocks directly in terms of the data
// they share between them. The `exported_bindings` property seems
// like a step in the right direction to create such a graph, if it
// helps to build a correct and optimized allocator.
use super::index;
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

pub type FixedArray<T> = Box<[T]>;

// NOTE: most of the `Vec`s here can be converted to arrays that are
// allocated and deallocated exactly once. All the pushing was made
// in HLIR. All the sizes are constant now.

// TODO: group blocks in CFGs by usage?

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

    #[derive(Debug, Clone, Copy, Ord, PartialEq, Eq)]
    pub enum UsageIndex {
        Op(u16),
        BlockEnd,
    }

    impl PartialOrd for UsageIndex {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            match (self, other) {
                (Self::BlockEnd, Self::BlockEnd) => Some(std::cmp::Ordering::Equal),
                (Self::BlockEnd, _) => Some(std::cmp::Ordering::Greater),
                (_, Self::BlockEnd) => Some(std::cmp::Ordering::Less),
                (Self::Op(a), Self::Op(b)) => a.partial_cmp(&b),
            }
        }
    }

    impl PartialEq<u16> for UsageIndex {
        fn eq(&self, other: &u16) -> bool {
            match self {
                Self::Op(i) => i == other,
                Self::BlockEnd => false,
            }
        }
    }

    impl PartialOrd<u16> for UsageIndex {
        fn partial_cmp(&self, other: &u16) -> Option<std::cmp::Ordering> {
            self.as_index().and_then(|i| i.partial_cmp(other))
        }
    }

    impl UsageIndex {
        pub const fn as_index(self) -> Option<u16> {
            match self {
                UsageIndex::Op(index) => Some(index),
                UsageIndex::BlockEnd => None,
            }
        }
    }

    /// Describes how and/or where a binding is used.
    #[derive(Debug, Clone, Copy)]
    pub struct Usage {
        pub usage_kind: UsageKind,
        pub index: UsageIndex,
    }

    /// Describes how and/or where a binding is defined
    #[derive(Debug, Clone, Copy)]
    pub enum Definition {
        Argument(u16),
        Op(u16),
    }

    impl Definition {
        pub const fn index(self) -> u16 {
            match self {
                Self::Argument(i) | Self::Op(i) => i,
            }
        }
    }
}

/// Out of a call, we might not be interested
/// in all the values but just define some of them.
/// Here's how we keep this information handy for the
/// allocators:
#[derive(Debug)]
pub struct CallReturnUsage {
    /// A list of the results that we're interested in,
    /// out of all the results that the call may spit out
    pub result_usage: FixedArray<u8>,
    /// The range of indices for the bindings that use the results.
    /// There are as many result bindings as `used_indices.len()`
    pub result_binding_range: BindingRange,
    /// The label that is called
    pub called_label: index::Label,
}

#[derive(Debug)]
pub struct Block {
    /// Argument count that the block accepts.
    /// Used for ease of access into buckets, since
    /// the first `..arg_count` buckets *will* be the argument
    /// bindings.
    pub arg_count: usize,
    /// Definition information where each binding is an index into
    /// the Vec.
    pub binding_defs: FixedArray<bucket::Definition>,
    /// Usage information where each binding is an index into the outer
    /// Vec. Each binding might have multiple usages, which represent
    /// the need for the physical place that holds the binding to keep
    /// holding it.
    pub binding_usages: FixedArray<FixedArray<bucket::Usage>>,
    /// List of call return usage information, handy for the allocators.
    pub call_return_usages: FixedArray<CallReturnUsage>,
    /// List of operations that describe *what* is the work being done.
    ///
    /// This is only used by the folding/optimization passes, because
    /// allocators don't really care about what is being done, but rather
    /// how the data flows between control breaks.
    ///
    /// Each `Op` does not have a separate copy of the bindings they declare
    /// because that information is already available in the binding buckets.
    pub operations: FixedArray<Op>,
    /// When a block leaves execution, it may yield some data with it. The bindings that are
    /// yielded are stored here.
    pub exported_bindings: FixedArray<index::Binding>,
    pub end: CFTransfer,
}

/// The set of bindings that are used as arguments to the next block,
/// Given that branches are like calls but without having to prepare things for return.
pub type ExportedBindings = FixedArray<index::Binding>;

/// Description of how control ends for this block (i.e is transferred
/// to other block). HLIR blocks with a `BlockIsEmpty` end will be marked
/// as empty blocks and inline wherever they're used to a no-op. They
/// don't return anything so a checker pass will catch anything that is
/// bound to them.
#[derive(Debug, Clone, Copy)]
pub enum CFTransfer {
    /// Return a set of values back to the caller.
    /// It's like a direct branch, except the target label
    /// is dynamic.
    Return,
    /// a jump. It just jumps into a label. Nothing fancy here.
    DirectBranch { target: index::Label },
    /// A branch that depends directly on a set of flags, set by a binding.
    /// The exported array from the block is shared between the true and false branches, where
    /// ..true_branch_binding_count is the range of the true branch and the rest is for the false
    /// branch.
    ConditionalBranch {
        stored_condition: Condition,
        flag_definition: index::Binding,
        target_if_true: index::Label,
        target_if_false: index::Label,
        true_branch_binding_count: u8,
    },
}

/// Indicates that control flow is being passed with data
// TODO: think of a better way to store redirections so they're more accessible from ForwardEdge's.
// We need to store the conditionally passed data somewhere.
#[derive(Debug, Clone, Copy)]
pub struct Redirection {
    pub target: index::Label,
    /// Each redirection
    pub exported_count: u8,
}

#[derive(Debug)]
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
#[derive(Debug)]
pub enum Op {
    Constant(Constant),
    Call {
        label: index::Label,
        args: FixedArray<index::Binding>,
        /// Reference to its usage information so it can be used
        /// by the assembler and the optimizer (in order to remove
        /// the information safely)
        usage_info_index: usize,
    },
    // Currently `add` is the only opcode I support.
    // TODO: more opcodes
    Add {
        lhs: index::Binding,
        rhs: index::Binding,
    },
    /// Subtract `rhs` from `lhs`.
    Sub {
        lhs: index::Binding,
        rhs: index::Binding,
    },
    FetchFlags(Condition),
}

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    Numeric(u64),
    Label(index::Label),
}

impl TryFrom<crate::hlir::Pure> for Constant {
    type Error = index::Binding;

    /// Tries converting from an HLIR Pure value. If it's a binding,
    /// returns Err with the binding.
    fn try_from(value: crate::hlir::Pure) -> Result<Self, Self::Error> {
        match value {
            crate::hlir::Pure::Binding(binding) => Err(binding),
            crate::hlir::Pure::Label(label) => Ok(Self::Label(label)),
            crate::hlir::Pure::Constant(constant) => Ok(Self::Numeric(constant)),
        }
    }
}

/// Forward-going can be one of three kinds:
#[derive(Debug, Clone, Copy)]
pub enum ForwardEdge {
    /// This block does not jump to a statically known
    /// block, rather it uses a call/return mechanism to
    /// transfer its control block
    Dynamic,
    /// One direct jump to a specific label
    Direct(index::Label),
    /// A selected branch through a condition
    Conditional {
        target_if_true: index::Label,
        target_if_false: index::Label,
    }, // NOTE: the label_if_true/false info is duplicated in Value as well for now, unless accessing
       // the map results in better performance.
}

#[derive(Debug)]
pub struct IR {
    pub blocks: Vec<Block>,
    /// Branching map that goes parent->child direction.
    pub forwards_branching_map: HashMap<index::Label, ForwardEdge>,
    /// Branching map that goes child->parent direction.
    pub backwards_branching_map: HashMap<index::Label, FixedArray<index::Label>>,
    pub return_blocks: Box<[Vec<index::Label>]>,
    pub return_counts: Box<[u8]>,
}

struct BlockBuilder {
    hlir_results: HashMap<index::Binding, index::Binding>,
    flag_definitions: HashMap<index::Binding, Condition>,
    ops: Vec<Op>,
    arg_count: usize,
    binding_definitions: Vec<bucket::Definition>,
    binding_usages: Vec<Vec<bucket::Usage>>,
    call_return_usages: Vec<CallReturnUsage>,
}

#[derive(Debug, Clone)]
pub struct BindingRange(Range<u16>);

impl IntoIterator for BindingRange {
    type Item = index::Binding;

    type IntoIter = BindingRangeIterator;

    fn into_iter(self) -> Self::IntoIter {
        BindingRangeIterator(self.0)
    }
}

unsafe impl std::iter::TrustedLen for BindingRangeIterator {}
impl std::iter::ExactSizeIterator for BindingRangeIterator {
    fn len(&self) -> usize {
        self.0.len()
    }
}

pub struct BindingRangeIterator(Range<u16>);

impl Iterator for BindingRangeIterator {
    type Item = index::Binding;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|x| unsafe { index::Binding::from_index(x) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl BindingRange {
    pub const fn single(binding: index::Binding) -> Self {
        // SAFE: we're going to return it as a binding later
        let index = unsafe { binding.to_index() };
        Self(index..index + 1)
    }
}

enum AssignedUsage {
    All,
    Specific(Vec<crate::hlir::AssignedBinding>),
}

impl BlockBuilder {
    /// Register an HLIR binding to be the same as a produced binding by us.
    /// # Safety
    /// Unsafe because the compiler can't guarantee that `hlir_target` and `self_target` come from
    /// the correct place.
    unsafe fn register_result(&mut self, hlir_target: index::Binding, self_target: index::Binding) {
        self.hlir_results.insert(hlir_target, self_target);
    }

    #[inline]
    fn binding_count(&self) -> u16 {
        self.binding_definitions.len() as u16
    }

    #[inline]
    fn get_registered_alias(&self, hlir_target: index::Binding) -> Option<index::Binding> {
        self.hlir_results.get(&hlir_target).copied()
    }

    /// Converts an HLIR definition into an OPTIR definition.
    fn compile_pure(&mut self, hlir_value: crate::hlir::Pure) -> Option<index::Binding> {
        match Constant::try_from(hlir_value) {
            // If it's a constant, then we'll have to define a new op
            Ok(constant) => Some(self.define(Op::Constant(constant))),
            // We'll grab the already defined result
            Err(alias) => self.get_registered_alias(alias),
        }
    }

    /// # Safety
    /// The operation must be added to make this usage index valid.
    unsafe fn usage_for_next_op(&self, usage_kind: bucket::UsageKind) -> bucket::Usage {
        bucket::Usage {
            usage_kind,
            index: bucket::UsageIndex::Op(self.ops.len() as u16),
        }
    }

    /// # Safety
    /// The given definition must be a valid index into the ops vec
    unsafe fn new_binding(&mut self, definition: bucket::Definition) -> index::Binding {
        let index = self.binding_definitions.len() as u16;
        self.binding_definitions.push(definition);
        self.binding_usages.push(Vec::new());
        unsafe { index::Binding::from_index(index) }
    }

    /// Pushes an operation, without it having to be aliased
    fn push_op(&mut self, op: Op) {
        self.ops.push(op);
    }

    /// Create a new binding and define it. Also
    /// adds an empty usage bucket
    fn define(&mut self, op: Op) -> index::Binding {
        let definition = bucket::Definition::Op(self.ops.len() as u16);
        self.ops.push(op);
        // SAFE: op index is correct since we've pushed a new op
        unsafe { self.new_binding(definition) }
    }

    fn get_usage_bucket(&mut self, binding: index::Binding) -> &mut Vec<bucket::Usage> {
        &mut self.binding_usages[unsafe { binding.to_index() } as usize]
    }

    fn compile_add(
        &mut self,
        lhs: crate::hlir::Pure,
        rest: Vec<crate::hlir::Pure>,
    ) -> Option<index::Binding> {
        // 1. Convert current lhs to an OPTIR binding
        // 2. For each rhs in the arguments:
        rest.into_iter().fold(self.compile_pure(lhs), |lhs, rhs| {
            lhs.and_then(|lhs| {
                // 2.a Convert rhs to an OPTIR binding
                let rhs = self.compile_pure(rhs)?;
                // 2.b Register usage of current lhs  and rhs for this compute op
                let usage = unsafe { self.usage_for_next_op(bucket::UsageKind::Exclusive) };
                self.get_usage_bucket(lhs).push(usage);
                self.get_usage_bucket(rhs).push(usage);
                // 2.c Define next lhs to be the result of adding current lhs and rhs
                Some(self.define(Op::Add { lhs, rhs }))
            })
        })
    }

    fn compile_sub(
        &mut self,
        lhs: crate::hlir::Pure,
        rest: Vec<crate::hlir::Pure>,
    ) -> Option<index::Binding> {
        rest.into_iter()
            .try_fold(self.compile_pure(lhs)?, |lhs, rhs| {
                // Convert rhs to an OPTIR binding
                let rhs = self.compile_pure(rhs)?;
                // Regitser usage of current lhs and rhs for this compute op
                let usage = unsafe { self.usage_for_next_op(bucket::UsageKind::Exclusive) };
                self.get_usage_bucket(lhs).push(usage);
                self.get_usage_bucket(rhs).push(usage);
                Some(self.define(Op::Sub { lhs, rhs }))
            })
    }

    fn compile_copied(
        &mut self,
        pures: Vec<crate::hlir::Pure>,
    ) -> Option<FixedArray<index::Binding>> {
        pures
            .into_iter()
            .map(|pure| self.compile_pure(pure))
            .collect::<Option<Vec<_>>>()
            .map(Vec::into_boxed_slice)
    }

    /// Compile a call operation.
    fn compile_call(
        &mut self,
        label: index::Label,
        params: Vec<crate::hlir::Pure>,
        assigned_usage: AssignedUsage,
        target_return_count: u8,
    ) -> Option<BindingRange> {
        // SAFE: we're pushing the operation later, when we finish assigning
        // all the usages
        let usage = unsafe { self.usage_for_next_op(bucket::UsageKind::Exclusive) };

        // 1. Transform all parameters into OPTIR bindings
        let params = params
            .into_iter()
            .map(|value| {
                let param = self.compile_pure(value)?;
                self.get_usage_bucket(param).push(usage);
                Some(param)
            })
            .collect::<Option<Vec<_>>>()
            .map(Vec::into_boxed_slice)?;

        let result_start_index = self.binding_count();
        let definition = bucket::Definition::Op(self.ops.len() as u16);

        // Collect the used indices, while registering the result
        // to their bindings.
        let result_usage = match assigned_usage {
            AssignedUsage::Specific(used_bindings) => {
                used_bindings
                    .into_iter()
                    .map(|assign| {
                        // SAFE:
                        // - the definition will be the call operation that we'll append later
                        // - `result` was computed using `new_binding` and `assign` comes
                        // from HLIR.
                        unsafe {
                            let result = self.new_binding(definition);
                            self.register_result(assign.binding, result);
                        }

                        assign.assign_index
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            }
            AssignedUsage::All => {
                for _ in 0..target_return_count {
                    unsafe {
                        self.new_binding(definition);
                    }
                }

                (0..target_return_count)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            }
        };

        let result_binding_range = BindingRange(result_start_index..self.binding_count());

        self.push_op(Op::Call {
            label,
            args: params,
            usage_info_index: self.call_return_usages.len(),
        });

        self.call_return_usages.push(CallReturnUsage {
            result_usage,
            result_binding_range: result_binding_range.clone(),
            called_label: label,
        });

        Some(result_binding_range)
    }

    fn release(mut self, end: CFTransfer, exported_bindings: FixedArray<index::Binding>) -> Block {
        // register usages for end
        exported_bindings.iter().copied().for_each(|binding| {
            self.get_usage_bucket(binding).push(bucket::Usage {
                usage_kind: bucket::UsageKind::Exclusive,
                index: bucket::UsageIndex::BlockEnd,
            })
        });
        Block {
            arg_count: self.arg_count,
            binding_defs: self.binding_definitions.into(),
            binding_usages: self
                .binding_usages
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            call_return_usages: self.call_return_usages.into(),
            operations: self.ops.into(),
            exported_bindings,
            end,
        }
    }
}

impl Block {
    fn from_hlir_block(hlir_block: super::hlir::Block, block_return_counts: &[u8]) -> Option<Self> {
        // 1. Create the definitions
        // NOTE: I'm only using `gets` for its length... Maybe storing those arguments knowing
        // they're the first ones... welp
        let arg_buckets = (0..hlir_block.gets.len() as u16).map(bucket::Definition::Argument);
        // SAFE: using the binding indices produced by HLIR is fine,
        // we're inserting them in **the same order**.

        let mut builder = BlockBuilder {
            hlir_results: (0..hlir_block.gets.len() as u32)
                .map(|x| {
                    // SAFE: argument bindings are marked first in `binding_definitions`
                    let binding_for_both = unsafe { index::Binding::from_index(x as u16) };
                    (binding_for_both, binding_for_both)
                })
                .collect(),
            ops: Vec::new(), // NOTE: we can have a pre-estimate about how many ops from a quick
            flag_definitions: HashMap::new(),
            // scan of the assignments
            arg_count: hlir_block.gets.len(),
            binding_definitions: arg_buckets.collect(),
            call_return_usages: Vec::new(),
            binding_usages: (0..hlir_block.gets.len()).map(|_| Vec::new()).collect(),
        };

        // compile down HLIR values into separate ops
        for assignment in hlir_block.assigns {
            use crate::hlir::Value;
            match assignment.value {
                Value::Copied(copied) => {
                    let results = builder.compile_copied(copied)?;
                    for (target, result) in assignment
                        .used_bindings
                        .into_iter()
                        .map(|x| x.binding)
                        .zip(results.iter().copied())
                    {
                        unsafe {
                            builder.register_result(target, result);
                        }
                    }
                }
                Value::Sub { lhs, rest } => {
                    // Since subtracting is a pure operation (can't access any
                    // other data), we'll skip compiling this if there is no
                    // target
                    if let Some(crate::hlir::AssignedBinding {
                        binding: target, ..
                    }) = assignment.used_bindings.into_iter().next()
                    {
                        let result = builder.compile_sub(lhs, rest)?;
                        unsafe {
                            builder.register_result(target, result);
                        }
                    }
                }
                Value::Add { lhs, rest } => {
                    // Since adding is a pure operation (can't access any
                    // other data), we'll skip compiling this if there is no
                    // target
                    if let Some(crate::hlir::AssignedBinding {
                        binding: target, ..
                    }) = assignment.used_bindings.into_iter().next()
                    {
                        let result = builder.compile_add(lhs, rest)?;
                        unsafe {
                            builder.register_result(target, result);
                        }
                    }
                }
                Value::Call { label, params } => {
                    builder.compile_call(
                        label,
                        params,
                        AssignedUsage::Specific(assignment.used_bindings),
                        block_return_counts[unsafe { label.to_index() } as usize],
                    )?;
                }
                Value::Flags {
                    instruction,
                    condition,
                } => {
                    // Same reason as add: obtaining the flags is just a pure read operation, if
                    // we're not doing anything with them we may as well not compile it.
                    if let Some(crate::hlir::AssignedBinding {
                        binding: target, ..
                    }) = assignment.used_bindings.into_iter().next()
                    {
                        let instruction = builder.get_registered_alias(instruction)?;
                        let op_index = builder.ops.len() as u16;
                        let binding = builder.define(Op::FetchFlags(condition));
                        builder.get_usage_bucket(instruction).push(bucket::Usage {
                            usage_kind: bucket::UsageKind::Exclusive,
                            index: bucket::UsageIndex::Op(op_index),
                        });
                        unsafe {
                            builder.register_result(target, binding);
                        }

                        builder.flag_definitions.insert(binding, condition);
                    }
                }
            }
        }

        let (end, exported_bindings) = match hlir_block.end {
            crate::hlir::End::TailValue(value) => (
                CFTransfer::Return,
                match value {
                    crate::hlir::Value::Copied(pures) => builder.compile_copied(pures)?,
                    crate::hlir::Value::Add { lhs, rest } => {
                        vec![builder.compile_add(lhs, rest)?].into_boxed_slice()
                    }
                    crate::hlir::Value::Sub { lhs, rest } => {
                        vec![builder.compile_sub(lhs, rest)?].into_boxed_slice()
                    }
                    crate::hlir::Value::Call { label, params } => builder
                        .compile_call(
                            label,
                            params,
                            AssignedUsage::All,
                            block_return_counts[unsafe { label.to_index() } as usize],
                        )?
                        .into_iter()
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    crate::hlir::Value::Flags { .. } => {
                        todo!("returning flags is not yet supported")
                    }
                },
            ),
            // TODO: force bindings on conditional branches?
            crate::hlir::End::ConditionalBranch {
                flag: flag_definition,
                if_true,
                if_false,
            } => {
                let flag_definition = builder.get_registered_alias(flag_definition)?;
                let stored_condition = builder.flag_definitions.get(&flag_definition).copied()?;
                let target_if_true = if_true.label;
                let target_if_false = if_false.label;
                let true_branch_binding_count = if_true.args.len() as u8;
                let exposed = if_true
                    .args
                    .into_iter()
                    .chain(if_false.args)
                    .map(|arg| builder.compile_pure(arg))
                    .try_collect::<Vec<_>>()?
                    .into_boxed_slice();
                builder
                    .get_usage_bucket(flag_definition)
                    .push(bucket::Usage {
                        usage_kind: bucket::UsageKind::Exclusive,
                        index: bucket::UsageIndex::BlockEnd,
                    });
                (
                    CFTransfer::ConditionalBranch {
                        stored_condition,
                        flag_definition,
                        target_if_true,
                        target_if_false,
                        true_branch_binding_count,
                    },
                    exposed,
                )
            }
        };

        Some(builder.release(end, exported_bindings))
    }
}

impl IR {
    pub fn return_blocks_in_dependency_order(&self) -> impl Iterator<Item = index::Label> {
        crate::DependencyOrderIterWithSet::new(
            self.return_blocks
                .iter()
                .enumerate()
                .map(|(index, returns)| {
                    (
                        unsafe { index::Label::from_index(index as u16) },
                        returns.iter().copied().collect::<HashSet<_>>(),
                    )
                })
                .collect(),
        )
    }

    fn from_blocks(
        blocks: Vec<Block>,
        return_blocks: Box<[Vec<index::Label>]>,
        return_counts: Box<[u8]>,
    ) -> Self {
        let mut backwards_branching_map: HashMap<_, HashSet<_>> =
            HashMap::with_capacity(blocks.len());
        let forwards_branching_map: HashMap<_, _> = blocks
            .iter()
            .enumerate()
            .map(|(index, block)| {
                let label = unsafe { index::Label::from_index(index as u16) };
                let edge = match block.end {
                    CFTransfer::Return { .. } => ForwardEdge::Dynamic,
                    CFTransfer::DirectBranch { target, .. } => {
                        backwards_branching_map
                            .entry(target)
                            .or_default()
                            .insert(label);
                        ForwardEdge::Direct(target)
                    }
                    CFTransfer::ConditionalBranch {
                        target_if_true,
                        target_if_false,
                        ..
                    } => {
                        backwards_branching_map
                            .entry(target_if_true)
                            .or_default()
                            .insert(label);
                        backwards_branching_map
                            .entry(target_if_false)
                            .or_default()
                            .insert(label);
                        ForwardEdge::Conditional {
                            target_if_true,
                            target_if_false,
                        }
                    }
                };
                (label, edge)
            })
            .collect();

        let backwards_branching_map = backwards_branching_map
            .into_iter()
            .map(|(label, set)| {
                (
                    label,
                    set.into_iter().collect::<Vec<_>>().into_boxed_slice(),
                )
            })
            .collect();

        Self {
            blocks,
            forwards_branching_map,
            backwards_branching_map,
            return_blocks,
            return_counts,
        }
    }
}

trait MoveLabel {
    /// "Moves" or renames a label.
    /// # Safety
    /// - the new label must not clash with existing labels.
    /// - the block referred to by the old label must not be in use
    /// by any other.
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label);
}

impl MoveLabel for index::Label {
    #[inline]
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label) {
        if self == &previous {
            *self = next;
        }
    }
}

impl MoveLabel for PhiSelector {
    #[inline]
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label) {
        unsafe {
            self.block_from.move_label(previous, next);
        }
    }
}

impl MoveLabel for Constant {
    #[inline]
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label) {
        match self {
            Constant::Numeric(_) => (),
            Constant::Label(label) => unsafe { label.move_label(previous, next) },
        }
    }
}

impl MoveLabel for Op {
    #[inline]
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label) {
        match self {
            Op::Constant(c) => unsafe { c.move_label(previous, next) },
            Op::Call {
                label,
                args: _,
                usage_info_index: _,
            } => unsafe { label.move_label(previous, next) },
            Op::Add { lhs: _, rhs: _ } => (),
            Op::FetchFlags(_) => (),
            Op::Sub { lhs: _, rhs: _ } => (),
        }
    }
}

impl MoveLabel for Block {
    #[inline]
    unsafe fn move_label(&mut self, previous: index::Label, next: index::Label) {
        match &mut self.end {
            CFTransfer::Return => (),
            CFTransfer::DirectBranch { target } => unsafe { target.move_label(previous, next) },
            CFTransfer::ConditionalBranch {
                target_if_true,
                target_if_false,
                ..
            } => unsafe {
                target_if_true.move_label(previous, next);
                target_if_false.move_label(previous, next);
            },
        }
        for op in self.operations.iter_mut() {
            unsafe {
                op.move_label(previous, next);
            }
        }
    }
}

pub fn dissect_from_hlir(blocks: Vec<crate::hlir::Block>) -> IR {
    use std::collections::BinaryHeap;
    let mut returning_blocks = vec![HashSet::new(); blocks.len()].into_boxed_slice();
    let (return_counts, malformed_branches) = compute_return_counts(&blocks, &mut returning_blocks);

    let mut compiled_blocks = blocks
        .into_iter()
        .filter_map(|block| Block::from_hlir_block(block, &return_counts))
        .collect::<Vec<_>>();

    // being a max-heap, we'll have max indices first
    let malformed_branches = BinaryHeap::from_iter(malformed_branches.into_iter());

    for remove_index in malformed_branches.into_iter_sorted() {
        // offset labels back one by one
        for move_index in remove_index..(compiled_blocks.len() as u16).saturating_sub(1) {
            // SAFE: we're using block indices, so these are true labels.
            let old_label = unsafe { index::Label::from_index(move_index + 1) };
            let new_label = unsafe { index::Label::from_index(move_index) };
            // SAFE:
            // - by doing the iteration is in this order, we ensure that labels aren't
            // duplicated
            // - all blocks that had a reference to this block appear in `malformed_branches`,
            // so they are removed as well.
            compiled_blocks
                .iter_mut()
                .for_each(|block| unsafe { block.move_label(old_label, new_label) });
        }

        // now we can safely remove the block
        compiled_blocks.remove(remove_index as usize);
    }

    IR::from_blocks(
        compiled_blocks,
        crate::BoxIntoIter::new(returning_blocks)
            .map(|set| set.into_iter().collect())
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        return_counts,
    )
}

/// Returns an array of the return amounts for each block, as well as the blocks that have
/// different return counts per branch.
// NOTE: zero counts might just be that they're a loop
fn compute_return_counts(
    blocks: &[crate::hlir::Block],
    returning_blocks: &mut [HashSet<index::Label>],
) -> (FixedArray<u8>, HashSet<u16>) {
    use crate::hlir::End;
    use std::collections::VecDeque;
    let mut slice = vec![0; blocks.len()];

    let mut solved = HashSet::new();
    let mut malformed_branches = HashSet::new();

    struct Task {
        index: u16,
        tries: u16,
    }

    impl Task {
        const fn new(index: u16) -> Self {
            Self { index, tries: 0 }
        }
    }

    // ensure we go through everyone before we repeat.
    let mut queue = VecDeque::from_iter((0..blocks.len() as u16).map(Task::new));

    while let Some(mut next) = queue.pop_front() {
        match &blocks[next.index as usize].end {
            End::TailValue(value) => {
                // since we're at a TailValue, this block returns from itself.
                returning_blocks[next.index as usize]
                    .insert(unsafe { index::Label::from_index(next.index) });
                match value {
                    crate::hlir::Value::Copied(pures) => {
                        slice[next.index as usize] = pures.len() as u8;
                        solved.insert(next.index);
                        continue;
                    }
                    crate::hlir::Value::Add { lhs: _, rest: _ }
                    | crate::hlir::Value::Sub { lhs: _, rest: _ } => {
                        slice[next.index as usize] = 1;
                        solved.insert(next.index);
                        continue;
                    }
                    crate::hlir::Value::Call { label, params: _ } => {
                        let target_index = unsafe { label.to_index() };
                        // NOTE: same TODO  from below applies here, but to a single call. This
                        // implies that the rest of operations after any call whose block yields
                        // never makes for the deletion of the *rest* of the block. And yes, this
                        // could be done in HLIR :). Doing this is safe because the situations for
                        // yielding never are exactly two: entering a block that yields never or
                        // completing an unconditional jump chain.
                        if malformed_branches.contains(&target_index) {
                            malformed_branches.insert(next.index);
                            continue;
                        }
                        // if our dependency was solved, then we can resolve this one to the same
                        if solved.contains(&target_index) {
                            slice[next.index as usize] = slice[target_index as usize];
                            solved.insert(next.index);
                            continue;
                        }
                    }
                    crate::hlir::Value::Flags { .. } => {
                        todo!("returning flags is not yet supported")
                    }
                }
            }
            End::ConditionalBranch {
                flag: _,
                if_true,
                if_false,
            } => {
                let true_index = unsafe { if_true.label.to_index() };
                let false_index = unsafe { if_false.label.to_index() };
                if malformed_branches.contains(&true_index)
                    || malformed_branches.contains(&false_index)
                {
                    malformed_branches.insert(next.index);
                    continue;
                }
                if solved.contains(&true_index) && solved.contains(&false_index) {
                    // block calls are malformed
                    // TODO: this is not particularly true, specially in the case that one of the
                    // blocks never returns. This behavior should be pre-detected and then this
                    // expression should select the amount of returns from the block that never
                    // returns. If both blocks return and they return different arguments, then it
                    // IS a malformed branch since it yields undefined values.
                    if slice[true_index as usize] != slice[false_index as usize] {
                        malformed_branches.insert(next.index);
                        continue;
                    }

                    // sadly due to the possibility of iterator invalidation during the extend()
                    // method and my absence of knowledge about the inner workings of
                    // HashSet::extend I have to clone these two.
                    // TODO: peek into the implementation of HashSet::extend and figure out how to
                    // avoid these two copies.
                    let true_set = returning_blocks[true_index as usize].clone();
                    let false_set = returning_blocks[false_index as usize].clone();

                    returning_blocks[next.index as usize]
                        .extend(false_set.into_iter().chain(true_set));
                    slice[next.index as usize] = slice[true_index as usize];
                    solved.insert(next.index);
                    continue;
                }
            }
        }
        if next.tries < 10 {
            next.tries += 1;
            // note: 10 tries means 10! call chain depth, which is very very unlikely.
            // I'll just won't continue this one and leave it as zero.
            queue.push_back(next);
        }
    }

    (slice.into_boxed_slice(), malformed_branches)
}

// NOTE: specs aren't used by optimizers, they're used by allocators.
// Therefore they are not defined here on purpose. There are plans
// to include a non-generic Spec that the allocators may use in their
// passes but the Architecture type will only be erased when invoking the
// allocators when their output is passed to the correct codegen for
// the selected Architecture.
