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

    #[derive(Debug, Clone, Copy)]
    pub enum UsageIndex {
        Op(usize),
        BlockEnd,
    }

    impl UsageIndex {
        pub const fn as_index(self) -> Option<usize> {
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
        Argument(usize),
        Op(usize),
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
    pub result_usage: FixedArray<usize>,
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
    pub end: CFTransfer,
}

/// A set of bindings that are used in other phi statements.
/// In the source code phi nodes are identified by:
/// ```abism
/// %a = phi @from-1:[%hello] @from-2:[%world] ...
/// ```
/// Those are converted to exported bindings here, and they may have
/// a (small) vec of blocks they are used in.
pub type ExportedBindings = HashMap<index::Label, FixedArray<index::Binding>>;

/// Description of how control ends for this block (i.e is transferred
/// to other block). HLIR blocks with a `BlockIsEmpty` end will be marked
/// as empty blocks and inline wherever they're used to a no-op. They
/// don't return anything so a checker pass will catch anything that is
/// bound to them.
#[derive(Debug, Clone)]
pub enum CFTransfer {
    /// Return a set of values back to the caller.
    /// It's like a direct branch, except the target label
    /// is dynamic.
    Return(FixedArray<index::Binding>),
    /// a jump. It just jumps into a label. Nothing fancy here.
    DirectBranch {
        target: index::Label,
        exported_bindings: ExportedBindings,
    },
    ConditionalBranch {
        exported_bindings: ExportedBindings,
        condition_source: index::Binding,
        target_if_true: index::Label,
        target_if_false: index::Label,
    },
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
    Phi(FixedArray<PhiSelector>),
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
}

struct BlockBuilder {
    hlir_results: HashMap<index::Binding, index::Binding>,
    ops: Vec<Op>,
    arg_count: usize,
    binding_definitions: Vec<bucket::Definition>,
    binding_usages: Vec<Vec<bucket::Usage>>,
    call_return_usages: Vec<CallReturnUsage>,
}

#[derive(Debug, Clone)]
pub struct BindingRange(Range<usize>);

impl BindingRange {
    pub const fn single(binding: index::Binding) -> Self {
        // SAFE: we're going to return it as a binding later
        let index = unsafe { binding.to_index() };
        Self(index..index + 1)
    }
}

impl Iterator for BindingRange {
    type Item = index::Binding;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|index| unsafe { index::Binding::from_index(index) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

unsafe impl std::iter::TrustedLen for BindingRange {}
impl std::iter::ExactSizeIterator for BindingRange {
    #[inline]
    fn len(&self) -> usize {
        self.0.len()
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
    fn binding_count(&self) -> usize {
        self.binding_definitions.len()
    }

    /// Converts an HLIR definition into an OPTIR definition.
    fn compile_pure(&mut self, hlir_value: crate::hlir::Pure) -> index::Binding {
        match Constant::try_from(hlir_value) {
            // If it's a constant, then we'll have to define a new op
            Ok(constant) => self.define(Op::Constant(constant)),
            // We'll grab the already defined result
            Err(alias) => self.hlir_results[&alias],
        }
    }

    /// # Safety
    /// The operation must be added to make this usage index valid.
    unsafe fn usage_for_next_op(&self, usage_kind: bucket::UsageKind) -> bucket::Usage {
        bucket::Usage {
            usage_kind,
            index: bucket::UsageIndex::Op(self.ops.len()),
        }
    }

    /// # Safety
    /// The given definition must be a valid index into the ops vec
    unsafe fn new_binding(&mut self, definition: bucket::Definition) -> index::Binding {
        let index = self.binding_definitions.len();
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
        let definition = bucket::Definition::Op(self.ops.len());
        self.ops.push(op);
        // SAFE: op index is correct since we've pushed a new op
        unsafe { self.new_binding(definition) }
    }

    fn get_usage_bucket(&mut self, binding: index::Binding) -> &mut Vec<bucket::Usage> {
        &mut self.binding_usages[unsafe { binding.to_index() }]
    }

    fn compile_add(
        &mut self,
        lhs: crate::hlir::Pure,
        rest: Vec<crate::hlir::Pure>,
    ) -> index::Binding {
        // 1. Convert current lhs to an OPTIR binding
        // 2. For each rhs in the arguments:
        rest.into_iter().fold(self.compile_pure(lhs), |lhs, rhs| {
            // 2.a Convert rhs to an OPTIR binding
            let rhs = self.compile_pure(rhs);
            // 2.b Register usage of current lhs  and rhs for this compute op
            let usage = unsafe { self.usage_for_next_op(bucket::UsageKind::Exclusive) };
            self.get_usage_bucket(lhs).push(usage);
            self.get_usage_bucket(rhs).push(usage);
            // 2.c Define next lhs to be the result of adding current lhs and rhs
            self.define(Op::Add { lhs, rhs })
        })
    }

    fn compile_copied(&mut self, pures: Vec<crate::hlir::Pure>) -> FixedArray<index::Binding> {
        pures
            .into_iter()
            .map(|pure| self.compile_pure(pure))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    /// Compile a call operation.
    /// If `specific_usage` is `None`, then
    fn compile_call(
        &mut self,
        label: index::Label,
        params: Vec<crate::hlir::Pure>,
        assigned_usage: AssignedUsage,
        target_return_count: usize,
    ) -> BindingRange {
        // SAFE: we're pushing the operation later, when we finish assigning
        // all the usages
        let usage = unsafe { self.usage_for_next_op(bucket::UsageKind::Exclusive) };

        // 1. Transform all parameters into OPTIR bindings
        let params = params
            .into_iter()
            .map(|value| {
                let param = self.compile_pure(value);
                self.get_usage_bucket(param).push(usage);
                param
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let result_start_index = self.binding_count();
        let definition =
            bucket::Definition::Op(unsafe { usage.index.as_index().unwrap_unchecked() });

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

        result_binding_range
    }

    fn release(self, end: CFTransfer) -> Block {
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
            end,
        }
    }
}

impl Block {
    fn from_hlir_block(hlir_block: super::hlir::Block, block_return_counts: &[usize]) -> Self {
        // 1. Create the definitions
        // NOTE: I'm only using `gets` for its length... Maybe storing those arguments knowing
        // they're the first ones... welp
        let arg_buckets = (0..hlir_block.gets.len()).map(bucket::Definition::Argument);
        // SAFE: using the binding indices produced by HLIR is fine,
        // we're inserting them in **the same order**.

        let mut builder = BlockBuilder {
            hlir_results: (0..hlir_block.gets.len())
                .map(|x| {
                    // SAFE: argument bindings are marked first in `binding_definitions`
                    let binding_for_both = unsafe { index::Binding::from_index(x) };
                    (binding_for_both, binding_for_both)
                })
                .collect(),
            ops: Vec::new(), // NOTE: we can have a pre-estimate about how many ops from a quick
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
                    let results = builder.compile_copied(copied);
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
                Value::Add { lhs, rest } => {
                    // Since adding is a pure operation (can't access any
                    // other data), we'll skip compiling this if there is no
                    // target
                    if let Some(crate::hlir::AssignedBinding {
                        binding: target, ..
                    }) = assignment.used_bindings.into_iter().next()
                    {
                        let result = builder.compile_add(lhs, rest);
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
                        block_return_counts[unsafe { label.to_index() }],
                    );
                }
            }
        }

        let end = match hlir_block.end {
            crate::hlir::End::TailValue(value) => CFTransfer::Return(match value {
                crate::hlir::Value::Copied(pures) => builder.compile_copied(pures),
                crate::hlir::Value::Add { lhs, rest } => {
                    vec![builder.compile_add(lhs, rest)].into_boxed_slice()
                }
                crate::hlir::Value::Call { label, params } => builder
                    .compile_call(
                        label,
                        params,
                        AssignedUsage::All,
                        block_return_counts[unsafe { label.to_index() }],
                    )
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }),
            crate::hlir::End::ConditionalBranch {
                condition,
                label_if_true,
                label_if_false,
            } => {
                let condition = builder.compile_pure(condition);
                CFTransfer::ConditionalBranch {
                    // TODO: grab exported bindings from conditionals
                    exported_bindings: HashMap::new(),
                    condition_source: condition,
                    target_if_true: label_if_true,
                    target_if_false: label_if_false,
                }
            }
        };

        builder.release(end)
    }
}

impl IR {
    fn from_blocks(blocks: Vec<Block>) -> Self {
        let mut backwards_branching_map: HashMap<_, HashSet<_>> =
            HashMap::with_capacity(blocks.len());
        let forwards_branching_map: HashMap<_, _> = blocks
            .iter()
            .enumerate()
            .map(|(index, block)| {
                let label = unsafe { index::Label::from_index(index) };
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
                        condition_source: _,
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
        }
    }
}

pub fn dissect_from_hlir(blocks: Vec<crate::hlir::Block>) -> IR {
    let return_counts = compute_return_counts(&blocks);
    let compiled_blocks = blocks
        .into_iter()
        .map(|block| Block::from_hlir_block(block, &return_counts))
        .collect();
    IR::from_blocks(compiled_blocks)
}

fn compute_return_counts(blocks: &[crate::hlir::Block]) -> FixedArray<usize> {
    use crate::hlir::End;
    use std::collections::VecDeque;
    let mut slice = vec![0; blocks.len()];

    let mut solved = HashSet::new();

    struct Task {
        index: usize,
        tries: usize,
    }

    impl Task {
        const fn new(index: usize) -> Self {
            Self { index, tries: 0 }
        }
    }

    // ensure we go through everyone before we repeat.
    let mut queue = VecDeque::from_iter((0..blocks.len()).map(Task::new));

    while let Some(next) = queue.pop_front() {
        match &blocks[next.index].end {
            End::TailValue(value) => match value {
                crate::hlir::Value::Copied(pures) => {
                    slice[next.index] = pures.len();
                    solved.insert(next.index);
                    continue;
                }
                crate::hlir::Value::Add { lhs: _, rest: _ } => {
                    slice[next.index] = 1;
                    solved.insert(next.index);
                    continue;
                }
                crate::hlir::Value::Call { label, params: _ } => {
                    let target_index = unsafe { label.to_index() };
                    // if our dependency was solved, then we can resolve this one to the same
                    if solved.contains(&target_index) {
                        slice[next.index] = slice[target_index];
                        solved.insert(next.index);
                        continue;
                    }
                }
            },
            End::ConditionalBranch {
                condition: _,
                label_if_true,
                label_if_false,
            } => {
                let true_index = unsafe { label_if_true.to_index() };
                let false_index = unsafe { label_if_false.to_index() };
                if solved.contains(&true_index) && solved.contains(&false_index) {
                    debug_assert_eq!(slice[true_index], slice[false_index]);
                    slice[next.index] = slice[true_index];
                    solved.insert(next.index);
                    continue;
                }
            }
        }
        if next.tries < 10 {
            // note: 10 tries means 10! call chain depth, which is very very unlikely.
            // I'll just won't continue this one and leave it as zero.
            // TODO: result to indicate that there is a call chain loop
            queue.push_back(next);
        }
    }

    slice.into_boxed_slice()
}

// NOTE: specs aren't used by optimizers, they're used by allocators.
// Therefore they are not defined here on purpose. There are plans
// to include a non-generic Spec that the allocators may use in their
// passes but the Architecture type will only be erased when invoking the
// allocators when their output is passed to the correct codegen for
// the selected Architecture.
