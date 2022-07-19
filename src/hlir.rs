//! A High-Level Intermediate Representation.
//! Serves for debugging purposes, and to turn down
//! the AST into a stricter layout that doesn't use
//! strings.
//!
//! This module will be responsible for handing out
//! user errors in the case of there being any. Past
//! this point any malformed IR is purely the program's
//! fault.
use crate::arch::Architecture;
use crate::index;
use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, LinkageLabel, Lvalue, Rvalue, Statement};

// TODO: Convert tail calls with single parent-child relationship to jumps

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Block {
    // arguments
    pub gets: Vec<index::Binding>,
    // other calculations (could be ignored)
    pub assigns: Vec<Assignment>,
    pub end: End,
}

pub struct Spec<Arch> {
    pub arguments: Vec<index::Register>,
    pub returns: Vec<index::Register>,
    _phantom: std::marker::PhantomData<Arch>,
}

// needs its own Debug impl
// ... because automatic deriving places Arch: Debug. Lol.
impl<Arch> std::fmt::Debug for Spec<Arch> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Spec")
            .field("arguments", &self.arguments)
            .field("returns", &self.returns)
            .finish()
    }
}

// needs my own default impl because automatic deriving
// places `Arch: Default` when it is not needed (since we're using
// it only in phantom data)
impl<Arch> Default for Spec<Arch> {
    fn default() -> Self {
        Self {
            arguments: Vec::new(),
            returns: Vec::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct AssignedBinding {
    pub assign_index: u8,
    pub binding: index::Binding,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Assignment {
    pub used_bindings: Vec<AssignedBinding>,
    pub value: Value,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Value {
    Copied(Vec<Pure>),
    Add {
        lhs: Pure,
        rest: Vec<Pure>,
    },
    // TODO: accept bindings as dynamic call labels (needed for e.g vtable implementation)
    Call {
        label: index::Label,
        params: Vec<Pure>,
    },
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Pure {
    Binding(index::Binding),
    Label(index::Label),
    Constant(u64),
}

impl Pure {
    fn from_ast<'src>(
        rvalue: super::ast::Rvalue<'src>,
        bindings: &BindingMap<'src>,
        labels: &LabelMap<'src>,
    ) -> Option<Self> {
        match rvalue {
            crate::ast::Rvalue::Label(label) => labels.get_label_index(label).map(Self::Label),
            crate::ast::Rvalue::Constant(c) => Some(Self::Constant(c)),
            crate::ast::Rvalue::Binding(binding) => {
                bindings.get_binding_index(binding).map(Self::Binding)
            }
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum End {
    TailValue(Value),
    ConditionalBranch {
        condition: Pure,
        label_if_true: index::Label,
        label_if_false: index::Label,
    },
}

fn register_list_from_ast<'s, A: Architecture, L: IntoIterator<Item = &'s str>>(
    list: L,
) -> Vec<index::Register> {
    list.into_iter()
        .filter_map(index::Register::from::<A>)
        .collect()
}

impl<Arch> Spec<Arch> {
    fn from_ast(super::ast::Spec { arguments, returns }: super::ast::Spec) -> Self
    where
        Arch: Architecture,
    {
        Self {
            arguments: arguments
                .map(register_list_from_ast::<Arch, _>)
                .unwrap_or_default(),
            returns: returns
                .map(register_list_from_ast::<Arch, _>)
                .unwrap_or_default(),

            _phantom: Default::default(),
        }
    }
}

struct BindingMap<'a>(HashMap<&'a str, u32>);

impl<'a> BindingMap<'a> {
    fn expect_binding_index(&self, name: &'a str) -> index::Binding {
        let index = self.get_binding_index(name);
        match index {
            Some(value) => value,
            None => panic!("binding {:?} not defined", name),
        }
    }

    // Used by functions which know that the binding is there.
    unsafe fn get_binding_unchecked(&self, name: &'a str) -> index::Binding {
        unsafe { self.get_binding_index(name).unwrap_unchecked() }
    }

    fn get_binding_index(&self, name: &'a str) -> Option<index::Binding> {
        self.0
            .get(&name)
            .copied()
            .map(|index| unsafe { index::Binding::from_index(index) })
    }
}

impl<'a> FromIterator<(&'a str, u32)> for BindingMap<'a> {
    fn from_iter<T: IntoIterator<Item = (&'a str, u32)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

pub enum Linkage {
    Export,
    Internal,
}

#[derive(Debug)]
pub struct LabelMap<'a> {
    labels: HashMap<&'a str, u16>, // no more than 65536 labels
    /// The first N labels are exported
    export_count: u16,
}

impl<'a> LabelMap<'a> {
    fn get_label_index(&self, name: &'a str) -> Option<index::Label> {
        self.labels
            .get(name)
            .copied()
            .map(|index| unsafe { index::Label::from_index(index) })
    }
}

fn expect_label_from_ast<'src>(
    rvalue: Rvalue<'src>,
    label_map: &LabelMap<'src>,
) -> Option<index::Label> {
    if let Rvalue::Label(name) = rvalue {
        label_map.get_label_index(name)
    } else {
        None
    }
}

fn expr_as_br_cond<'src>(
    expr: Expr<'src>,
    binding_map: &BindingMap<'src>,
    label_map: &LabelMap<'src>,
) -> Option<Result<(Pure, index::Label, index::Label), Expr<'src>>> {
    // br-cond <cond> @true-label @false-label
    Some(
        if let Expr::Insn {
            name: "br-cond",
            args,
        } = expr
        {
            let mut args = args.into_iter();
            let condition = Pure::from_ast(args.next()?, binding_map, label_map)?;
            let label_if_true = expect_label_from_ast(args.next()?, label_map)?;
            let label_if_false = expect_label_from_ast(args.next()?, label_map)?;
            Ok((condition, label_if_true, label_if_false))
        } else {
            Err(expr)
        },
    )
}

fn expr_as_value<'src>(
    expr: Expr<'src>,
    binding_map: &BindingMap<'src>,
    label_map: &LabelMap<'src>,
) -> Option<Value> {
    match expr {
        Expr::Insn { name, args } => {
            match name {
                // add args...
                "add" => {
                    let mut args = args
                        .into_iter()
                        .rev()
                        .filter_map(|arg| Pure::from_ast(arg, binding_map, label_map));
                    let lhs = args.next()?;
                    let rest = args.collect();

                    Some(Value::Add { lhs, rest })
                }
                // call @label args...?
                "call" => {
                    let mut args = args.into_iter();
                    let label = expect_label_from_ast(args.next()?, label_map)?;

                    let params = args
                        .filter_map(|arg| Pure::from_ast(arg, binding_map, label_map))
                        .collect();

                    Some(Value::Call { label, params })
                }
                _ => None,
            }
        }
        Expr::Copied(values) => values
            .into_iter()
            .map(|arg| Pure::from_ast(arg, binding_map, label_map))
            .collect::<Option<_>>()
            .map(Value::Copied),
    }
}

impl Block {
    fn from_ast<'src>(
        mut stmts: Vec<Statement<'src>>,
        arguments: Option<Vec<&'src str>>,
        label_map: &LabelMap<'src>,
    ) -> Option<Self> {
        // create a binding with:
        // - arguments defined
        // - defined bindngs in assignments
        let binding_map: BindingMap = arguments
            .iter()
            .flatten()
            .copied()
            .chain(
                stmts
                    .iter()
                    .flat_map(|stmt| {
                        if let Statement::Assign { bindings, .. } = stmt {
                            Some(bindings.iter().filter_map(Lvalue::name_if_not_ignored))
                        } else {
                            None
                        }
                    })
                    .flatten(),
            )
            .enumerate()
            .map(|(a, b)| (b, a as u32))
            .collect();

        let gets = arguments
            .into_iter()
            .flatten()
            .map(|name| {
                // SAFE: we just added them!
                unsafe { binding_map.get_binding_unchecked(name) }
            })
            .collect();

        // re-declare as immutable
        let binding_map = binding_map;

        let end = match stmts.pop()? {
            crate::ast::Statement::Assign { bindings, value } => {
                // we're going to implicitly create a `Copied` tail value
                let assigned_bindings = bindings
                    .iter()
                    .copied()
                    .filter_map(|lv| match lv {
                        crate::ast::Lvalue::Ignore => None,
                        crate::ast::Lvalue::Named(name) => {
                            Some(binding_map.expect_binding_index(name))
                        }
                    })
                    .map(Pure::Binding)
                    .collect();

                // push the assignment back
                stmts.push(crate::ast::Statement::Assign { bindings, value });

                End::TailValue(Value::Copied(assigned_bindings))
            }
            // TODO: check if the return value is a `br` insn
            crate::ast::Statement::Return(expr) => {
                match expr_as_br_cond(expr, &binding_map, label_map)? {
                    Ok((condition, label_if_true, label_if_false)) => End::ConditionalBranch {
                        condition,
                        label_if_true,
                        label_if_false,
                    },
                    Err(other) => End::TailValue(expr_as_value(other, &binding_map, label_map)?),
                }
            }
        };

        let assigns = stmts
            .into_iter()
            .filter_map(|stmt| match stmt {
                crate::ast::Statement::Assign { bindings, value } => {
                    let value = expr_as_value(value, &binding_map, label_map)?;
                    let (used_bindings, value) = if let Value::Copied(copied) = value {
                        // Ignore `Pure` values that were ignored
                        let (used_bindings, values): (Vec<_>, Vec<_>) = bindings
                            .into_iter()
                            .zip(copied)
                            .filter_map(|(lvalue, copied)| {
                                lvalue
                                    .name_if_not_ignored()
                                    .map(|name| (binding_map.expect_binding_index(name), copied))
                            })
                            .unzip();

                        (
                            used_bindings
                                .into_iter()
                                .enumerate()
                                .map(|(assign_index, binding)| AssignedBinding {
                                    assign_index: assign_index as u8,
                                    binding,
                                })
                                .collect(),
                            Value::Copied(values),
                        )
                    } else {
                        (
                            bindings
                                .into_iter()
                                .enumerate()
                                .filter_map(|(assign_index, lvalue)| {
                                    lvalue
                                        .name_if_not_ignored()
                                        .map(|name| binding_map.expect_binding_index(name))
                                        .map(move |binding| AssignedBinding {
                                            assign_index: assign_index as u8,
                                            binding,
                                        })
                                })
                                .collect(),
                            value,
                        )
                    };

                    Some(Assignment {
                        used_bindings,
                        value,
                    })
                }
                // We'll have an ignored assignment
                crate::ast::Statement::Return(value) => Some(Assignment {
                    used_bindings: Vec::new(),
                    value: expr_as_value(value, &binding_map, label_map)?,
                }),
            })
            .collect();

        Some(Block { gets, assigns, end })
    }
}

pub struct IR<'src, Arch> {
    pub label_map: LabelMap<'src>,
    pub blocks: Vec<Block>,
    pub specs: Vec<Spec<Arch>>,
}

// ... I'm getting tired of automatic deriving telling Arch: Debug
// when IT'S NOT NEEDED BY THE IMPLEMENTATION.
impl<Arch> std::fmt::Debug for IR<'_, Arch> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IR")
            .field("label_map", &self.label_map)
            .field("blocks", &self.blocks)
            .field("specs", &self.specs)
            .finish()
    }
}

impl<'src, Arch> IR<'src, Arch> {
    pub fn from_ast(ast: Vec<crate::ast::Block<'src>>) -> Self
    where
        Arch: Architecture,
    {
        // 1. Collect the number of exports
        let export_count = ast
            .iter()
            .filter(|block| matches!(block.name, LinkageLabel::Export(_)))
            .count() as u16;

        // 2. Make the labels with the indices, use two:
        // one for the exports and one for the locals.
        // the locals have their index offset by the export count, so that
        // the exports are the first ones.
        let labels = ast
            .iter()
            .scan(
                (0, export_count),
                |(ref mut export_index, ref mut local_index), block| {
                    Some(match block.name {
                        LinkageLabel::Export(name) => {
                            let index = *export_index;
                            *export_index += 1;
                            (name, index)
                        }
                        LinkageLabel::Internal(name) => {
                            let index = *local_index;
                            *local_index += 1;
                            (name, index)
                        }
                    })
                },
            )
            .collect();

        let label_map = LabelMap {
            labels,
            export_count,
        };

        let (blocks, specs) = ast
            .into_iter()
            .filter_map(|block| {
                let spec = block.spec.map(Spec::<Arch>::from_ast).unwrap_or_default();
                let block = Block::from_ast(block.stmts, block.arguments, &label_map)?;
                Some((block, spec))
            })
            .unzip();

        Self {
            label_map,
            blocks,
            specs,
        }
    }
}

// limit implementation for Arbitrary<Vec<Block>>,
// so its output can be fed to OPTIR conversion
// reasons:
// - labels must have been created according to block amount,
//   so the label index must match for the block
#[cfg(feature = "arbitrary")]
fn check_arbitrary_label(label: index::Label, block_len: usize) -> bool {
    (unsafe { label.to_index() }) < block_len
}

#[cfg(feature = "arbitrary")]
fn check_pure_label(pure: &Pure, block_len: usize) -> bool {
    if let Pure::Label(label) = pure {
        check_arbitrary_label(*label, block_len)
    } else {
        true
    }
}

#[cfg(feature = "arbitrary")]
fn check_value_labels(value: &Value, block_len: usize) -> bool {
    match value {
        Value::Copied(pures) => pures.iter().all(|pure| check_pure_label(pure, block_len)),
        Value::Add { lhs, rest } => {
            check_pure_label(lhs, block_len)
                && rest.iter().all(|pure| check_pure_label(pure, block_len))
        }
        Value::Call { label, params } => {
            check_arbitrary_label(*label, block_len)
                && params.iter().all(|pure| check_pure_label(pure, block_len))
        }
    }
}

#[cfg(feature = "arbitrary")]
fn check_block_labels(block: &Block, block_len: usize) -> bool {
    (match &block.end {
        End::TailValue(value) => check_value_labels(value, block_len),
        End::ConditionalBranch {
            condition,
            label_if_true,
            label_if_false,
        } => {
            check_pure_label(condition, block_len)
                && check_arbitrary_label(*label_if_true, block_len)
                && check_arbitrary_label(*label_if_false, block_len)
        }
    }) && block
        .assigns
        .iter()
        .all(|assign| check_value_labels(&assign.value, block_len))
}

#[cfg(feature = "arbitrary")]
pub fn check_arbitary_blocks(blocks: Vec<Block>) -> Result<Vec<Block>, arbitrary::Error> {
    let block_len = blocks.len();
    if !blocks
        .iter()
        .all(|block| check_block_labels(block, block_len))
    {
        Err(arbitrary::Error::IncorrectFormat)
    } else {
        Ok(blocks)
    }
}
