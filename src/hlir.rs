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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct AssignedBinding {
    pub assign_index: usize,
    pub binding: index::Binding,
}

#[derive(Debug)]
pub struct Assignment {
    pub used_bindings: Vec<AssignedBinding>,
    pub value: Value,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Pure {
    Binding(index::Binding),
    Label(index::Label),
    Constant(u64),
}

impl Pure {
    fn from_ast<'src>(
        rvalue: super::ast::Rvalue<'src>,
        bindings: &BindingMap<'src>,
        labels: &mut LabelMap<'src>,
    ) -> Self {
        match rvalue {
            crate::ast::Rvalue::Label(label) => Self::Label(labels.get_or_add_label_index(label)),
            crate::ast::Rvalue::Constant(c) => Self::Constant(c),
            crate::ast::Rvalue::Binding(binding) => {
                Self::Binding(bindings.expect_binding_index(binding))
            }
        }
    }
}

#[derive(Debug)]
pub enum End {
    TailValue(Value),
    ConditionalBranch {
        condition: Pure,
        label_if_true: index::Label,
        label_if_false: index::Label,
    },
    /// Happens when the block is empty.
    BlockIsEmpty,
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

struct BindingMap<'a>(HashMap<&'a str, usize>);

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

impl<'a> FromIterator<(&'a str, usize)> for BindingMap<'a> {
    fn from_iter<T: IntoIterator<Item = (&'a str, usize)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

pub enum Linkage {
    Export,
    Internal,
}

#[derive(Debug)]
pub struct LabelMap<'a> {
    labels: HashMap<&'a str, usize>,
    exports: HashSet<usize>,
}

impl<'a> LabelMap<'a> {
    fn new() -> Self {
        Self {
            labels: HashMap::new(),
            exports: HashSet::new(),
        }
    }

    fn add_label(&mut self, label: LinkageLabel<'a>) {
        let (name, is_export) = match label {
            LinkageLabel::Export(name) => (name, true),
            LinkageLabel::Internal(name) => (name, false),
        };
        let index = unsafe { self.get_or_add_label_index(name).to_index() };
        if is_export {
            self.exports.insert(index);
        }
    }

    fn get_or_add_label_index(&mut self, label_name: &'a str) -> index::Label {
        let default_index = self.labels.len();
        let index = *self.labels.entry(label_name).or_insert(default_index);
        unsafe { index::Label::from_index(index) }
    }
}

impl<'a> Default for LabelMap<'a> {
    fn default() -> Self {
        Self::new()
    }
}

fn expect_label_from_ast<'src>(
    rvalue: Rvalue<'src>,
    label_map: &mut LabelMap<'src>,
) -> index::Label {
    if let Rvalue::Label(name) = rvalue {
        label_map.get_or_add_label_index(name)
    } else {
        panic!("expected {:?} to be a label", rvalue)
    }
}

fn expr_as_br_cond<'src>(
    expr: Expr<'src>,
    binding_map: &BindingMap<'src>,
    label_map: &mut LabelMap<'src>,
) -> Result<(Pure, index::Label, index::Label), Expr<'src>> {
    // br-cond <cond> @true-label @false-label
    if let Expr::Insn {
        name: "br-cond",
        args,
    } = expr
    {
        let mut args = args.into_iter();
        let condition = Pure::from_ast(args.next().unwrap(), binding_map, label_map);
        let label_if_true =
            expect_label_from_ast(args.next().expect("br-cond needs @true-label"), label_map);
        let label_if_false =
            expect_label_from_ast(args.next().expect("br-cond needs @false-label"), label_map);
        Ok((condition, label_if_true, label_if_false))
    } else {
        Err(expr)
    }
}

fn expr_as_value<'src>(
    expr: Expr<'src>,
    binding_map: &BindingMap<'src>,
    label_map: &mut LabelMap<'src>,
) -> Value {
    match expr {
        Expr::Insn { name, args } => {
            match name {
                // add args...
                "add" => {
                    let mut args = args
                        .into_iter()
                        .rev()
                        .map(|arg| Pure::from_ast(arg, binding_map, label_map));
                    let lhs = args.next().expect("must have lhs on add");
                    let rest = args.collect();

                    Value::Add { lhs, rest }
                }
                // call @label args...?
                "call" => {
                    let mut args = args.into_iter();
                    let label = expect_label_from_ast(args.next().unwrap(), label_map);

                    let params = args
                        .map(|arg| Pure::from_ast(arg, binding_map, label_map))
                        .collect();

                    Value::Call { label, params }
                }
                other => panic!("unknown instruction: {:?}", other),
            }
        }
        Expr::Copied(values) => Value::Copied(
            values
                .into_iter()
                .map(|arg| Pure::from_ast(arg, binding_map, label_map))
                .collect(),
        ),
    }
}

impl Block {
    fn from_ast<'src>(
        mut stmts: Vec<Statement<'src>>,
        arguments: Option<Vec<&'src str>>,
        label_map: &mut LabelMap<'src>,
    ) -> Self {
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
            .map(|(a, b)| (b, a))
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

        let end = match stmts.pop() {
            None => End::BlockIsEmpty,
            Some(stmt) => match stmt {
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
                    match expr_as_br_cond(expr, &binding_map, label_map) {
                        Ok((condition, label_if_true, label_if_false)) => End::ConditionalBranch {
                            condition,
                            label_if_true,
                            label_if_false,
                        },
                        Err(other) => End::TailValue(expr_as_value(other, &binding_map, label_map)),
                    }
                }
            },
        };

        let assigns = stmts
            .into_iter()
            .map(|stmt| match stmt {
                crate::ast::Statement::Assign { bindings, value } => Assignment {
                    used_bindings: bindings
                        .into_iter()
                        .enumerate()
                        .filter_map(|(index, lvalue)| match lvalue {
                            crate::ast::Lvalue::Ignore => None,
                            crate::ast::Lvalue::Named(name) => Some(AssignedBinding {
                                assign_index: index,
                                binding: binding_map.expect_binding_index(name),
                            }),
                        })
                        .collect(),
                    value: expr_as_value(value, &binding_map, label_map),
                },
                // We'll have an ignored assignment
                crate::ast::Statement::Return(value) => Assignment {
                    used_bindings: Vec::new(),
                    value: expr_as_value(value, &binding_map, label_map),
                },
            })
            .collect();

        Block { gets, assigns, end }
    }
}

pub struct IR<'src, Arch> {
    label_map: LabelMap<'src>,
    blocks: Vec<Block>,
    specs: Vec<Spec<Arch>>,
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
        let mut label_map = LabelMap::default();
        let (blocks, specs) = ast
            .into_iter()
            .map(|block| {
                label_map.add_label(block.name);
                let spec = block.spec.map(Spec::<Arch>::from_ast).unwrap_or_default();
                let block = Block::from_ast(block.stmts, block.arguments, &mut label_map);
                (block, spec)
            })
            .unzip();

        Self {
            label_map,
            blocks,
            specs,
        }
    }
}
