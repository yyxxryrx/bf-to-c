use crate::ast;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug, Clone, Copy)]
pub enum PtrOffset {
    Left(isize),
    Right(isize),
    None,
}

impl From<isize> for PtrOffset {
    fn from(value: isize) -> Self {
        if value < 0 {
            PtrOffset::Left(-value)
        } else if value > 0 {
            PtrOffset::Right(value)
        } else {
            PtrOffset::None
        }
    }
}

impl From<&isize> for PtrOffset {
    fn from(value: &isize) -> Self {
        if *value < 0 {
            PtrOffset::Left(-value)
        } else if *value > 0 {
            PtrOffset::Right(*value)
        } else {
            PtrOffset::None
        }
    }
}

impl From<PtrOffset> for isize {
    fn from(value: PtrOffset) -> Self {
        match value {
            PtrOffset::Left(i) => -i,
            PtrOffset::Right(i) => i,
            PtrOffset::None => 0,
        }
    }
}

impl From<&mut PtrOffset> for isize {
    fn from(value: &mut PtrOffset) -> Self {
        match value {
            PtrOffset::Left(i) => -*i,
            PtrOffset::Right(i) => *i,
            PtrOffset::None => 0,
        }
    }
}

impl std::fmt::Display for PtrOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PtrOffset::Left(i) => write!(f, " - {i}"),
            PtrOffset::Right(i) => write!(f, " + {i}"),
            PtrOffset::None => write!(f, ""),
        }
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub enum IROp {
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for IROp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IRExpr {
    pub source: PtrOffset,
    pub op: Option<IROp>,
    pub value: Option<isize>,
}

#[derive(Debug, Clone, Copy)]
pub enum IRValue {
    Const(isize),
    Expr(IRExpr),
}

impl From<isize> for IRValue {
    fn from(value: isize) -> Self {
        Self::Const(value)
    }
}

impl From<&isize> for IRValue {
    fn from(value: &isize) -> Self {
        Self::Const(*value)
    }
}

impl From<IRExpr> for IRValue {
    fn from(expr: IRExpr) -> Self {
        Self::Expr(expr)
    }
}

#[derive(Debug, Clone)]
pub enum BfIR {
    IncrementPointer(isize),
    DecrementPointer(isize),
    IncrementValue(PtrOffset, IRValue),
    DecrementValue(PtrOffset, IRValue),
    /// SetValue(ptr_offset, value)
    SetValue(PtrOffset, isize),
    Output(PtrOffset),
    Input(PtrOffset),
    DeadLoop(Vec<BfIR>),
    WhileLoop(Vec<BfIR>),
    ForLoop(Vec<BfIR>, isize),
}

trait ValueChange<T> {
    fn value(ptr_offset: PtrOffset, val: T) -> Self;
}

impl ValueChange<isize> for BfIR {
    fn value(ptr_offset: PtrOffset, val: isize) -> Self {
        if val >= 0 {
            Self::IncrementValue(ptr_offset, val.into())
        } else {
            Self::DecrementValue(ptr_offset, (-val).into())
        }
    }
}

impl ValueChange<&mut IRExpr> for BfIR {
    fn value(ptr_offset: PtrOffset, val: &mut IRExpr) -> Self {
        match (val.op, val.value) {
            (Some(op), Some(value)) => match op {
                IROp::Mul | IROp::Div => {
                    if value >= 0 {
                        Self::IncrementValue(ptr_offset, val.clone().into())
                    } else {
                        val.value = Some(-value);
                        Self::DecrementValue(ptr_offset, val.clone().into())
                    }
                }
                _ => Self::IncrementValue(ptr_offset, val.clone().into()),
            },
            _ => Self::IncrementValue(ptr_offset, val.clone().into()),
        }
    }
}

impl BfIR {
    fn is_for_loop(&self) -> bool {
        matches!(self, BfIR::ForLoop(_, _))
    }

    fn is_loop(&self) -> bool {
        matches!(self, BfIR::WhileLoop(_) | BfIR::ForLoop(_, _))
    }

    #[allow(unused)]
    fn with_offset(&self, ptr_offset: PtrOffset) -> Self {
        match self {
            Self::IncrementValue(_, val) => Self::IncrementValue(ptr_offset, *val),
            Self::DecrementValue(_, val) => Self::DecrementValue(ptr_offset, *val),
            Self::SetValue(_, val) => Self::SetValue(ptr_offset, *val),
            Self::Input(_) => Self::Input(ptr_offset),
            Self::Output(_) => Self::Output(ptr_offset),
            _ => self.clone(),
        }
    }
}

fn ast_add_to_ir(ast_add: &ast::BfAst) -> Option<BfIR> {
    let ast::BfAst::Add(i) = ast_add else {
        panic!("Not an add")
    };
    if *i == 0 {
        None
    } else {
        Some(BfIR::value(PtrOffset::None, *i))
    }
}

fn ast_move_to_ir(ast_move: &ast::BfAst) -> Option<BfIR> {
    let ast::BfAst::Move(i) = ast_move else {
        panic!("Not a move")
    };
    if *i > 0 {
        Some(BfIR::IncrementPointer(*i))
    } else if *i < 0 {
        Some(BfIR::DecrementPointer(-i))
    } else {
        None
    }
}

fn ast_loop_to_ir(ast_loop: &ast::BfAst) -> BfIR {
    let ast::BfAst::Loop(ast_loop) = ast_loop else {
        panic!("Not a loop")
    };
    let mut ir = vec![];
    let mut ptr = 0;
    let mut value_changed = 0;
    let mut all_for = true;
    for child_ast in ast_loop {
        match child_ast {
            ast::BfAst::Move(i) => {
                ptr += i;
                if let Some(move_ir) = ast_move_to_ir(child_ast) {
                    ir.push((move_ir, 1));
                }
            }
            ast::BfAst::Add(i) => {
                if ptr == 0 {
                    value_changed += i;
                }
                if let Some(ir_add) = ast_add_to_ir(child_ast) {
                    ir.push((ir_add, ptr));
                }
            }
            ast::BfAst::Loop(_) => {
                let loop_ir = ast_loop_to_ir(child_ast);
                all_for = all_for && (loop_ir.is_for_loop() || !loop_ir.is_loop());
                ir.push((loop_ir, 1));
            }
            ast::BfAst::Output => ir.push((BfIR::Output(PtrOffset::None), 1)),
            ast::BfAst::Input => ir.push((BfIR::Input(PtrOffset::None), 1)),
        }
    }
    if ptr == 0 && all_for {
        BfIR::ForLoop(
            ir.into_iter()
                .filter_map(|(ir, ptr)| if ptr == 0 { None } else { Some(ir) })
                .collect(),
            value_changed,
        )
    } else {
        BfIR::WhileLoop(ir.into_iter().map(|(ir, _)| ir).collect())
    }
}

pub fn generate_ir(ast_tree: &Vec<ast::BfAst>) -> Vec<BfIR> {
    let mut ir = vec![];
    for ast_child in ast_tree {
        let child_ir = match ast_child {
            ast::BfAst::Move(_) => ast_move_to_ir(ast_child),
            ast::BfAst::Add(_) => ast_add_to_ir(ast_child),
            ast::BfAst::Loop(_) => Some(ast_loop_to_ir(ast_child)),
            ast::BfAst::Output => Some(BfIR::Output(PtrOffset::None)),
            ast::BfAst::Input => Some(BfIR::Input(PtrOffset::None)),
        };
        if let Some(child_ir) = child_ir {
            ir.push(child_ir);
        }
    }
    ir
}

struct OptimizeIRContext {
    has_while: bool,
    local_var: HashMap<isize, u8>,
}

impl Default for OptimizeIRContext {
    fn default() -> Self {
        Self {
            has_while: false,
            local_var: (-15000..15000).map(|i| (i, 0u8)).collect(),
        }
    }
}

pub fn optimize_ir(ir: &mut Vec<BfIR>) {
    fn _opt(ir: &mut Vec<BfIR>, ctx: &mut OptimizeIRContext) {
        let mut has_while = ctx.has_while;
        let local_var = &mut ctx.local_var;
        let mut ptr = 0;
        let mut index = 0;
        while index < ir.len() {
            let child = &mut ir[index];
            match child {
                BfIR::ForLoop(children, value_changed) => {
                    if local_var.get(&ptr) == Some(&0) {
                        ir.remove(index);
                        continue;
                    }
                    if children.is_empty() && *value_changed == -1 {
                        *child = BfIR::SetValue(0.into(), 0);
                        index += 1;
                        continue;
                    }
                    optimize_ir(children);
                    if *value_changed == 0 {
                        *child = BfIR::DeadLoop(children.clone());
                        index += 1;
                        continue;
                    }
                    let is_monolayer = children.iter().all(|child| !child.is_loop());
                    if is_monolayer {
                        let mut has_other = false;
                        let mut local_ptr = 0;
                        let mut value_changes = HashMap::new();
                        for child in children {
                            match child {
                                BfIR::IncrementValue(ptr_offset, i) => {
                                    let ptr_offset: isize = ptr_offset.into();
                                    if let IRValue::Const(value) = i {
                                        *value_changes
                                            .entry(local_ptr + ptr_offset)
                                            .or_insert(0) += *value;
                                    } else {
                                        value_changes.remove(&(local_ptr + ptr_offset));
                                    }
                                }
                                BfIR::DecrementValue(ptr_offset, i) => {
                                    let ptr_offset: isize = ptr_offset.into();
                                    if let IRValue::Const(value) = i {
                                        *value_changes
                                            .entry(local_ptr + ptr_offset)
                                            .or_insert(0) -= *value;
                                    } else {
                                        value_changes.remove(&(local_ptr + ptr_offset));
                                    }
                                }
                                BfIR::SetValue(ptr_offset, value) => {
                                    let ptr_offset: isize = ptr_offset.into();
                                    value_changes
                                        .entry(local_ptr + ptr_offset)
                                        .insert_entry(*value);
                                }
                                BfIR::IncrementPointer(i) => {
                                    local_ptr += *i;
                                }
                                BfIR::DecrementPointer(i) => {
                                    local_ptr -= *i;
                                }
                                _ => {
                                    has_other = true;
                                    break;
                                }
                            }
                        }
                        if let Some(init_value) = local_var.get(&ptr) {
                            if *init_value as isize % *value_changed != 0 {
                                index += 1;
                                continue;
                            }
                            let init_value = if *value_changed < 0 {
                                *init_value
                            } else {
                                255 - *init_value
                            };
                            let count = init_value as isize / (*value_changed).abs();
                            if !has_other {
                                ir.remove(index);
                                for (offset, (ptr, value)) in value_changes.iter().enumerate() {
                                    ir.insert(
                                        index + offset,
                                        BfIR::value(ptr.into(), *value * count),
                                    );
                                    *local_var.entry((index + offset) as _).or_insert(0) =
                                        (*value * count) as u8;
                                }
                                index += value_changes.len();
                                ir.insert(index, BfIR::SetValue(0.into(), 0));
                                continue;
                            }
                        }
                        if (*value_changed).abs() == 1 && !has_other {
                            ir.remove(index);
                            for (offset, (ptr, value)) in value_changes.iter().enumerate() {
                                let mut expr = IRExpr {
                                    source: PtrOffset::None,
                                    op: Some(IROp::Mul),
                                    value: Some(*value),
                                };
                                ir.insert(index + offset, BfIR::value(ptr.into(), &mut expr));
                                local_var.remove(&((index + offset) as isize));
                            }
                            index += value_changes.len();
                            ir.insert(index, BfIR::SetValue(0.into(), 0));
                            continue;
                        }
                    }
                }
                BfIR::IncrementPointer(i) => {
                    ptr += *i;
                }
                BfIR::DecrementPointer(i) => {
                    ptr -= *i;
                }
                BfIR::IncrementValue(ptr_offset, i) => {
                    let ptr_offset: isize = ptr_offset.into();
                    let ptr = ptr.clone() + ptr_offset;
                    if !has_while || ptr == 0 {
                        if let IRValue::Const(value) = i {
                            let val = local_var
                                .entry(ptr)
                                .or_insert(0)
                                .overflowing_add(*value as u8)
                                .0;
                            local_var.insert(ptr, val);
                        } else {
                            local_var.remove(&ptr);
                        }
                    }
                }
                BfIR::DecrementValue(ptr_offset, i) => {
                    let ptr_offset: isize = ptr_offset.into();
                    let ptr = ptr.clone() + ptr_offset;
                    if !has_while || ptr == 0 {
                        if let IRValue::Const(value) = i {
                            let val = local_var
                                .entry(ptr)
                                .or_insert(0)
                                .overflowing_sub(*value as u8)
                                .0;
                            local_var.insert(ptr, val);
                        } else {
                            local_var.remove(&ptr);
                        }
                    }
                }
                BfIR::SetValue(ptr_offset, value) => {
                    let ptr_offset: isize = ptr_offset.into();
                    local_var.insert(ptr + ptr_offset, *value as u8);
                }
                BfIR::WhileLoop(children) => {
                    has_while = true;
                    optimize_ir(children);
                    ptr = 0;
                    local_var.clear();
                    local_var.insert(ptr, 0);
                }
                BfIR::Input(_) => {
                    local_var.remove(&ptr);
                }
                _ => {}
            }
            index += 1;
        }
    }
    _opt(ir, &mut OptimizeIRContext::default());
}
