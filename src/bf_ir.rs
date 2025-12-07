use crate::ast;
use crate::bf_ir::error::{OperationMergeError, OperationMergeResult};
use nohash::BuildNoHashHasher;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl PtrOffset {
    pub fn to_ir<'a>(&self) -> Option<(&'a str, isize)> {
        match self {
            Self::Left(val) => Some(("sub", *val)),
            Self::Right(val) => Some(("add", *val)),
            Self::None => None,
        }
    }

    pub fn apply_offset(&self, ptr: isize) -> isize {
        match self {
            Self::None => ptr,
            Self::Left(val) => ptr - *val,
            Self::Right(val) => ptr + *val,
        }
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq)]
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

impl IROp {
    pub fn to_ir(&self) -> &str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "udiv",
        }
    }

    pub fn op<T, U>(&self, a: T, b: T) -> U
    where
        T: std::ops::Add<Output = U>
            + std::ops::Sub<Output = U>
            + std::ops::Div<Output = U>
            + std::ops::Mul<Output = U>,
    {
        match self {
            Self::Add => a + b,
            Self::Sub => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IRExpr {
    pub source: PtrOffset,
    pub op: Option<IROp>,
    pub value: Option<isize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl IRValue {
    pub fn merge(&self, other_value: &Self, op: IROp) -> OperationMergeResult<Self> {
        match (self, other_value) {
            (Self::Const(val1), Self::Const(val2)) => Ok(Self::Const(op.op(*val1, *val2))),
            (Self::Expr(expr), Self::Const(val)) | (Self::Const(val), Self::Expr(expr)) => {
                match (expr.source, expr.op, expr.value) {
                    (src, None, None) => Ok(Self::Expr(IRExpr {
                        source: src,
                        op: Some(op),
                        value: Some(*val),
                    })),
                    (src, Some(op2), Some(val2)) => {
                        if op2 == op {
                            Ok(Self::Expr(IRExpr {
                                source: src,
                                op: Some(op),
                                value: Some(*val + val2),
                            }))
                        } else {
                            Err(OperationMergeError::CannotMatchExpr)
                        }
                    }
                    _ => Err(OperationMergeError::CannotMatchExpr),
                }
            }
            _ => Err(OperationMergeError::CannotMatchExpr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRValues(isize, Vec<(IROp, IRValue)>);

impl IRValues {
    pub fn new() -> Self {
        Self { 0: 0, 1: vec![] }
    }

    pub fn push(&mut self, value: IRValue, op: IROp) {
        let IRValue::Const(val) = value else {
            self.1.push((op, value));
            return;
        };
        self.0 = op.op(self.0, val);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BfIR {
    IncrementPointer(isize),
    DecrementPointer(isize),
    IncrementValue(PtrOffset, IRValue),
    IncrementValues(PtrOffset, IRValues),
    DecrementValue(PtrOffset, IRValue),
    DecrementValues(PtrOffset, IRValues),
    /// SetValue(ptr_offset, value)
    SetValue(PtrOffset, isize),
    SetExpr(PtrOffset, IRValue),
    SetExprs(PtrOffset, IRValues),
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
    pub fn is_for_loop(&self) -> bool {
        matches!(self, BfIR::ForLoop(..))
    }

    pub fn is_loop(&self) -> bool {
        matches!(self, BfIR::WhileLoop(..) | BfIR::ForLoop(..))
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

    pub fn merge_value(&self, other_ir: &Self) -> OperationMergeResult<Self> {
        match (self, other_ir) {
            (Self::IncrementValue(_, val1), Self::IncrementValue(offset, val2)) => {
                match val1.merge(val2, IROp::Add) {
                    Ok(val) => Ok(Self::IncrementValue(*offset, val)),
                    Err(..) => {
                        let mut values = IRValues::new();
                        values.push(*val1, IROp::Add);
                        values.push(*val2, IROp::Add);
                        Ok(BfIR::IncrementValues(*offset, values))
                    }
                }
            }
            (Self::DecrementValue(_, val1), Self::DecrementValue(offset, val2)) => {
                let mut values = IRValues::new();
                values.push(*val1, IROp::Sub);
                values.push(*val2, IROp::Sub);
                Ok(BfIR::IncrementValues(*offset, values))
            }
            (Self::IncrementValue(_, val1), Self::DecrementValue(offset, val2))
            | (Self::DecrementValue(_, val2), Self::IncrementValue(offset, val1)) => {
                match val1.merge(val2, IROp::Sub) {
                    Ok(val) => Ok(Self::IncrementValue(*offset, val)),
                    Err(..) => {
                        let mut values = IRValues::new();
                        values.push(*val1, IROp::Add);
                        values.push(*val2, IROp::Sub);
                        Ok(BfIR::IncrementValues(*offset, values))
                    }
                }
            }
            _ => Err(OperationMergeError::CannotMerge {
                one: format!("{self:?}"),
                other_one: format!("{other_ir:?}"),
            }),
        }
    }

    pub fn merge_op(&self, other_ir: &Self) -> OperationMergeResult<Self> {
        Ok(match (self, other_ir) {
            (.., Self::SetValue(..)) | (.., Self::SetExpr(..)) | (.., Self::SetExprs(..)) => {
                other_ir.clone()
            }
            (Self::SetValue(_, value), Self::IncrementValue(offset, val)) => match val {
                IRValue::Const(val) => Self::SetValue(*offset, *val + *value),
                IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                    (.., Some(..), Some(..)) => Self::SetExprs(
                        *offset,
                        IRValues {
                            0: *value,
                            1: vec![(IROp::Add, *val)],
                        },
                    ),
                    (src, None, None) => Self::SetExpr(
                        *offset,
                        IRValue::Expr(IRExpr {
                            source: src,
                            op: Some(IROp::Add),
                            value: Some(*value),
                        }),
                    ),
                    _ => return Err(OperationMergeError::CannotMatchExpr),
                },
            },
            (Self::SetValue(_, value), Self::DecrementValue(offset, val)) => match val {
                IRValue::Const(val) => Self::SetValue(*offset, *value - *val),
                IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                    (.., Some(..), Some(..)) => Self::SetExprs(
                        *offset,
                        IRValues {
                            0: *value,
                            1: vec![(IROp::Sub, *val)],
                        },
                    ),
                    (src, None, None) => Self::SetExpr(
                        *offset,
                        IRValue::Expr(IRExpr {
                            source: src,
                            op: Some(IROp::Sub),
                            value: Some(*value),
                        }),
                    ),
                    _ => return Err(OperationMergeError::CannotMatchExpr),
                },
            },
            (Self::IncrementValue(..), Self::IncrementValue(..))
            | (Self::IncrementValues(..), Self::DecrementValue(..))
            | (Self::DecrementValue(..), Self::IncrementValue(..))
            | (Self::DecrementValue(..), Self::DecrementValue(..)) => self.merge_value(other_ir)?,
            _ => {
                todo!("还没做完呢");
                return Err(OperationMergeError::CannotMerge {
                    one: format!("{self:?}"),
                    other_one: format!("{other_ir:?}"),
                });
            }
        })
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
            ast::BfAst::Loop(..) => {
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
            ast::BfAst::Move(..) => ast_move_to_ir(ast_child),
            ast::BfAst::Add(..) => ast_add_to_ir(ast_child),
            ast::BfAst::Loop(..) => Some(ast_loop_to_ir(ast_child)),
            ast::BfAst::Output => Some(BfIR::Output(PtrOffset::None)),
            ast::BfAst::Input => Some(BfIR::Input(PtrOffset::None)),
        };
        if let Some(child_ir) = child_ir {
            ir.push(child_ir);
        }
    }
    ir
}

#[derive(Debug, Clone)]
struct OptimizeIRContext {
    has_while: bool,
    local_var: HashMap<isize, u8, BuildNoHashHasher<isize>>,
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
                    if children.is_empty() && (*value_changed).abs() == 1 {
                        *child = BfIR::SetValue(0.into(), 0);
                        index += 1;
                        continue;
                    }
                    let mut new_ctx = OptimizeIRContext {
                        local_var: local_var.clone(),
                        has_while: ctx.has_while,
                    };
                    _opt(children, &mut new_ctx);
                    // *local_var = new_ctx.local_var;
                    if *value_changed == 0 {
                        *child = BfIR::DeadLoop(children.clone());
                        index += 1;
                        continue;
                    }
                    let is_monolayer = children.iter().all(|child| !child.is_loop());
                    if is_monolayer {
                        let mut has_other = false;
                        let mut local_ptr = 0;
                        let mut value_changes: HashMap<isize, isize, BuildNoHashHasher<isize>> =
                            Default::default();
                        for child in children.into_iter() {
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
                            if children.is_empty() {
                                *child = BfIR::SetValue(PtrOffset::None, 0);
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
                BfIR::WhileLoop(..) => {
                    has_while = true;
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

#[derive(Debug, Default)]
pub struct OptimizeIROADContext {
    pub local_var_refence_count: HashMap<isize, usize, BuildNoHashHasher<isize>>,
    pub local_var_ir_index: HashMap<
        isize,
        HashMap<usize, Vec<usize>, BuildNoHashHasher<isize>>,
        BuildNoHashHasher<isize>,
    >,
    pub ptr: isize,
}

/// oad 是 Operation And Dead code 的缩写
///
/// 这个函数是用于合并操作和清楚死代码的
///
/// 开发中
pub fn optimize_ir_oad(ir: &mut Vec<BfIR>) {
    // 这里的 Result 是用来当遇到 while 循环的时候直接全部退出用的，所以不需要管他
    fn _scan(ir: &Vec<BfIR>, ctx: &mut OptimizeIROADContext) -> Result<(), ()> {
        let mut index = 0;
        while index < ir.len() {
            let child_ir = &ir[index];
            match child_ir {
                BfIR::SetValue(offset, _) => {
                    let ptr = offset.apply_offset(ctx.ptr);
                    let count = *ctx.local_var_refence_count.entry(ptr).or_insert(0);
                    ctx.local_var_ir_index
                        .entry(ptr)
                        .or_default()
                        .entry(count)
                        .or_default()
                        .push(index);
                }
                BfIR::IncrementValue(offset, val) | BfIR::DecrementValue(offset, val) => {
                    let ptr = offset.apply_offset(ctx.ptr);
                    let count = *ctx.local_var_refence_count.entry(ptr).or_insert(0);
                    ctx.local_var_ir_index
                        .entry(ptr)
                        .or_default()
                        .entry(count)
                        .or_default()
                        .push(index);
                    if let IRValue::Expr(expr) = val {
                        let ptr = expr.source.apply_offset(ctx.ptr);
                        *ctx.local_var_refence_count.entry(ptr).or_insert(0) += 1;
                    }
                }
                BfIR::Output(offset) | BfIR::Input(offset) => {
                    let ptr = offset.apply_offset(ctx.ptr);
                    *ctx.local_var_refence_count.entry(ptr).or_insert(0) += 1;
                }
                BfIR::IncrementPointer(val) => {
                    ctx.ptr += *val;
                }
                BfIR::DecrementPointer(val) => {
                    ctx.ptr -= *val;
                }
                BfIR::WhileLoop(..) => {
                    for i in ctx.local_var_refence_count.values_mut() {
                        *i += 1;
                    }
                    return Err(());
                }
                _ => {}
            }
            index += 1;
        }
        Ok(())
    }

    fn _opt(ir: &mut Vec<BfIR>, ctx: &OptimizeIROADContext) {
        let mut removed_index = Vec::<usize>::new();
        let mut ignore_ops: HashMap<isize, usize> = HashMap::new();
        for (ptr, entry) in ctx.local_var_ir_index.iter() {
            let mut init_ir = BfIR::SetValue(PtrOffset::None, 0);
            for (count, indexs) in entry {
                if indexs.len() < 1 {
                    continue;
                }
                let final_ir = indexs
                    .into_iter()
                    .try_fold(init_ir.clone(), |last_ir, index| {
                        let cur_ir =
                            &ir[*index - removed_index.iter().filter(|x| **x < *index).count()];
                        let merged_ir = last_ir.merge_op(cur_ir);
                        // 调试输出
                        println!("last: {last_ir:?}");
                        println!("cur: {cur_ir:?}");
                        println!("merged: {merged_ir:?}");
                        merged_ir
                    })
                    .unwrap();
                for index in indexs.iter().take(indexs.len() - 1) {
                    ir.remove(*index - removed_index.iter().filter(|x| **x < *index).count());
                    removed_index.push(*index);
                }
                let last_index = *indexs.last().unwrap();
                let cur_last_index =
                    last_index - removed_index.iter().filter(|x| **x < last_index).count();
                if final_ir == init_ir {
                    ir.remove(cur_last_index);
                    removed_index.push(last_index);
                    ignore_ops.entry(*ptr).insert_entry(*count);
                    break;
                }
                ir[cur_last_index] = final_ir.clone();
                init_ir = final_ir;
            }
        }
        // 消除死代码
        for (ptr, count) in ctx.local_var_refence_count.iter() {
            let Some(indexs) = ctx.local_var_ir_index[ptr].get(count) else {
                continue;
            };
            let Some(index) = indexs.last() else {
                continue;
            };
            if ignore_ops
                .get(ptr)
                .map(|v| *v == *count)
                .unwrap_or_default()
            {
                continue;
            }
            let offset = removed_index.iter().filter(|x| *x < index).count();
            println!("{ignore_ops:?} {:?}, {} {ptr}", ir, *index - offset);
            ir.remove(*index - offset);
            removed_index.push(*index);
        }
    }

    let mut ctx = Default::default();
    _ = _scan(ir, &mut ctx);
    // 调试输出
    println!("{ctx:?}");
    _opt(ir, &ctx);
}

mod error {
    use std::fmt::Formatter;

    #[derive(Debug)]
    pub enum OperationMergeError {
        CannotMerge { one: String, other_one: String },
        CannotMatchExpr,
    }

    impl std::fmt::Display for OperationMergeError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::CannotMerge {
                    one: a,
                    other_one: b,
                } => write!(f, "Cannot merge '{a}' and '{b}'"),
                Self::CannotMatchExpr => write!(f, "Cannot match expr"),
            }
        }
    }

    impl std::error::Error for OperationMergeError {}

    pub type OperationMergeResult<T> = Result<T, OperationMergeError>;
}
