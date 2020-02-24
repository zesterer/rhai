use std::any::TypeId;
use std::cmp::{PartialEq, PartialOrd};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Shl, Shr, Sub};
use std::{convert::TryInto, sync::Arc};

use crate::any::{Any, AnyExt};
use crate::call::FunArgs;
use crate::fn_register::{RegisterBoxFn, RegisterFn};
use crate::parser::{lex, parse, Expr, FnDef, ParseError, Stmt, AST};
use fmt::{Debug, Display};

type Array = Vec<Box<dyn Any>>;

#[derive(Debug, Clone)]
pub enum EvalAltResult {
    ErrorParseError(ParseError),
    ErrorFunctionNotFound(String),
    ErrorFunctionArgMismatch,
    ErrorArrayOutOfBounds(usize, i64),
    ErrorArrayMismatch,
    ErrorIndexMismatch,
    ErrorIfGuardMismatch,
    ErrorForMismatch,
    ErrorVariableNotFound(String),
    ErrorAssignmentToUnknownLHS,
    ErrorMismatchOutputType(String),
    ErrorCantOpenScriptFile(String),
    ErrorMalformedDotExpression,
    LoopBreak,
    Return(Box<dyn Any>),
}

impl EvalAltResult {
    fn as_str(&self) -> Option<&str> {
        Some(match *self {
            EvalAltResult::ErrorCantOpenScriptFile(ref s)
            | EvalAltResult::ErrorVariableNotFound(ref s)
            | EvalAltResult::ErrorFunctionNotFound(ref s)
            | EvalAltResult::ErrorMismatchOutputType(ref s) => s,
            _ => return None,
        })
    }
}

impl PartialEq for EvalAltResult {
    fn eq(&self, other: &Self) -> bool {
        use EvalAltResult::*;

        match (self, other) {
            (&ErrorParseError(ref a), &ErrorParseError(ref b)) => a == b,
            (&ErrorFunctionNotFound(ref a), &ErrorFunctionNotFound(ref b)) => a == b,
            (&ErrorFunctionArgMismatch, &ErrorFunctionArgMismatch) => true,
            (&ErrorIndexMismatch, &ErrorIndexMismatch) => true,
            (&ErrorArrayMismatch, &ErrorArrayMismatch) => true,
            (&ErrorArrayOutOfBounds(max1, index1), &ErrorArrayOutOfBounds(max2, index2)) => {
                max1 == max2 && index1 == index2
            }
            (&ErrorIfGuardMismatch, &ErrorIfGuardMismatch) => true,
            (&ErrorForMismatch, &ErrorForMismatch) => true,
            (&ErrorVariableNotFound(ref a), &ErrorVariableNotFound(ref b)) => a == b,
            (&ErrorAssignmentToUnknownLHS, &ErrorAssignmentToUnknownLHS) => true,
            (&ErrorMismatchOutputType(ref a), &ErrorMismatchOutputType(ref b)) => a == b,
            (&ErrorCantOpenScriptFile(ref a), &ErrorCantOpenScriptFile(ref b)) => a == b,
            (&ErrorMalformedDotExpression, &ErrorMalformedDotExpression) => true,
            (&LoopBreak, &LoopBreak) => true,
            _ => false,
        }
    }
}

impl Error for EvalAltResult {
    fn description(&self) -> &str {
        match *self {
            EvalAltResult::ErrorParseError(ref p) => p.description(),
            EvalAltResult::ErrorFunctionNotFound(_) => "Function not found",
            EvalAltResult::ErrorFunctionArgMismatch => "Function argument types do not match",
            EvalAltResult::ErrorIndexMismatch => "Array access expects integer index",
            EvalAltResult::ErrorArrayMismatch => "Indexing can only be performed on an array",
            EvalAltResult::ErrorArrayOutOfBounds(_, index) if index < 0 => {
                "Array access expects non-negative index"
            }
            EvalAltResult::ErrorArrayOutOfBounds(max, _) if max == 0 => "Access of empty array",
            EvalAltResult::ErrorArrayOutOfBounds(_, _) => "Array index out of bounds",
            EvalAltResult::ErrorIfGuardMismatch => "If guards expect boolean expression",
            EvalAltResult::ErrorForMismatch => "For loops expect array",
            EvalAltResult::ErrorVariableNotFound(_) => "Variable not found",
            EvalAltResult::ErrorAssignmentToUnknownLHS => {
                "Assignment to an unsupported left-hand side expression"
            }
            EvalAltResult::ErrorMismatchOutputType(_) => "Output type is incorrect",
            EvalAltResult::ErrorCantOpenScriptFile(_) => "Cannot open script file",
            EvalAltResult::ErrorMalformedDotExpression => "Malformed dot expression",
            EvalAltResult::LoopBreak => "[Not Error] Breaks out of loop",
            EvalAltResult::Return(_) => "[Not Error] Function returns value",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl fmt::Display for EvalAltResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = self.as_str() {
            write!(f, "{}: {}", self.description(), s)
        } else {
            match self {
                EvalAltResult::ErrorParseError(ref p) => write!(f, "Syntax error: {}", p),
                EvalAltResult::ErrorArrayOutOfBounds(_, index) if *index < 0 => {
                    write!(f, "{}: {} < 0", self.description(), index)
                }
                EvalAltResult::ErrorArrayOutOfBounds(max, _) if *max == 0 => {
                    write!(f, "{}", self.description())
                }
                EvalAltResult::ErrorArrayOutOfBounds(max, index) => {
                    write!(f, "{} (max {}): {}", self.description(), max - 1, index)
                }
                err => write!(f, "{}", err.description()),
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec {
    ident: String,
    args: Option<Vec<TypeId>>,
}

type IteratorFn = dyn Fn(&Box<dyn Any>) -> Box<dyn Iterator<Item = Box<dyn Any>>>;

/// Rhai's engine type. This is what you use to run Rhai scripts
///
/// ```rust
/// extern crate rhai;
/// use rhai::Engine;
///
/// fn main() {
///     let mut engine = Engine::new();
///
///     if let Ok(result) = engine.eval::<i64>("40 + 2") {
///         println!("Answer: {}", result);  // prints 42
///     }
/// }
/// ```
#[derive(Clone)]
pub struct Engine {
    /// A hashmap containing all functions known to the engine
    pub fns: HashMap<FnSpec, Arc<FnIntExt>>,
    pub type_iterators: HashMap<TypeId, Arc<IteratorFn>>,
}

pub enum FnIntExt {
    Ext(Box<FnAny>),
    Int(FnDef),
}

pub type FnAny = dyn Fn(Vec<&mut dyn Any>) -> Result<Box<dyn Any>, EvalAltResult>;

/// A type containing information about current scope.
/// Useful for keeping state between `Engine` runs
///
/// ```rust
/// use rhai::{Engine, Scope};
///
/// let mut engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// assert!(engine.eval_with_scope::<()>(&mut my_scope, "let x = 5;").is_ok());
/// assert_eq!(engine.eval_with_scope::<i64>(&mut my_scope, "x + 1").unwrap(), 6);
/// ```
///
/// Between runs, `Engine` only remembers functions when not using own `Scope`.
pub type Scope = Vec<(String, Box<dyn Any>)>;

impl Engine {
    pub fn call_fn<'a, I, A, T>(&self, ident: I, args: A) -> Result<T, EvalAltResult>
    where
        I: Into<String>,
        A: FunArgs<'a>,
        T: Any + Clone,
    {
        self.call_fn_raw(ident.into(), args.into_vec())
            .and_then(|b| {
                b.downcast()
                    .map(|b| *b)
                    .map_err(|a| EvalAltResult::ErrorMismatchOutputType((*a).type_name()))
            })
    }

    /// Universal method for calling functions, that are either
    /// registered with the `Engine` or written in Rhai
    pub fn call_fn_raw(
        &self,
        ident: String,
        args: Vec<&mut dyn Any>,
    ) -> Result<Box<dyn Any>, EvalAltResult> {
        debug_println!(
            "Trying to call function {:?} with args {:?}",
            ident,
            args.iter()
                .map(|x| Any::type_name(&**x))
                .collect::<Vec<_>>()
        );

        let spec = FnSpec {
            ident: ident.clone(),
            args: Some(args.iter().map(|a| Any::type_id(&**a)).collect()),
        };

        self.fns
            .get(&spec)
            .or_else(|| {
                let spec1 = FnSpec {
                    ident: ident.clone(),
                    args: None,
                };
                self.fns.get(&spec1)
            })
            .ok_or_else(|| {
                let typenames = args
                    .iter()
                    .map(|x| (*(&**x).box_clone()).type_name())
                    .collect::<Vec<_>>();
                EvalAltResult::ErrorFunctionNotFound(format!(
                    "{} ({})",
                    ident,
                    typenames.join(", ")
                ))
            })
            .and_then(move |f| match **f {
                FnIntExt::Ext(ref f) => f(args),
                FnIntExt::Int(ref f) => {
                    let mut scope = Scope::new();
                    scope.extend(
                        f.params
                            .iter()
                            .cloned()
                            .zip(args.iter().map(|x| (&**x).box_clone())),
                    );

                    match self.eval_stmt(&mut scope, &*f.body) {
                        Err(EvalAltResult::Return(x)) => Ok(x),
                        other => other,
                    }
                }
            })
    }

    pub fn register_fn_raw(&mut self, ident: String, args: Option<Vec<TypeId>>, f: Box<FnAny>) {
        debug_println!("Register; {:?} with args {:?}", ident, args);

        let spec = FnSpec { ident, args };

        self.fns.insert(spec, Arc::new(FnIntExt::Ext(f)));
    }

    /// Register a type for use with Engine. Keep in mind that
    /// your type must implement Clone.
    pub fn register_type<T: Any>(&mut self) {
        // currently a no-op, exists for future extensibility
    }

    /// Register an iterator adapter for a type.
    pub fn register_iterator<T: Any, F>(&mut self, f: F)
    where
        F: 'static + Fn(&Box<dyn Any>) -> Box<dyn Iterator<Item = Box<dyn Any>>>,
    {
        self.type_iterators.insert(TypeId::of::<T>(), Arc::new(f));
    }

    /// Register a get function for a member of a registered type
    pub fn register_get<T: Clone + Any, U: Clone + Any, F>(&mut self, name: &str, get_fn: F)
    where
        F: 'static + Fn(&mut T) -> U,
    {
        let get_name = "get$".to_string() + name;
        self.register_fn(&get_name, get_fn);
    }

    /// Register a set function for a member of a registered type
    pub fn register_set<T: Clone + Any, U: Clone + Any, F>(&mut self, name: &str, set_fn: F)
    where
        F: 'static + Fn(&mut T, U) -> (),
    {
        let set_name = "set$".to_string() + name;
        self.register_fn(&set_name, set_fn);
    }

    /// Shorthand for registering both getters and setters
    pub fn register_get_set<T: Clone + Any, U: Clone + Any, F, G>(
        &mut self,
        name: &str,
        get_fn: F,
        set_fn: G,
    ) where
        F: 'static + Fn(&mut T) -> U,
        G: 'static + Fn(&mut T, U) -> (),
    {
        self.register_get(name, get_fn);
        self.register_set(name, set_fn);
    }

    fn get_dot_val_helper(
        &self,
        scope: &mut Scope,
        this_ptr: &mut dyn Any,
        dot_rhs: &Expr,
    ) -> Result<Box<dyn Any>, EvalAltResult> {
        use std::iter::once;

        match *dot_rhs {
            Expr::FunctionCall(ref fn_name, ref args) => {
                let mut args: Array = args
                    .iter()
                    .map(|arg| self.eval_expr(scope, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let args = once(this_ptr)
                    .chain(args.iter_mut().map(|b| b.as_mut()))
                    .collect();

                self.call_fn_raw(fn_name.to_owned(), args)
            }
            Expr::Identifier(ref id) => {
                let get_fn_name = "get$".to_string() + id;

                self.call_fn_raw(get_fn_name, vec![this_ptr])
            }
            Expr::Index(ref id, ref idx_raw) => {
                let idx = self.eval_expr(scope, idx_raw)?;
                let get_fn_name = "get$".to_string() + id;

                let mut val = self.call_fn_raw(get_fn_name, vec![this_ptr])?;

                ((*val).downcast_mut() as Option<&mut Array>)
                    .ok_or(EvalAltResult::ErrorArrayMismatch)
                    .and_then(|arr| {
                        idx.downcast_ref::<i64>()
                            .map(|idx| (arr, *idx))
                            .ok_or(EvalAltResult::ErrorIndexMismatch)
                    })
                    .and_then(|(arr, idx)| match idx {
                        x if x < 0 => Err(EvalAltResult::ErrorArrayOutOfBounds(0, x)),
                        x => arr
                            .get(x as usize)
                            .cloned()
                            .ok_or(EvalAltResult::ErrorArrayOutOfBounds(arr.len(), x)),
                    })
            }
            Expr::Dot(ref inner_lhs, ref inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    self.call_fn_raw(get_fn_name, vec![this_ptr])
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), inner_rhs))
                }
                _ => Err(EvalAltResult::ErrorMalformedDotExpression),
            },
            _ => Err(EvalAltResult::ErrorMalformedDotExpression),
        }
    }

    fn search_scope<'a, F, T>(
        scope: &'a mut Scope,
        id: &str,
        map: F,
    ) -> Result<(usize, T), EvalAltResult>
    where
        F: FnOnce(&'a mut dyn Any) -> Result<T, EvalAltResult>,
    {
        scope
            .iter_mut()
            .enumerate()
            .rev()
            .find(|&(_, &mut (ref name, _))| *id == *name)
            .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.to_owned()))
            .and_then(move |(idx, &mut (_, ref mut val))| map(val.as_mut()).map(|val| (idx, val)))
    }

    fn array_value(
        &self,
        scope: &mut Scope,
        id: &str,
        idx: &Expr,
    ) -> Result<(usize, usize, Box<dyn Any>), EvalAltResult> {
        let idx_boxed = self
            .eval_expr(scope, idx)?
            .downcast::<i64>()
            .map_err(|_| EvalAltResult::ErrorIndexMismatch)?;
        let idx_raw = *idx_boxed;
        let idx = match idx_raw {
            x if x < 0 => return Err(EvalAltResult::ErrorArrayOutOfBounds(0, x)),
            x => x as usize,
        };
        let (idx_sc, val) = Self::search_scope(scope, id, |val| {
            ((*val).downcast_mut() as Option<&mut Array>)
                .ok_or(EvalAltResult::ErrorArrayMismatch)
                .and_then(|arr| {
                    arr.get(idx)
                        .cloned()
                        .ok_or(EvalAltResult::ErrorArrayOutOfBounds(arr.len(), idx_raw))
                })
        })?;

        Ok((idx_sc, idx, val))
    }

    fn get_dot_val(
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
    ) -> Result<Box<dyn Any>, EvalAltResult> {
        match *dot_lhs {
            Expr::Identifier(ref id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.box_clone()))?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(ref id, ref idx_raw) => {
                let (sc_idx, idx, mut target) = self.array_value(scope, id, idx_raw)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1.downcast_mut::<Array>().unwrap()[idx] = target;

                value
            }
            _ => Err(EvalAltResult::ErrorMalformedDotExpression),
        }
    }

    fn set_dot_val_helper(
        &self,
        this_ptr: &mut dyn Any,
        dot_rhs: &Expr,
        mut source_val: Box<dyn Any>,
    ) -> Result<Box<dyn Any>, EvalAltResult> {
        match *dot_rhs {
            Expr::Identifier(ref id) => {
                let set_fn_name = "set$".to_string() + id;
                self.call_fn_raw(set_fn_name, vec![this_ptr, source_val.as_mut()])
            }
            Expr::Dot(ref inner_lhs, ref inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    self.call_fn_raw(get_fn_name, vec![this_ptr])
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), inner_rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = "set$".to_string() + id;

                            self.call_fn_raw(set_fn_name, vec![this_ptr, v.as_mut()])
                        })
                }
                _ => Err(EvalAltResult::ErrorMalformedDotExpression),
            },
            _ => Err(EvalAltResult::ErrorMalformedDotExpression),
        }
    }

    fn set_dot_val(
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        source_val: Box<dyn Any>,
    ) -> Result<Box<dyn Any>, EvalAltResult> {
        match *dot_lhs {
            Expr::Identifier(ref id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.box_clone()))?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(ref id, ref idx_raw) => {
                let (sc_idx, idx, mut target) = self.array_value(scope, id, idx_raw)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1.downcast_mut::<Array>().unwrap()[idx] = target;

                value
            }
            _ => Err(EvalAltResult::ErrorMalformedDotExpression),
        }
    }

    fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Box<dyn Any>, EvalAltResult> {
        match *expr {
            Expr::IntegerConstant(i) => Ok(Box::new(i)),
            Expr::FloatConstant(i) => Ok(Box::new(i)),
            Expr::StringConstant(ref s) => Ok(Box::new(s.clone())),
            Expr::CharConstant(ref c) => Ok(Box::new(*c)),
            Expr::Identifier(ref id) => {
                for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                    if *id == *name {
                        return Ok(val.clone());
                    }
                }
                Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
            }
            Expr::Index(ref id, ref idx_raw) => {
                self.array_value(scope, id, idx_raw).map(|(_, _, x)| x)
            }
            Expr::Assignment(ref id, ref rhs) => {
                let rhs_val = self.eval_expr(scope, rhs)?;

                match **id {
                    Expr::Identifier(ref n) => {
                        for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                            if *n == *name {
                                *val = rhs_val;

                                return Ok(Box::new(()));
                            }
                        }
                        Err(EvalAltResult::ErrorVariableNotFound(n.clone()))
                    }
                    Expr::Index(ref id, ref idx_raw) => {
                        let idx = self.eval_expr(scope, idx_raw)?;

                        for &mut (ref name, ref mut val) in &mut scope.iter_mut().rev() {
                            if *id == *name {
                                return if let Some(&i) = idx.downcast_ref::<i64>() {
                                    if let Some(arr_typed) =
                                        (*val).downcast_mut() as Option<&mut Array>
                                    {
                                        if i < 0 {
                                            Err(EvalAltResult::ErrorArrayOutOfBounds(0, i))
                                        } else if i as usize >= arr_typed.len() {
                                            Err(EvalAltResult::ErrorArrayOutOfBounds(
                                                arr_typed.len(),
                                                i,
                                            ))
                                        } else {
                                            arr_typed[i as usize] = rhs_val;
                                            Ok(Box::new(()))
                                        }
                                    } else {
                                        Err(EvalAltResult::ErrorIndexMismatch)
                                    }
                                } else {
                                    Err(EvalAltResult::ErrorIndexMismatch)
                                };
                            }
                        }

                        Err(EvalAltResult::ErrorVariableNotFound(id.clone()))
                    }
                    Expr::Dot(ref dot_lhs, ref dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }
                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS),
                }
            }
            Expr::Dot(ref lhs, ref rhs) => self.get_dot_val(scope, lhs, rhs),
            Expr::Array(ref contents) => {
                let mut arr = Vec::new();

                for item in &(*contents) {
                    let arg = self.eval_expr(scope, item)?;
                    arr.push(arg);
                }

                Ok(Box::new(arr))
            }
            Expr::FunctionCall(ref fn_name, ref args) => self.call_fn_raw(
                fn_name.to_owned(),
                args.iter()
                    .map(|ex| self.eval_expr(scope, ex))
                    .collect::<Result<Array, _>>()?
                    .iter_mut()
                    .map(|b| b.as_mut())
                    .collect(),
            ),
            Expr::True => Ok(Box::new(true)),
            Expr::False => Ok(Box::new(false)),
            Expr::Unit => Ok(Box::new(())),
        }
    }

    fn eval_stmt(&self, scope: &mut Scope, stmt: &Stmt) -> Result<Box<dyn Any>, EvalAltResult> {
        match *stmt {
            Stmt::Expr(ref e) => self.eval_expr(scope, e),
            Stmt::Block(ref b) => {
                let prev_len = scope.len();
                let mut last_result: Result<Box<dyn Any>, EvalAltResult> = Ok(Box::new(()));

                for s in b.iter() {
                    last_result = self.eval_stmt(scope, s);
                    if let Err(x) = last_result {
                        last_result = Err(x);
                        break;
                    }
                }

                while scope.len() > prev_len {
                    scope.pop();
                }

                last_result
            }
            Stmt::If(ref guard, ref body) => {
                let guard_result = self.eval_expr(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        } else {
                            Ok(Box::new(()))
                        }
                    }
                    Err(_) => Err(EvalAltResult::ErrorIfGuardMismatch),
                }
            }
            Stmt::IfElse(ref guard, ref body, ref else_body) => {
                let guard_result = self.eval_expr(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            self.eval_stmt(scope, body)
                        } else {
                            self.eval_stmt(scope, else_body)
                        }
                    }
                    Err(_) => Err(EvalAltResult::ErrorIfGuardMismatch),
                }
            }
            Stmt::While(ref guard, ref body) => loop {
                let guard_result = self.eval_expr(scope, guard)?;
                match guard_result.downcast::<bool>() {
                    Ok(g) => {
                        if *g {
                            match self.eval_stmt(scope, body) {
                                Err(EvalAltResult::LoopBreak) => return Ok(Box::new(())),
                                Err(x) => return Err(x),
                                _ => (),
                            }
                        } else {
                            return Ok(Box::new(()));
                        }
                    }
                    Err(_) => return Err(EvalAltResult::ErrorIfGuardMismatch),
                }
            },
            Stmt::Loop(ref body) => loop {
                match self.eval_stmt(scope, body) {
                    Err(EvalAltResult::LoopBreak) => return Ok(Box::new(())),
                    Err(x) => return Err(x),
                    _ => (),
                }
            },
            Stmt::For(ref name, ref expr, ref body) => {
                let arr = self.eval_expr(scope, expr)?;
                let tid = Any::type_id(&*arr);
                if let Some(iter_fn) = self.type_iterators.get(&tid) {
                    scope.push((name.clone(), Box::new(())));
                    let idx = scope.len() - 1;
                    for a in iter_fn(&arr) {
                        scope[idx].1 = a;
                        match self.eval_stmt(scope, body) {
                            Err(EvalAltResult::LoopBreak) => break,
                            Err(x) => return Err(x),
                            _ => (),
                        }
                    }
                    scope.remove(idx);
                    Ok(Box::new(()))
                } else {
                    return Err(EvalAltResult::ErrorForMismatch);
                }
            }
            Stmt::Break => Err(EvalAltResult::LoopBreak),
            Stmt::Return => Err(EvalAltResult::Return(Box::new(()))),
            Stmt::ReturnWithVal(ref a) => {
                let result = self.eval_expr(scope, a)?;
                Err(EvalAltResult::Return(result))
            }
            Stmt::Let(ref name, ref init) => {
                match *init {
                    Some(ref v) => {
                        let i = self.eval_expr(scope, v)?;
                        scope.push((name.clone(), i));
                    }
                    None => scope.push((name.clone(), Box::new(()))),
                };
                Ok(Box::new(()))
            }
        }
    }

    /// Compile a string into an AST
    pub fn compile(input: &str) -> Result<AST, ParseError> {
        let tokens = lex(input);

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        tree
    }

    /// Compile a file into an AST
    pub fn compile_file(fname: &str) -> Result<AST, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(fname) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                Self::compile(&contents).map_err(|err| EvalAltResult::ErrorParseError(err))
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
        }
    }

    /// Evaluate a file
    pub fn eval_file<T: Any + Clone>(&mut self, fname: &str) -> Result<T, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(fname) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                self.eval::<T>(&contents)
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
        }
    }

    /// Evaluate a string
    pub fn eval<T: Any + Clone>(&mut self, input: &str) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_with_scope(&mut scope, input)
    }

    /// Evaluate a string with own scope
    pub fn eval_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        input: &str,
    ) -> Result<T, EvalAltResult> {
        let ast = Self::compile(input).map_err(|err| EvalAltResult::ErrorParseError(err))?;
        self.eval_ast_with_scope(scope, &ast)
    }

    /// Evaluate an AST
    pub fn eval_ast<T: Any + Clone>(&mut self, ast: &AST) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_ast_with_scope(&mut scope, ast)
    }

    /// Evaluate an AST with own scope
    pub fn eval_ast_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        ast: &AST,
    ) -> Result<T, EvalAltResult> {
        let AST(os, fns) = ast;
        let mut x: Result<Box<dyn Any>, EvalAltResult> = Ok(Box::new(()));

        for f in fns {
            let name = f.name.clone();
            let local_f = f.clone();

            let spec = FnSpec {
                ident: name,
                args: None,
            };

            self.fns.insert(spec, Arc::new(FnIntExt::Int(local_f)));
        }

        for o in os {
            x = match self.eval_stmt(scope, o) {
                Ok(v) => Ok(v),
                Err(e) => return Err(e),
            }
        }

        let x = x?;

        match x.downcast::<T>() {
            Ok(out) => Ok(*out),
            Err(a) => Err(EvalAltResult::ErrorMismatchOutputType((*a).type_name())),
        }
    }

    /// Evaluate a file, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_file(&mut self, fname: &str) -> Result<(), EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(fname) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                if let e @ Err(_) = self.consume(&contents) {
                    e
                } else {
                    Ok(())
                }
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(fname.to_owned()))
        }
    }

    /// Evaluate a string, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume(&mut self, input: &str) -> Result<(), EvalAltResult> {
        self.consume_with_scope(&mut Scope::new(), input)
    }

    /// Evaluate a string with own scope, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_with_scope(
        &mut self,
        scope: &mut Scope,
        input: &str,
    ) -> Result<(), EvalAltResult> {
        let tokens = lex(input);

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        match tree {
            Ok(AST(ref os, ref fns)) => {
                for f in fns {
                    if f.params.len() > 6 {
                        return Ok(());
                    }
                    let name = f.name.clone();
                    let local_f = f.clone();

                    let spec = FnSpec {
                        ident: name,
                        args: None,
                    };

                    self.fns.insert(spec, Arc::new(FnIntExt::Int(local_f)));
                }

                for o in os {
                    if let Err(e) = self.eval_stmt(scope, o) {
                        return Err(e);
                    }
                }

                Ok(())
            }
            Err(_) => Err(EvalAltResult::ErrorFunctionArgMismatch),
        }
    }

    /// Register the default library. That means, numeric types, char, bool
    /// String, arithmetics and string concatenations.
    pub fn register_default_lib(engine: &mut Engine) {
        macro_rules! reg_op {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(x: $y, y: $y)->$y);
                )*
            )
        }

        macro_rules! reg_un {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(x: $y)->$y);
                )*
            )
        }

        macro_rules! reg_cmp {
            ($engine:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(x: $y, y: $y)->bool);
                )*
            )
        }

        macro_rules! reg_func1 {
            ($engine:expr, $x:expr, $op:expr, $r:ty, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(x: $y)->$r);
                )*
            )
        }

        macro_rules! reg_func2x {
            ($engine:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(x: $v, y: $y)->$r);
                )*
            )
        }

        macro_rules! reg_func2y {
            ($engine:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
                $(
                    $engine.register_fn($x, $op as fn(y: $y, x: $v)->$r);
                )*
            )
        }

        fn add<T: Add>(x: T, y: T) -> <T as Add>::Output {
            x + y
        }
        fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
            x - y
        }
        fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
            x * y
        }
        fn div<T: Div>(x: T, y: T) -> <T as Div>::Output {
            x / y
        }
        fn neg<T: Neg>(x: T) -> <T as Neg>::Output {
            -x
        }
        fn lt<T: PartialOrd>(x: T, y: T) -> bool {
            x < y
        }
        fn lte<T: PartialOrd>(x: T, y: T) -> bool {
            x <= y
        }
        fn gt<T: PartialOrd>(x: T, y: T) -> bool {
            x > y
        }
        fn gte<T: PartialOrd>(x: T, y: T) -> bool {
            x >= y
        }
        fn eq<T: PartialEq>(x: T, y: T) -> bool {
            x == y
        }
        fn ne<T: PartialEq>(x: T, y: T) -> bool {
            x != y
        }
        fn and(x: bool, y: bool) -> bool {
            x && y
        }
        fn or(x: bool, y: bool) -> bool {
            x || y
        }
        fn not(x: bool) -> bool {
            !x
        }
        fn concat(x: String, y: String) -> String {
            x + &y
        }
        fn binary_and<T: BitAnd>(x: T, y: T) -> <T as BitAnd>::Output {
            x & y
        }
        fn binary_or<T: BitOr>(x: T, y: T) -> <T as BitOr>::Output {
            x | y
        }
        fn binary_xor<T: BitXor>(x: T, y: T) -> <T as BitXor>::Output {
            x ^ y
        }
        fn left_shift<T: Shl<T>>(x: T, y: T) -> <T as Shl<T>>::Output {
            x.shl(y)
        }
        fn right_shift<T: Shr<T>>(x: T, y: T) -> <T as Shr<T>>::Output {
            x.shr(y)
        }
        fn modulo<T: Rem<T>>(x: T, y: T) -> <T as Rem<T>>::Output {
            x % y
        }
        fn pow_i64_i64(x: i64, y: i64) -> i64 {
            x.pow(y as u32)
        }
        fn pow_f64_f64(x: f64, y: f64) -> f64 {
            x.powf(y)
        }
        fn pow_f64_i64(x: f64, y: i64) -> f64 {
            x.powi(y as i32)
        }
        fn unit_eq(_a: (), _b: ()) -> bool {
            true
        }

        reg_op!(engine, "+", add, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "-", sub, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "*", mul, i32, i64, u32, u64, f32, f64);
        reg_op!(engine, "/", div, i32, i64, u32, u64, f32, f64);

        reg_cmp!(engine, "<", lt, i32, i64, u32, u64, String, f64);
        reg_cmp!(engine, "<=", lte, i32, i64, u32, u64, String, f64);
        reg_cmp!(engine, ">", gt, i32, i64, u32, u64, String, f64);
        reg_cmp!(engine, ">=", gte, i32, i64, u32, u64, String, f64);
        reg_cmp!(engine, "==", eq, i32, i64, u32, u64, bool, String, f64);
        reg_cmp!(engine, "!=", ne, i32, i64, u32, u64, bool, String, f64);

        reg_op!(engine, "||", or, bool);
        reg_op!(engine, "&&", and, bool);
        reg_op!(engine, "|", binary_or, i32, i64, u32, u64);
        reg_op!(engine, "|", or, bool);
        reg_op!(engine, "&", binary_and, i32, i64, u32, u64);
        reg_op!(engine, "&", and, bool);
        reg_op!(engine, "^", binary_xor, i32, i64, u32, u64);
        reg_op!(engine, "<<", left_shift, i32, i64, u32, u64);
        reg_op!(engine, ">>", right_shift, i32, i64, u32, u64);
        reg_op!(engine, "%", modulo, i32, i64, u32, u64);
        engine.register_fn("~", pow_i64_i64);
        engine.register_fn("~", pow_f64_f64);
        engine.register_fn("~", pow_f64_i64);

        reg_un!(engine, "-", neg, i32, i64, f32, f64);
        reg_un!(engine, "!", not, bool);

        engine.register_fn("+", concat);
        engine.register_fn("==", unit_eq);

        // engine.register_fn("[]", idx);
        // FIXME?  Registering array lookups are a special case because we want to return boxes
        // directly let ent = engine.fns.entry("[]".to_string()).or_insert_with(Vec::new);
        // (*ent).push(FnType::ExternalFn2(Box::new(idx)));

        // Register print and debug
        fn print_debug<T: Debug>(x: T) {
            println!("{:?}", x);
        }
        fn print<T: Display>(x: T) {
            println!("{}", x);
        }

        reg_func1!(engine, "print", print, (), i32, i64, u32, u64);
        reg_func1!(engine, "print", print, (), f32, f64, bool, String);
        reg_func1!(engine, "print", print_debug, (), Array);
        engine.register_fn("print", |_: ()| println!());

        reg_func1!(engine, "debug", print_debug, (), i32, i64, u32, u64);
        reg_func1!(engine, "debug", print_debug, (), f32, f64, bool, String);
        reg_func1!(engine, "debug", print_debug, (), Array, ());

        // Register array functions
        fn push<T: Any + 'static>(list: &mut Array, item: T) {
            list.push(Box::new(item));
        }

        reg_func2x!(engine, "push", push, &mut Array, (), i32, i64, u32, u64);
        reg_func2x!(engine, "push", push, &mut Array, (), f32, f64, bool);
        reg_func2x!(engine, "push", push, &mut Array, (), String, Array, ());

        engine.register_box_fn("pop", |list: &mut Array| list.pop().unwrap());
        engine.register_box_fn("shift", |list: &mut Array| list.remove(0));
        engine.register_fn("len", |list: &mut Array| -> i64 {
            list.len().try_into().unwrap()
        });

        // Register string concatenate functions
        fn prepend<T: Display>(x: T, y: String) -> String {
            format!("{}{}", x, y)
        }
        fn append<T: Display>(x: String, y: T) -> String {
            format!("{}{}", x, y)
        }

        reg_func2x!(engine, "+", append, String, String, i32, i64, u32, u64, f32, f64, bool);
        engine.register_fn("+", |x: String, y: Array| format!("{}{:?}", x, y));
        engine.register_fn("+", |x: String, _: ()| format!("{}", x));

        reg_func2y!(engine, "+", prepend, String, String, i32, i64, u32, u64, f32, f64, bool);
        engine.register_fn("+", |x: Array, y: String| format!("{:?}{}", x, y));
        engine.register_fn("+", |_: (), y: String| format!("{}", y));

        // Register array iterator
        engine.register_iterator::<Array, _>(|a| {
            Box::new(a.downcast_ref::<Array>().unwrap().clone().into_iter())
        });

        // Register range function
        use std::ops::Range;
        engine.register_iterator::<Range<i64>, _>(|a| {
            Box::new(
                a.downcast_ref::<Range<i64>>()
                    .unwrap()
                    .clone()
                    .map(|n| Box::new(n) as Box<dyn Any>),
            )
        });

        engine.register_fn("range", |i1: i64, i2: i64| (i1..i2));
    }

    /// Make a new engine
    pub fn new() -> Engine {
        let mut engine = Engine {
            fns: HashMap::new(),
            type_iterators: HashMap::new(),
        };

        Engine::register_default_lib(&mut engine);

        engine
    }
}
