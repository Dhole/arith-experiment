use auto_ops::*;

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Mutex;
use std::sync::OnceLock;

fn expr_store() -> &'static Mutex<Vec<Expr>> {
    static STORE: OnceLock<Mutex<Vec<Expr>>> = OnceLock::new();
    STORE.get_or_init(|| Mutex::new(vec![]))
}

#[derive(Clone, Copy, Debug)]
pub enum ExprRef {
    Const(isize),
    BigConst(usize),
    Var(usize),
    Expr(usize),
}

struct Context<F: Display, V: Var> {
    id: usize,
    consts: Vec<F>,
    vars: Vec<V>,
}

impl<F: Display, V: Var> Context<F, V> {
    fn new() -> Self {
        Self {
            id: 0,
            consts: Vec::new(),
            vars: Vec::new(),
        }
    }
    fn new_var(&mut self, v: V) -> ExprRef {
        let var_id = self.vars.len();
        self.vars.push(v);
        ExprRef::Var(var_id)
    }
    fn zero(&mut self) -> ExprRef {
        ExprRef::Const(0)
    }
    fn new_big_const(&mut self, c: F) -> ExprRef {
        let const_id = self.consts.len();
        self.consts.push(c);
        ExprRef::BigConst(const_id)
    }
    fn _print_expr_ref(&self, store: &Vec<Expr>, e: ExprRef) {
        match e {
            ExprRef::Const(c) => print!("{}", c),
            ExprRef::BigConst(const_id) => print!("{}", self.consts[const_id]),
            ExprRef::Var(var_id) => print!("{}", self.vars[var_id]),
            ExprRef::Expr(expr_id) => match store[expr_id] {
                Expr::Sum(a, b) => {
                    print!("(");
                    self._print_expr_ref(store, a);
                    print!(" + ");
                    self._print_expr_ref(store, b);
                    print!(")");
                }
                Expr::Prod(a, b) => {
                    print!("(");
                    self._print_expr_ref(store, a);
                    print!(" * ");
                    self._print_expr_ref(store, b);
                    print!(")");
                }
                Expr::Neg(a) => {
                    print!("-(");
                    self._print_expr_ref(store, a);
                    print!(")");
                }
            },
        }
    }
    fn print_expr_ref(&self, e: ExprRef) {
        let store = expr_store().lock().unwrap();
        self._print_expr_ref(&store, e);
        println!();
    }
}

pub trait Var: Clone + Debug + PartialEq + Eq + Hash + Ord + Display {}
impl Var for &'static str {}
impl Var for String {}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    Sum(ExprRef, ExprRef),
    Prod(ExprRef, ExprRef),
    Neg(ExprRef),
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum Expr<F, V: Var> {
//     Const(F),
//     Var(V),
//     Sum(Vec<Expr<F, V>>),
//     Mul(Vec<Expr<F, V>>),
//     Neg(Box<Expr<F, V>>),
//     Pow(Box<Expr<F, V>>, u32),
// }

impl_op_ex!(+|a: &ExprRef, b: &ExprRef| -> ExprRef {
    let expr = Expr::Sum(*a, *b);
    let expr_id = {
        let mut store = expr_store().lock().unwrap();
        let expr_id = store.len();
        store.push(expr);
        expr_id
    };
    ExprRef::Expr(expr_id)
});
impl_op_ex_commutative!(+|a: &ExprRef, b: isize| -> ExprRef {
    *a + ExprRef::Const(b)
});
impl_op_ex!(*|a: &ExprRef, b: &ExprRef| -> ExprRef {
    let expr = Expr::Prod(*a, *b);
    let expr_id = {
        let mut store = expr_store().lock().unwrap();
        let expr_id = store.len();
        store.push(expr);
        expr_id
    };
    ExprRef::Expr(expr_id)
});
impl_op_ex_commutative!(*|a: &ExprRef, b: isize| -> ExprRef { *a * ExprRef::Const(b) });
impl_op_ex!(-|a: &ExprRef| -> ExprRef {
    let expr = Expr::Neg(*a);
    let expr_id = {
        let mut store = expr_store().lock().unwrap();
        let expr_id = store.len();
        store.push(expr);
        expr_id
    };
    ExprRef::Expr(expr_id)
});
impl_op_ex!(-|a: isize, b: &ExprRef| -> ExprRef { ExprRef::Const(a) - *b });
impl_op_ex!(-|a: &ExprRef, b: &ExprRef| -> ExprRef {
    let expr_neg_b = -*b;
    *a + expr_neg_b
});

fn main() {
    let mut ctx: Context<usize, &'static str> = Context::new();
    let a = ctx.new_var("a");
    let b = ctx.new_var("b");
    let c = a + b + 1;
    let d = 1 - a + c * 2;
    println!("{:?}", c);
    println!("{:?}", d);
    ctx.print_expr_ref(c);
    ctx.print_expr_ref(d);
}
