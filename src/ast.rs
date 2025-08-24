use std::ops::{Index, IndexMut};

use crate::parser::NodeKind;

#[derive(Debug, Clone)]
pub enum NodeData {
    IntLit(i128),
    UIntLit(u128),
    FloatLit(f64),
    String(String),
    VarKids(Vec<usize>),
    None,
    Error,
}

#[derive(Clone, Debug)]
pub struct Span {
    file: usize,
    start: (usize, usize),
    end: (usize, usize),
}

impl Span {
    pub fn new(file: usize, start: (usize, usize), end: (usize, usize)) -> Self {
        Self { file, start, end }
    }

    pub fn merge(&self, rhs: &Self) -> Self {
        assert_eq!(self.file, rhs.file);
        Self {
            file: self.file,
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
        }
    }

    pub fn merge_with(&mut self, rhs: &Self) {
        self.start = self.start.min(rhs.start);
        self.end = self.end.max(rhs.end);
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub data: NodeData,
}

#[derive(Debug, Default)]
pub struct AST {
    arena: Vec<Node>,
    files: Vec<String>,
}

impl AST {
    pub fn push(&mut self, new: Node) -> usize {
        self.arena.push(new);
        self.arena.len() - 1
    }

    pub fn get_mut<const N: usize>(&mut self, idxs: [usize; N]) -> [&mut Node; N] {
        self.arena
            .get_disjoint_mut(idxs)
            .expect("Overlapping indices in call to ast.get_mut()")
    }

    pub fn add_file(&mut self, fname: String) {
        self.files.push(fname);
    }
}

impl Index<usize> for AST {
    type Output = Node;
    fn index(&self, i: usize) -> &Self::Output {
        &self.arena[i]
    }
}

impl IndexMut<usize> for AST {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.arena[i]
    }
}
