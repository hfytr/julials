use std::{
    collections::BTreeMap,
    fmt::Debug,
    ops::{BitOr, BitOrAssign, Index, Not},
    rc::Rc,
};

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};

#[derive(Default, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct USizeSet(Vec<u64>);

impl Debug for USizeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("USizeSet")?;
        write!(f, "{:?}", self.iter().collect::<Vec<_>>())
    }
}

impl<'a> USizeSet {
    pub fn set(&mut self, i: usize, b: bool) {
        if i / 64 >= self.0.len() {
            self.0.resize(i / 64 + 1, 0);
        }
        self.0[i / 64] &= !(1 << (i % 64));
        self.0[i / 64] |= (b as u64) << (i % 64);
    }
    pub fn get(&self, i: usize) -> bool {
        if i / 64 >= self.0.len() {
            return false;
        }
        self.0[i / 64] & (1 << (i % 64)) > 0
    }
    pub fn iter(&'a self) -> USizeSetIterator<'a> {
        USizeSetIterator {
            ind: self.0.len() - 1,
            cur: *self.0.last().unwrap_or(&0),
            set: &self,
        }
    }
}

impl<const N: usize> From<[u64; N]> for USizeSet {
    fn from(value: [u64; N]) -> Self {
        Self(value.to_vec())
    }
}

impl From<&Vec<u64>> for USizeSet {
    fn from(value: &Vec<u64>) -> Self {
        Self(value.clone())
    }
}

impl FromIterator<usize> for USizeSet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut res = Self(vec![]);
        for x in iter {
            res.set(x, true);
        }
        res
    }
}

impl Not for USizeSet {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(self.0.iter().map(Not::not).collect())
    }
}

impl BitOrAssign<&USizeSet> for USizeSet {
    fn bitor_assign(&mut self, rhs: &Self) {
        let len = if rhs.0.len() > self.0.len() {
            let res = self.0.len();
            self.0.resize(rhs.0.len(), 0);
            res
        } else {
            rhs.0.len()
        };
        for i in 0..len {
            self.0[i] |= rhs.0[i];
        }
    }
}

impl BitOr for &USizeSet {
    type Output = USizeSet;
    fn bitor(self, rhs: Self) -> Self::Output {
        let mut res = self.clone();
        res |= &rhs;
        res
    }
}

impl quote::ToTokens for USizeSet {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut vec_inner = TokenStream::new();
        vec_inner.append_separated(self.0.iter(), Punct::new(',', Spacing::Alone));
        tokens.append_all(quote! {
            shared_structs::USizeSet::from([#vec_inner])
        });
    }
}

pub struct USizeSetIterator<'a> {
    ind: usize,
    cur: u64,
    set: &'a USizeSet,
}

impl<'a> Iterator for USizeSetIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur == 0 && self.ind == 0 {
            return None;
        } else if self.cur == 0 {
            self.ind -= 1;
            self.cur = self.set.0[self.ind];
        }
        let lsb = self.cur.trailing_zeros() as usize;
        self.cur ^= 1 << lsb;
        Some(lsb + self.ind * 64)
    }
}

pub struct IndexableSet<T>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    set: BTreeMap<Rc<T>, usize>,
    vec: Vec<Rc<T>>,
}

impl<T> Debug for IndexableSet<T>
where
    T: Debug + Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.vec)
    }
}

impl<T> IndexableSet<T>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    pub fn len(&self) -> usize {
        self.vec.len()
    }
    pub fn last(&self) -> Option<Rc<T>> {
        self.vec.last().map(Rc::clone)
    }
    pub fn get(&self, x: &T) -> Option<usize> {
        self.set.get(x).map(|x| *x)
    }
    /// @return (inserted elements index, true if the element is new)
    pub fn push(&mut self, t: T) -> (usize, bool) {
        if let Some(ind) = self.set.get(&t) {
            (*ind, false)
        } else {
            let rc = Rc::new(t);
            self.set.insert(Rc::clone(&rc), self.len());
            self.vec.push(rc);
            (self.vec.len() - 1, true)
        }
    }
}

impl<T> Index<usize> for IndexableSet<T>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    fn index(&self, index: usize) -> &Self::Output {
        &self.vec[index]
    }
    type Output = Rc<T>;
}

impl<T, const N: usize> From<[T; N]> for IndexableSet<T>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    fn from(items: [T; N]) -> Self {
        let items: Vec<_> = items.into_iter().map(Rc::new).collect();
        Self {
            set: BTreeMap::from_iter(items.iter().enumerate().map(|(i, rc)| (Rc::clone(rc), i))),
            vec: items,
        }
    }
}

impl<T> ToTokens for IndexableSet<T>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd + ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut arr_inner = TokenStream::new();
        arr_inner.append_separated(self.vec.iter(), Punct::new(',', Spacing::Alone));
        tokens.append_all(quote! {
            shared_structs::IndexableSet::from([#arr_inner])
        });
    }
}
