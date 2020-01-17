// An implementation of the dynamic programming algorithm for solving the
// longest common subsequence (LCS) problem.

use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub struct Lengths(Vec<u32>);

impl Deref for Lengths {
    type Target = Vec<u32>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Lengths {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Lengths {
    pub fn new<T>(a: &Vec<T>, b: &Vec<T>) -> Lengths
    where
        T: PartialEq,
    {
        let w = a.len() + 1;
        let h = b.len() + 1;

        // v[i, j] => v[w * j + i]
        let mut v = Vec::new();
        v.resize(w * h, 0);
        let mut m = Lengths(v);

        for j in 1..h {
            for i in 1..w {
                m[w * j + i] = if a[i - 1] == b[j - 1] {
                    1 + m[w * (j - 1) + i - 1]
                } else {
                    let left = m[w * j + i - 1];
                    let up = m[w * (j - 1) + i];
                    if left > up {
                        left
                    } else {
                        up
                    }
                };
            }
        }

        m
    }

    pub fn backtrack<T>(
        &self,
        a: &Vec<T>,
        b: &Vec<T>,
        i: usize,
        j: usize,
    ) -> Vec<(usize, usize)>
    where
        T: PartialEq,
    {
        if i == 0 || j == 0 {
            return vec![];
        }

        if a[i - 1] == b[j - 1] {
            let mut bt = self.backtrack(a, b, i - 1, j - 1);
            bt.push((i - 1, j - 1));
            return bt;
        }

        let w = a.len() + 1;
        if self[w * (j - 1) + i] > self[w * j + i - 1] {
            return self.backtrack(a, b, i, j - 1);
        }

        self.backtrack(a, b, i - 1, j)
    }
}

pub fn lcs<T>(a: &Vec<T>, b: &Vec<T>) -> Vec<(usize, usize)>
where
    T: PartialEq,
{
    let m = Lengths::new(a, b);
    m.backtrack(a, b, a.len(), b.len())
}

pub fn pick<T>(a: &Vec<T>, idx: Vec<(usize, usize)>) -> Vec<T>
where
    T: Clone,
{
    let mut res: Vec<T> = vec![];
    for (i, _) in idx {
        res.push(a[i].clone());
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lengths_1() {
        let m = Lengths::new(&vec![1, 2], &vec![1, 2]);
        assert_eq!(m.0, vec![0, 0, 0, 0, 1, 1, 0, 1, 2]);
    }

    #[test]
    fn test_lengths_2() {
        let m = Lengths::new(&vec![1, 3, 5], &vec![1, 2, 3, 4, 5]);
        assert_eq!(
            m.0,
            vec![
                0, 0, 0, 0,
                0, 1, 1, 1,
                0, 1, 1, 1,
                0, 1, 2, 2,
                0, 1, 2, 2,
                0, 1, 2, 3
            ]
        );
    }

    #[test]
    fn test_lengths_3() {
        let m = Lengths::new(&vec![1, 2, 3, 4, 5], &vec![1, 3, 5]);
        assert_eq!(
            m.0,
            vec![
                0, 0, 0, 0, 0, 0,
                0, 1, 1, 1, 1, 1,
                0, 1, 1, 2, 2, 2,
                0, 1, 1, 2, 2, 3
            ]
        );
    }

    #[test]
    fn test_backtrack_1() {
        let a = &vec![1, 3, 5, 7];
        let b = &vec![5, 3, 7];
        let m = Lengths::new(a, b);
        let idx = m.backtrack(a, b, a.len(), b.len());
        assert_eq!(idx, vec![(1, 1), (3, 2)]);
    }

    #[test]
    fn test_lcs_1() {
        let a = vec!['a', 'b', 'c'];
        let b = vec!['a', 'b', 'c'];
        assert_eq!(lcs(&a, &b), vec![(0, 0), (1, 1), (2, 2)]);
    }

    #[test]
    fn test_lcs_2() {
        let a = vec!['1', 'h', 'e', 'l', 'l', 'o', '3', '0'];
        let b = vec!['2', 'w', 'o', 'r', 'l', 'd', '4', '5', '6', '0'];
        assert_eq!(lcs(&a, &b), vec![(3, 4), (7, 9)]);
    }

    #[test]
    fn test_lcs_3() {
        let a = vec![1.0, 2.0, 3.0];
        let b = vec![5.0];
        assert_eq!(lcs(&a, &b), vec![]);
    }

    #[test]
    fn test_lcs_4() {
        let a = vec![1, 2, 3];
        let b = vec![];
        assert_eq!(lcs(&a, &b), vec![]);
    }

    #[test]
    fn test_lcs_5() {
        let a = vec![];
        let b = vec![1, 2, 3];
        assert_eq!(lcs(&a, &b), vec![]);
    }

    #[test]
    fn test_lcs_6() {
        let a: Vec<bool> = vec![];
        let b: Vec<bool> = vec![];
        assert_eq!(lcs(&a, &b), vec![]);
    }

    #[test]
    fn test_pick_1() {
        let a = vec![0, 1, 99, 8, 101];
        assert_eq!(pick(&a, vec![(1, 0), (0, 0), (3, 0)]), vec![1, 0, 8]);
    }
}
