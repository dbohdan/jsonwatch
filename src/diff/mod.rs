use crate::lcs;
use serde_json::{map::Map, Value};

use std::{
    cmp::{max, min},
    collections::HashSet,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Debug, PartialEq)]
pub enum Op {
    Added(JsonPath, Value),
    Changed(JsonPath, Value, Value),
    Removed(JsonPath, Value),
}

pub type JsonPath = String;
pub type JsonDiffContents = Vec<Op>;

#[derive(Debug)]
pub struct JsonDiff(JsonDiffContents);

impl fmt::Display for JsonDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for op in self.iter() {
            match op {
                Op::Added(path, value) => {
                    writeln!(f, "+ {}: {}", path, value).unwrap();
                }
                Op::Changed(path, from_value, to_value) => {
                    writeln!(f, "{}: {} -> {}", path, from_value, to_value)
                        .unwrap();
                }
                Op::Removed(path, value) => {
                    writeln!(f, "- {}: {}", path, value).unwrap();
                }
            }
        }

        fmt::Result::Ok(())
    }
}

impl Deref for JsonDiff {
    type Target = JsonDiffContents;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for JsonDiff {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub fn diff(prev: &Option<Value>, current: &Option<Value>) -> JsonDiff {
    let mut diff = JsonDiff(Vec::new());
    let prefix: JsonPath = "".to_string();

    match (prev, current) {
        (None, None) => {}
        (Some(v), None) => {
            diff_helper(&mut diff, &prefix, &v, &Value::Null);
            // Remove the placeholder Null.
            match &diff[..] {
                [Op::Changed(p, v, Value::Null)] => {
                    diff[0] = Op::Removed(p.to_string(), v.clone());
                }
                _ => {
                    diff.pop();
                }
            }
        }
        (None, Some(v)) => {
            diff_helper(&mut diff, &prefix, &Value::Null, &v);
            match &diff[..] {
                [Op::Changed(p, Value::Null, v)] => {
                    diff[0] = Op::Added(p.to_string(), v.clone());
                }
                _ => {
                    diff.remove(0);
                }
            }
        }
        (Some(v1), Some(v2)) => {
            diff_helper(&mut diff, &prefix, &v1, &v2);
        }
    }

    diff
}

fn diff_helper(
    acc: &mut JsonDiff,
    prefix: &JsonPath,
    prev: &Value,
    current: &Value,
) {
    match (prev, current) {
        (Value::Array(a), Value::Array(b)) => {
            diff_array(acc, prefix, a, b);
        }
        (Value::Object(a), Value::Object(b)) => {
            diff_obj(acc, prefix, a, b);
        }
        (Value::Array(a), b) => {
            diff_array(acc, prefix, a, &Vec::new());
            acc.push(Op::Added(prefix.clone(), b.clone()));
        }
        (Value::Object(a), b) => {
            diff_obj(acc, prefix, a, &Map::new());
            acc.push(Op::Added(prefix.clone(), b.clone()));
        }
        (a, Value::Array(b)) => {
            acc.push(Op::Removed(prefix.clone(), a.clone()));
            diff_array(acc, prefix, &Vec::new(), b);
        }
        (a, Value::Object(b)) => {
            acc.push(Op::Removed(prefix.clone(), a.clone()));
            diff_obj(acc, prefix, &Map::new(), b);
        }
        (a, b) => {
            if a != b {
                acc.push(Op::Changed(prefix.to_string(), a.clone(), b.clone()));
            }
        }
    }
}

fn diff_array(
    acc: &mut JsonDiff,
    prefix: &JsonPath,
    a: &Vec<Value>,
    b: &Vec<Value>,
) {
    let lcs = lcs::lcs(a, b);

    let mut a_prev: usize = 0;
    let mut b_prev: usize = 0;

    for (a_i, b_i) in &lcs {
        let changed_range = max(a_prev, b_prev)..min(*a_i, *b_i);
        for i in changed_range.clone() {
            acc.push(Op::Changed(
                format!("{}.{}", prefix, i),
                a[i].clone(),
                b[i].clone(),
            ));
        }
        for i in a_prev..*a_i {
            if changed_range.contains(&i) {
                continue;
            };
            acc.push(Op::Removed(format!("{}.{}", prefix, i), a[i].clone()));
        }
        for i in b_prev..*b_i {
            if changed_range.contains(&i) {
                continue;
            };
            acc.push(Op::Added(format!("{}.{}", prefix, i), b[i].clone()));
        }

        a_prev = a_i + 1;
        b_prev = b_i + 1;
    }

    let changed_range = max(a_prev, b_prev)..min(a.len(), b.len());
    for i in changed_range.clone() {
        acc.push(Op::Changed(
            format!("{}.{}", prefix, i),
            a[i].clone(),
            b[i].clone(),
        ));
    }
    for i in a_prev..a.len() {
        if changed_range.contains(&i) {
            continue;
        };
        acc.push(Op::Removed(format!("{}.{}", prefix, i), a[i].clone()));
    }
    for i in b_prev..b.len() {
        if changed_range.contains(&i) {
            continue;
        };
        acc.push(Op::Added(format!("{}.{}", prefix, i), b[i].clone()));
    }
}

fn diff_obj(
    acc: &mut JsonDiff,
    prefix: &JsonPath,
    a: &Map<String, Value>,
    b: &Map<String, Value>,
) {
    let mut a_keys: HashSet<String> = HashSet::new();

    for (k, a_v) in a {
        a_keys.insert(k.clone());

        let new_prefix = format!("{}.{}", prefix, k);
        match b.get(k) {
            Some(b_v) => {
                diff_helper(acc, &new_prefix, a_v, b_v);
            }
            None => {
                acc.push(Op::Removed(new_prefix, a_v.clone()));
            }
        }
    }

    for (k, b_v) in b {
        let new_prefix = format!("{}.{}", prefix, k);
        if !a_keys.contains(k) {
            acc.push(Op::Added(new_prefix, b_v.clone()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vec_value_string(strs: &[&str]) -> Vec<Value> {
        let mut v: Vec<Value> = Vec::new();
        for s in strs {
            v.push(Value::String(s.to_string()));
        }

        v
    }

    #[test]
    fn test_diff_array_1() {
        let a = vec_value_string(&["foo", "bar"]);
        let b = vec_value_string(&["foo", "bar", "baz"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(*acc, vec![Op::Added(".2".to_string(), Value::from("baz"))]);
    }

    #[test]
    fn test_diff_array_2() {
        let a = vec_value_string(&["foo", "bar", "baz"]);
        let b = vec_value_string(&["foo", "bar"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![Op::Removed(".2".to_string(), Value::from("baz"))]
        );
    }

    #[test]
    fn test_diff_array_3() {
        let a = vec_value_string(&["foo", "baz"]);
        let b = vec_value_string(&["foo", "bar", "baz"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![Op::Added(".1".to_string(), Value::from("bar")),]
        );
    }

    #[test]
    fn test_diff_array_4() {
        let a = vec_value_string(&["foo", "bar", "baz"]);
        let b = vec_value_string(&["foo", "baz"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![Op::Removed(".1".to_string(), Value::from("bar")),]
        );
    }

    #[test]
    fn test_diff_array_5() {
        let a = vec_value_string(&["bar", "baz"]);
        let b = vec_value_string(&["foo", "bar", "baz"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![Op::Added(".0".to_string(), Value::from("foo")),]
        );
    }

    #[test]
    fn test_diff_array_6() {
        let a = vec_value_string(&["foo", "bar", "baz"]);
        let b = vec_value_string(&["bar", "baz"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![Op::Removed(".0".to_string(), Value::from("foo")),]
        );
    }

    #[test]
    fn test_diff_array_7() {
        let a = vec![
            Value::from("foo"),
            Value::from("bar"),
            Value::from(1),
            Value::from("a"),
            Value::from("b"),
        ];
        let b = vec![
            Value::from("baz"),
            Value::from("qux"),
            Value::from(1),
            Value::from("c"),
            Value::from("d"),
        ];

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![
                Op::Changed(
                    ".0".to_string(),
                    Value::from("foo"),
                    Value::from("baz")
                ),
                Op::Changed(
                    ".1".to_string(),
                    Value::from("bar"),
                    Value::from("qux")
                ),
                Op::Changed(
                    ".3".to_string(),
                    Value::from("a"),
                    Value::from("c")
                ),
                Op::Changed(
                    ".4".to_string(),
                    Value::from("b"),
                    Value::from("d")
                ),
            ]
        );
    }

    #[test]
    fn test_diff_array_8() {
        let a = vec_value_string(&["foo", "baz"]);
        let b = vec_value_string(&["bar"]);

        let mut acc = JsonDiff(Vec::new());
        diff_array(&mut acc, &"".to_string(), &a, &b);

        assert_eq!(
            *acc,
            vec![
                Op::Changed(
                    ".0".to_string(),
                    Value::from("foo"),
                    Value::from("bar")
                ),
                Op::Removed(".1".to_string(), Value::from("baz")),
            ]
        );
    }
}
