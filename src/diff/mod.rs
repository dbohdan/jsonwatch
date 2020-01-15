use serde_json::Value;

use std::{
    collections::{BTreeMap, HashSet},
    fmt,
};

pub type JsonPath = String;
pub type FlatJson = BTreeMap<JsonPath, Value>;

#[derive(Debug)]
pub struct FlatJsonDiff {
    added: FlatJson,
    removed: FlatJson,
    changed: BTreeMap<JsonPath, (Value, Value)>,
}

impl fmt::Display for FlatJsonDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (path, (from, to)) in &self.changed {
            writeln!(f, "{}: {} -> {}", path, from, to).unwrap();
        }
        for (path, value) in &self.removed {
            writeln!(f, "- {}: {}", path, value).unwrap();
        }
        for (path, value) in &self.added {
            writeln!(f, "+ {}: {}", path, value).unwrap();
        }

        fmt::Result::Ok(())
    }
}

impl FlatJsonDiff {
    pub fn total(&self) -> usize {
        self.added.len() + self.removed.len() + self.changed.len()
    }
}

pub fn diff(prev: FlatJson, current: FlatJson) -> FlatJsonDiff {
    let mut diff = FlatJsonDiff {
        added: BTreeMap::new(),
        removed: BTreeMap::new(),
        changed: BTreeMap::new(),
    };

    let mut prev_keys: HashSet<String> = HashSet::new();
    for (k, prev_v) in prev {
        prev_keys.insert(k.clone());

        match current.get(&k) {
            Some(current_v) => {
                let current_v = current_v.clone();
                if current_v != prev_v {
                    diff.changed.insert(k, (prev_v, current_v));
                }
            }
            None => {
                diff.removed.insert(k, prev_v);
            }
        }
    }

    for (k, current_v) in current {
        if !prev_keys.contains(&k) {
            diff.added.insert(k, current_v);
        }
    }

    diff
}

pub fn flatten(prefix: String, json: Value) -> FlatJson {
    match json {
        Value::Object(a) => a
            .iter()
            .flat_map(|(k, v)| {
                let new_prefix = format!("{}.{}", prefix, k);
                flatten(new_prefix, v.clone())
            })
            .collect::<FlatJson>(),
        Value::Array(a) => {
            let mut map = BTreeMap::new();
            for i in 0..a.len() {
                let new_prefix = format!("{}.{}", prefix, i);
                map.append(&mut flatten(new_prefix, a[i].clone()));
            }

            map
        }
        _ => {
            let mut fresh = BTreeMap::new();
            fresh.insert(prefix, json);

            fresh
        }
    }
}
