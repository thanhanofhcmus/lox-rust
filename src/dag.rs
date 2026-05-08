use std::collections::HashSet;

use crate::type_index::TypeIndex;

pub type NodeId<T> = TypeIndex<usize, T>;

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub data: T,
    neighbors: HashSet<NodeId<T>>,
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct DAG<T> {
    nodes: Vec<Node<T>>,
}

impl<T> DAG<T> {
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }

    pub fn get_node(&self, id: NodeId<T>) -> Option<&Node<T>> {
        self.nodes.get(id.get())
    }

    /// Create a new node in the DAG
    pub fn add_node(&mut self, data: T) -> NodeId<T> {
        let id = NodeId::new(self.nodes.len());
        self.nodes.push(Node {
            data,
            neighbors: HashSet::new(),
        });
        id
    }

    /// Set a node as parent of another node
    // TODO: return error if we encounter an invalid setup, including circle
    pub fn add_edge(&mut self, parent: NodeId<T>, child: NodeId<T>) {
        let len = self.nodes.len();
        if parent.get() >= len || child.get() >= len {
            // TODO: return error
        }
        let parent_node = self.nodes.get_mut(parent.get()).unwrap();
        parent_node.neighbors.insert(child);
        // TODO: maybe call detect circle here
    }

    pub fn has_circle(&self) -> bool {
        // TODO: implement
        false
    }

    pub fn get_leaf_first_order(&self) -> Vec<NodeId<T>> {
        let len = self.nodes.len();
        let mut order = Vec::with_capacity(len);
        let mut visited = vec![false; len];
        for node_idx in 0..len {
            if !visited[node_idx] {
                self.post_order_dfs(node_idx, &mut order, &mut visited);
            }
        }
        order
    }

    fn post_order_dfs(&self, current_idx: usize, order: &mut Vec<NodeId<T>>, visited: &mut [bool]) {
        visited[current_idx] = true;
        for neighbor_node in &self.nodes[current_idx].neighbors {
            let idx = neighbor_node.get();
            if !visited[idx] {
                self.post_order_dfs(idx, order, visited);
            }
        }
        order.push(NodeId::new(current_idx));
    }

    pub fn transitive_reduce(&mut self) {
        for current in 0..self.nodes.len() {
            let neighbors = &self.nodes[current].neighbors;
            let mut redundant = HashSet::new();

            for &from in neighbors {
                for &to in neighbors {
                    if from == to {
                        continue;
                    }
                    if self.has_path(from, to) {
                        redundant.insert(from);
                        break;
                    }
                }
            }

            self.nodes[current].neighbors.retain(|n| !redundant.contains(n));
        }
    }

    fn has_path(&self, from: NodeId<T>, to: NodeId<T>) -> bool {
        let mut visited = vec![false; self.nodes.len()];
        let mut to_visit = self.nodes.get(from.get()).unwrap().neighbors.iter().collect::<Vec<_>>();

        while let Some(&current_node) = to_visit.pop() {
            if current_node == to {
                return true;
            }
            let idx = current_node.get();
            if visited[idx] {
                continue;
            }
            visited[idx] = true;
            to_visit.extend(self.nodes[idx].neighbors.iter());
        }

        false
    }
}
