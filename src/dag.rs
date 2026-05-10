use std::collections::{HashMap, HashSet};

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
    node_index: HashMap<T, usize>,
}

#[derive(Debug, Clone, Copy)]
enum NodeCycleState {
    Unvisited,
    Exploring,
    Done,
}

impl<T: Clone + Eq + std::hash::Hash> DAG<T> {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            node_index: HashMap::new(),
        }
    }

    pub fn get_node(&self, id: NodeId<T>) -> Option<&Node<T>> {
        self.nodes.get(id.get())
    }

    /// Add a node to the DAG.
    ///
    /// If a node with the same data already exists, returns the existing
    /// [`NodeId`] instead of creating a duplicate.  This guarantees that
    /// every unique `T` value corresponds to at most one node, which is
    /// essential for correct cycle detection when multiple modules import
    /// the same dependency through different paths.
    pub fn add_node(&mut self, data: T) -> NodeId<T> {
        if let Some(&idx) = self.node_index.get(&data) {
            return NodeId::new(idx);
        }
        let id = NodeId::new(self.nodes.len());
        self.node_index.insert(data.clone(), id.get());
        let neighbors = HashSet::new();
        self.nodes.push(Node { data, neighbors });
        id
    }

    /// Set a node as the parent of an another node
    // TODO: return error if we encounter an invalid setup, including circle
    pub fn add_edge(&mut self, parent: NodeId<T>, child: NodeId<T>) {
        let len = self.nodes.len();
        if parent.get() >= len || child.get() >= len {
            // TODO: return error
        }
        let parent_node = self.nodes.get_mut(parent.get()).unwrap();
        parent_node.neighbors.insert(child);
    }

    /// Detects if the current graph has cycle, i.e. not DAG.
    /// Internally uses tri-color algorithm
    pub fn has_cycle(&self) -> bool {
        let mut states = vec![NodeCycleState::Unvisited; self.nodes.len()];
        for node_idx in 0..self.nodes.len() {
            if self.detect_cycle_dfs(node_idx, &mut states) {
                return true;
            }
        }
        false
    }

    fn detect_cycle_dfs(&self, current_idx: usize, states: &mut [NodeCycleState]) -> bool {
        states[current_idx] = NodeCycleState::Exploring;
        for neighbor_node in &self.nodes[current_idx].neighbors {
            let idx = neighbor_node.get();
            match states[idx] {
                NodeCycleState::Unvisited => {
                    if self.detect_cycle_dfs(idx, states) {
                        return true;
                    }
                }
                NodeCycleState::Exploring => return true,
                NodeCycleState::Done => continue,
            }
        }
        states[current_idx] = NodeCycleState::Done;
        false
    }

    pub fn get_leaf_first_order(&self) -> Vec<NodeId<T>> {
        let len = self.nodes.len();
        let mut order = Vec::with_capacity(len);
        let mut visited = vec![false; len];
        for node_idx in 0..len {
            if !visited[node_idx] {
                self.collect_post_order_dfs(node_idx, &mut order, &mut visited);
            }
        }
        order
    }

    fn collect_post_order_dfs(&self, current_idx: usize, order: &mut Vec<NodeId<T>>, visited: &mut [bool]) {
        visited[current_idx] = true;
        for neighbor_node in &self.nodes[current_idx].neighbors {
            let idx = neighbor_node.get();
            if !visited[idx] {
                self.collect_post_order_dfs(idx, order, visited);
            }
        }
        order.push(NodeId::new(current_idx));
    }

    /// transitive_reduce mutate the current DAG inplace to simplify the dendepencies.
    /// Any direct connection between 2 nodes is removed if there already is a they can be reached from other path
    pub fn transitive_reduce(&mut self) {
        for current in 0..self.nodes.len() {
            let neighbors = &self.nodes[current].neighbors;
            let mut redundant = HashSet::new();

            for &from in neighbors {
                for &to in neighbors {
                    if from == to {
                        continue;
                    }
                    // Say we have a graph A -> B, B -> C, and A -> C.
                    // We are checking current = A, from = B, to = C,
                    // there is a path B -> C => we remove the path A -> C (insert C to the redundant set)
                    if self.has_path(from, to) {
                        redundant.insert(to);
                        // Do not `break` early here
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_nodes_and_edges() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");

        dag.add_edge(a, b);

        assert_eq!(dag.nodes.len(), 2);
        assert!(dag.get_node(a).unwrap().neighbors.contains(&b));
    }

    #[test]
    fn test_cycle_detection_positive() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");

        dag.add_edge(a, b);
        dag.add_edge(b, c);
        dag.add_edge(c, a); // Creates a cycle: A -> B -> C -> A

        assert!(dag.has_cycle(), "Should detect a cycle in A->B->C->A");
    }

    #[test]
    fn test_cycle_detection_negative() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");

        dag.add_edge(a, b);
        dag.add_edge(a, c);
        dag.add_edge(b, c); // A points to both B and C, B points to C. Valid DAG.

        assert!(!dag.has_cycle(), "A diamond shape is not a cycle");
    }

    #[test]
    fn test_leaf_first_order() {
        let mut dag = DAG::new();
        let a = dag.add_node("Root");
        let b = dag.add_node("Middle");
        let c = dag.add_node("Leaf");

        dag.add_edge(a, b);
        dag.add_edge(b, c);

        let order = dag.get_leaf_first_order();

        // In post-order/leaf-first: C must come before B, and B must come before A.
        let pos_a = order.iter().position(|&id| id == a).unwrap();
        let pos_b = order.iter().position(|&id| id == b).unwrap();
        let pos_c = order.iter().position(|&id| id == c).unwrap();

        assert!(pos_c < pos_b);
        assert!(pos_b < pos_a);
    }

    #[test]
    fn test_transitive_reduction() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");

        // Structure: A -> B, B -> C, and a redundant A -> C
        dag.add_edge(a, b);
        dag.add_edge(b, c);
        dag.add_edge(a, c);

        assert_eq!(
            dag.get_node(a).unwrap().neighbors.len(),
            2,
            "Before reduction, A should have 2 neighbors"
        );

        dag.transitive_reduce();

        let a_node = dag.get_node(a).unwrap();
        assert_eq!(
            a_node.neighbors.len(),
            1,
            "After reduction, A should only have 1 neighbor (B)"
        );
        assert!(a_node.neighbors.contains(&b));
        assert!(
            !a_node.neighbors.contains(&c),
            "Redundant edge A -> C should be removed"
        );
    }

    #[test]
    fn test_reduction_long_chain() {
        let mut dag = DAG::new();
        // A -> B -> C -> D -> E
        // Additional redundant edges: A->C, A->D, A->E, B->D, B->E
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");
        let d = dag.add_node("D");
        let e = dag.add_node("E");

        dag.add_edge(a, b);
        dag.add_edge(b, c);
        dag.add_edge(c, d);
        dag.add_edge(d, e);

        // Redundant shortcuts
        dag.add_edge(a, c);
        dag.add_edge(a, e);
        dag.add_edge(b, e);

        dag.transitive_reduce();

        // After reduction, every node should have exactly 1 neighbor except E
        assert_eq!(dag.get_node(a).unwrap().neighbors.len(), 1, "A should only point to B");
        assert!(dag.get_node(a).unwrap().neighbors.contains(&b));

        assert_eq!(dag.get_node(b).unwrap().neighbors.len(), 1, "B should only point to C");
        assert!(dag.get_node(b).unwrap().neighbors.contains(&c));

        assert_eq!(dag.get_node(d).unwrap().neighbors.len(), 1, "D should only point to E");
    }

    #[test]
    fn test_reduction_diamond_with_shortcut() {
        let mut dag = DAG::new();
        /*
               A
              / \
             B   C
              \ / \
               D---E

        (Redundant edge A->D and C->E)
        */
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");
        let d = dag.add_node("D");
        let e = dag.add_node("E");

        dag.add_edge(a, b);
        dag.add_edge(a, c);
        dag.add_edge(b, d);
        dag.add_edge(c, d);
        dag.add_edge(c, e);
        dag.add_edge(d, e);

        // Shortcut that should be removed
        dag.add_edge(a, d);

        dag.transitive_reduce();

        // A -> D is redundant because A -> B -> D exists
        assert!(!dag.get_node(a).unwrap().neighbors.contains(&d));
        // C -> E is redundant because C -> D -> E exists
        assert!(!dag.get_node(c).unwrap().neighbors.contains(&e));

        // Essential edges must remain
        assert!(dag.get_node(a).unwrap().neighbors.contains(&b));
        assert!(dag.get_node(a).unwrap().neighbors.contains(&c));
    }

    #[test]
    fn test_reduction_cross_dependencies() {
        let mut dag = DAG::new();
        /*
            A -> B -> D
            |   /
            v  v
            C /
        */
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");
        let d = dag.add_node("D");

        dag.add_edge(a, b);
        dag.add_edge(a, c);
        dag.add_edge(b, c);
        dag.add_edge(b, d);

        dag.transitive_reduce();

        // A -> C is redundant because A -> B -> C exists
        let a_neighbors = &dag.get_node(a).unwrap().neighbors;
        assert!(a_neighbors.contains(&b));
        assert!(!a_neighbors.contains(&c), "A->C should be removed by B's presence");

        // B -> C and B -> D are essential
        let b_neighbors = &dag.get_node(b).unwrap().neighbors;
        assert!(b_neighbors.contains(&c));
        assert!(b_neighbors.contains(&d));
    }

    #[test]
    fn test_reduction_idempotency() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");

        dag.add_edge(a, b);
        dag.add_edge(b, c);
        dag.add_edge(a, c);

        dag.transitive_reduce();
        let first_pass = dag.get_node(a).unwrap().neighbors.clone();

        dag.transitive_reduce();
        let second_pass = dag.get_node(a).unwrap().neighbors.clone();

        assert_eq!(
            first_pass, second_pass,
            "Running reduction twice should not change the result"
        );
    }

    #[test]
    fn test_has_path() {
        let mut dag = DAG::new();
        let a = dag.add_node("A");
        let b = dag.add_node("B");
        let c = dag.add_node("C");
        let d = dag.add_node("D");

        dag.add_edge(a, b);
        dag.add_edge(b, c);

        assert!(dag.has_path(a, c), "Path exists from A to C via B");
        assert!(!dag.has_path(a, d), "No path should exist from A to D");
        assert!(!dag.has_path(c, a), "No path should exist backwards in a DAG");
    }
}
