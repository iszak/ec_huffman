use std::cmp::Ordering;
use std::cmp::Reverse;

use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Iterator;
use std::ops::Add;

use std::collections::BinaryHeap;
use std::collections::HashMap;

pub type EncodeCodebook<S> = HashMap<S, String>;
pub type DecodeCodebook<S, W> = HashMap<String, (S, W)>;

#[derive(Debug)]
struct Node<S, W> {
    children: Vec<usize>,
    data: NodeData<S, W>,
}

#[derive(Debug)]
struct NodePointer<S, W> {
    weight: W,
    node_type: NodeType,
    symbol: Option<S>,
    index: usize,
}

#[derive(Debug, PartialEq)]
enum NodeType {
    Leaf,
    Internal,
}

impl<S: Ord + Clone, W: Ord + Clone> Eq for NodePointer<S, W> {}

impl<S: Ord + Clone, W: Ord + Clone> PartialEq for NodePointer<S, W> {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<S: Ord + Clone, W: Ord + Clone> Ord for NodePointer<S, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        if &self.node_type == &NodeType::Internal && &other.node_type != &NodeType::Internal {
            // Internal nodes are always to the right
            return Ordering::Greater;
        } else if &self.node_type == &NodeType::Leaf && &other.node_type == &NodeType::Internal {
            // Leaf nodes are to the left
            return Ordering::Less;
        }

        let w1 = &self.weight;
        let w2 = &other.weight;

        match Reverse(w1).cmp(&Reverse(w2)) {
            Ordering::Equal => match &self.symbol {
                Some(s1) => match &other.symbol {
                    Some(s2) => Reverse(s1).cmp(&Reverse(s2)),
                    None => Ordering::Equal,
                },
                None => Ordering::Equal,
            },
            o => o,
        }
    }
}

impl<S: Ord + Clone, W: Ord + Clone> PartialOrd for NodePointer<S, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Leaf<S, W> {
    code: Option<String>,
    symbol: S,
    weight: W,
    index: usize,
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Internal<W> {
    weight: W,
    index: usize,
}

#[derive(Debug)]
enum NodeData<S, W> {
    Internal(Internal<W>),
    Leaf(Leaf<S, W>),
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
struct HeapData<S, W> {
    weight: W,
    symbol: S,
}

struct CodeGenerator {
    last_code: u32,
    current_code: String,
}

impl CodeGenerator {
    fn next(&mut self) -> String {
        if self.last_code == 0 {
            self.last_code = 1;
            return self.current_code.to_owned() + "1";
        } else {
            self.last_code = 0;
            return self.current_code.to_owned() + "0";
        }
    }

    fn dive(&mut self) {
        if self.last_code == 1 {
            self.current_code.push('1');
        } else {
            self.current_code.push('0');
        }
    }
}

// TODO:
// - Allow replacing code generator to allow Length-limited Huffman coding
fn assign_code<S: Debug + Eq + Hash + Clone, W: Debug + Ord + Clone>(
    node: &Node<S, W>,
    encode_book: &mut EncodeCodebook<S>,
    decode_book: &mut DecodeCodebook<S, W>,
    code_generator: &mut CodeGenerator,
) -> () {
    let new_code = code_generator.next();

    if let NodeData::Leaf(data) = &node.data {
        match encode_book.insert(data.symbol.clone(), new_code.clone()) {
            Some(_) => panic!(format!(
                "symbol {:?} already exists in encode book",
                data.symbol
            )),
            None => {}
        }
        match decode_book.insert(new_code.clone(), (data.symbol.clone(), data.weight.clone())) {
            Some(_) => panic!(format!(
                "code {:?} already exists in decode book",
                data.symbol
            )),
            None => {}
        };
    }
}

fn assign_codes<S: Debug + Clone + Eq + Hash, W: Debug + Clone + Ord>(
    nodes: &Vec<Node<S, W>>,
    node: &Node<S, W>,
    encode_book: &mut EncodeCodebook<S>,
    decode_book: &mut DecodeCodebook<S, W>,
    code_generator: &mut CodeGenerator,
) -> () {
    for node_index in node.children.iter() {
        let child_node = nodes.get(*node_index).unwrap();
        assign_code(child_node, encode_book, decode_book, code_generator);

        if child_node.children.len() > 0 {
            code_generator.dive();
            assign_codes(nodes, child_node, encode_book, decode_book, code_generator);
        }
    }
}

fn create_books<S: Hash + Eq + Debug + Ord + Clone, W: Debug + Ord + Clone + Add<Output = W>>(
    frequency_table: BinaryHeap<HeapData<S, W>>,
    nodes: usize,
) -> (EncodeCodebook<S>, DecodeCodebook<S, W>) {
    let len = frequency_table.len();

    // 1. Create a leaf node for each symbol and add it to the priority queue.
    // capacity = N leaf nodes + N - 1 internal nodes
    let mut all_nodes: Vec<Node<S, W>> = Vec::with_capacity(len + len - 1);

    // capacity will start with N leaf nodes and only get smaller
    let mut current_nodes: BinaryHeap<NodePointer<S, W>> = BinaryHeap::with_capacity(len);

    for (index, entry) in frequency_table.into_sorted_vec().iter().enumerate() {
        all_nodes.push(Node {
            children: Vec::with_capacity(nodes),
            data: NodeData::Leaf(Leaf {
                code: None,
                symbol: entry.symbol.clone(),
                weight: entry.weight.clone(),
                index: index,
            }),
        });
        current_nodes.push(NodePointer {
            symbol: Some(entry.symbol.clone()),
            weight: entry.weight.clone(),
            node_type: NodeType::Leaf,
            index: index,
        });
    }

    // 2. While there is more than one node in the queue:
    loop {
        match current_nodes.len() {
            0 => {
                panic!("no nodes found");
            }
            1 => {
                break;
            }
            _ => {
                // 1. Remove the N nodes of highest priority (lowest probability) from the queue
                let mut weight_accumulator: Option<W> = None;
                let mut children: Vec<usize> = Vec::with_capacity(nodes);

                let mut index = 0;
                loop {
                    if index == nodes {
                        break;
                    }

                    let node_pointer = match current_nodes.pop() {
                        Some(np) => np,
                        None => break,
                    };

                    children.insert(0, node_pointer.index);

                    weight_accumulator = match weight_accumulator {
                        None => Some(node_pointer.weight.clone()),
                        Some(w) => Some(node_pointer.weight.clone() + w),
                    };

                    index += 1;
                }

                let weight = weight_accumulator.unwrap();

                let new_index = all_nodes.len();

                // Create a new internal node with N nodes as children and with
                // probability equal to the sum of the two nodes' probabilities.
                // Add the new node to the queue.
                all_nodes.push(Node {
                    children: children,
                    data: NodeData::Internal(Internal {
                        weight: weight.clone(),
                        index: new_index,
                    }),
                });

                current_nodes.push(NodePointer {
                    symbol: None,
                    weight: weight.clone(),
                    index: new_index,
                    node_type: NodeType::Internal,
                });
            }
        }
    }

    let root_node = match current_nodes.pop() {
        Some(pointer) => match all_nodes.get(pointer.index) {
            Some(node) => node,
            None => panic!("root node not found"),
        },
        None => panic!("root node not found"),
    };

    let mut encode_book: EncodeCodebook<S> = EncodeCodebook::with_capacity(len);
    let mut decode_book: DecodeCodebook<S, W> = DecodeCodebook::with_capacity(len);
    let mut code_generator = CodeGenerator {
        last_code: 1,
        current_code: "".to_owned(),
    };

    assign_codes(
        &all_nodes,
        &root_node,
        &mut encode_book,
        &mut decode_book,
        &mut code_generator,
    );

    return (encode_book, decode_book);
}

// TODO: Implement lazy iterator?
pub fn encode_symbols<S: Eq + Debug + Clone + Hash>(
    codebook: &EncodeCodebook<S>,
    symbols: &Vec<S>,
) -> Vec<String> {
    let mut output: Vec<String> = Vec::with_capacity(symbols.len());
    for symbol in symbols {
        match codebook.get(symbol) {
            Some(code) => output.push(code.clone()),
            None => panic!(format!("code {:?} doesn't exist", symbol)),
        }
    }

    return output;
}

pub fn encode_symbol<'a, S: Eq + Debug + Clone + Hash>(
    codebook: &'a EncodeCodebook<S>,
    symbol: &S,
) -> Option<&'a String> {
    codebook.get(symbol)
}

pub fn encode_symbol_from_buffer<'a, S: Eq + Debug + Clone + Hash>(
    codebook: &'a EncodeCodebook<S>,
    symbol: &S,
    buffer: &mut Vec<&'a String>,
) -> () {
    match codebook.get(symbol) {
        Some(code) => buffer.push(code),
        None => {}
    };
}

pub fn decode_str<S: Debug + Clone, W: Debug>(
    codebook: &DecodeCodebook<S, W>,
    string: &str,
) -> Vec<S> {
    // TODO: Allocate vector of size string.len() / max_code_len() * max_symbol_len()
    let mut output: Vec<S> = vec![];

    // TODO: Cache on construction
    let max_code_len = codebook.keys().max_by_key(|k| k.len()).unwrap().len();

    // TODO: Allocate string of size string.len() / max_code_len() * max_symbol_len()
    let mut code: String = "".to_owned();
    for c in string.chars() {
        code.push(c);

        if code.len() > max_code_len {
            panic!(format!(
                "code {:?} exceeds max code length {:?}",
                code, max_code_len
            ));
        }

        match codebook.get(&code) {
            Some(a) => {
                output.push(a.0.clone());
                code = "".to_owned();
            }
            None => continue,
        }
    }

    return output;
}

pub fn from_iter<
    'a,
    S: 'a + Debug + Ord + Clone + Hash,
    W: 'a + Debug + Ord + Clone + Add<Output = W>,
    I: Iterator<Item = (&'a S, &'a W)>,
>(
    map: I,
) -> (EncodeCodebook<S>, DecodeCodebook<S, W>) {
    let mut heap = match map.size_hint() {
        (_, Some(upper)) => BinaryHeap::with_capacity(upper),
        (lower, None) => BinaryHeap::with_capacity(lower),
    };

    for (symbol, weight) in map {
        heap.push(HeapData {
            symbol: symbol.clone(),
            weight: weight.clone(),
        });
    }

    create_books(heap, 2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode1() {
        // a1:0.4, a2:0.35, a3:0.2, a4:0.05
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a1", 40); // 0
        map.insert("a2", 35); // 11
        map.insert("a3", 20); // 10
        map.insert("a4", 5); // 10

        let (encode_book, _) = from_iter(map.iter());
        assert_eq!(encode_symbols(&encode_book, &vec!["a1"]).join(""), "0");
        assert_eq!(encode_symbols(&encode_book, &vec!["a2"]).join(""), "10");
        assert_eq!(encode_symbols(&encode_book, &vec!["a3"]).join(""), "110");
        assert_eq!(encode_symbols(&encode_book, &vec!["a4"]).join(""), "111");
        assert_eq!(
            encode_symbols(&encode_book, &vec!["a1", "a2", "a3", "a4"]).join(""),
            "010110111"
        );
    }

    #[test]
    fn encode2() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 3); // 0
        map.insert("b", 2); // 10
        map.insert("c", 1); // 11

        let (encode_book, _) = from_iter(map.iter());
        assert_eq!(encode_symbols(&encode_book, &vec!["a"]).join(""), "0");
        assert_eq!(encode_symbols(&encode_book, &vec!["b"]).join(""), "10");
        assert_eq!(encode_symbols(&encode_book, &vec!["c"]).join(""), "11");
        assert_eq!(
            encode_symbols(&encode_book, &vec!["a", "a", "a", "b", "c"]).join(""),
            "0001011"
        );
    }

    #[test]
    fn decode_code_unique_weights() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 3); // 0
        map.insert("b", 2); // 10
        map.insert("c", 1); // 11

        let (_, decode_book) = from_iter(map.iter());
        assert_eq!(decode_str(&decode_book, "0").join(""), "a");
        assert_eq!(decode_str(&decode_book, "10").join(""), "b");
        assert_eq!(decode_str(&decode_book, "11").join(""), "c");
        assert_eq!(decode_str(&decode_book, "000101011").join(""), "aaabbc");
    }

    #[test]
    fn decode_code_duplicate_weights() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 1); // 111
        map.insert("b", 1); // 110
        map.insert("c", 1); // 10
        map.insert("d", 1); // 0

        let (_, decode_book) = from_iter(map.iter());
        assert_eq!(decode_str(&decode_book, "111").join(""), "a");
        assert_eq!(
            decode_str(&decode_book, "11111111111111011011010100").join(""),
            "aaaabbbccd"
        );
    }
}
