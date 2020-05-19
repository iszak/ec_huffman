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
    symbol: Option<S>,
    weight: W,
    index: usize,
}

impl<S: Ord + Clone, W: Ord + Clone> Eq for NodePointer<S, W> {}

impl<S: Ord + Clone, W: Ord + Clone> PartialEq for NodePointer<S, W> {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<S: Ord + Clone, W: Ord + Clone> Ord for NodePointer<S, W> {
    fn cmp(&self, other: &Self) -> Ordering {
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

// TODO:
// - Allow replacing code generator to allow Length-limited Huffman coding
fn assign_code<S: Debug + Eq + Hash + Clone, W: Debug + Ord + Clone>(
    nodes: &Vec<Node<S, W>>,
    node: &Node<S, W>,
    encode_book: &mut EncodeCodebook<S>,
    decode_book: &mut DecodeCodebook<S, W>,
    current_code: &str,
    suffix_code: &str,
) -> () {
    let new_code = current_code.to_string() + suffix_code;
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

    assign_codes(nodes, node, &new_code, encode_book, decode_book);
}

fn assign_codes<S: Debug + Clone + Eq + Hash, W: Debug + Clone + Ord>(
    nodes: &Vec<Node<S, W>>,
    node: &Node<S, W>,
    current_code: &str,
    encode_book: &mut EncodeCodebook<S>,
    decode_book: &mut DecodeCodebook<S, W>,
) -> () {
    let mut index = 0;
    for node_index in node.children.iter() {
        // TODO: Make generic
        let suffix_code: u32 = (index % 2) as u32;
        assign_code(
            nodes,
            nodes.get(*node_index).unwrap(),
            encode_book,
            decode_book,
            &current_code,
            &suffix_code.to_string(),
        );

        index += 1;
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

                    children.push(node_pointer.index);
                    weight_accumulator = match weight_accumulator {
                        None => Some(node_pointer.weight.clone()),
                        Some(w) => Some(node_pointer.weight.clone() + w),
                    };

                    index += 1;
                }

                let weight = weight_accumulator.unwrap();

                let new_index = all_nodes.len();

                // Create a new internal node with these two nodes as children and with
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

    assign_codes(
        &all_nodes,
        &root_node,
        "",
        &mut encode_book,
        &mut decode_book,
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
    fn encode() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 3); // 0
        map.insert("b", 2); // 11
        map.insert("c", 1); // 10

        let (encode_book, _) = from_iter(map.iter());
        assert_eq!(encode_symbols(&encode_book, &vec!["a"]).join(""), "0");
        assert_eq!(encode_symbols(&encode_book, &vec!["b"]).join(""), "11");
        assert_eq!(encode_symbols(&encode_book, &vec!["c"]).join(""), "10");
        assert_eq!(
            encode_symbols(&encode_book, &vec!["a", "a", "a", "b", "c"]).join(""),
            "0001110"
        );
    }

    #[test]
    fn decode_code_unique_weights() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 3); // 0
        map.insert("b", 2); // 11
        map.insert("c", 1); // 10

        let (_, decode_book) = from_iter(map.iter());
        assert_eq!(decode_str(&decode_book, "0").join(""), "a");
        assert_eq!(decode_str(&decode_book, "11").join(""), "b");
        assert_eq!(decode_str(&decode_book, "10").join(""), "c");
        assert_eq!(decode_str(&decode_book, "000111110").join(""), "aaabbc");
    }

    #[test]
    fn decode_code_duplicate_weights() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 1); // 1
        map.insert("b", 1); // 01
        map.insert("c", 1); // 10
        map.insert("d", 1); // 11

        let (_, decode_book) = from_iter(map.iter());
        assert_eq!(decode_str(&decode_book, "00").join(""), "a");
        assert_eq!(
            decode_str(&decode_book, "00000000010101101011").join(""),
            "aaaabbbccd"
        );
    }
}
