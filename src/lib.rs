use std::cmp::Ordering;
use std::cmp::Reverse;

use std::fmt::Debug;
use std::hash::Hash;
use std::iter::IntoIterator;
use std::iter::Iterator;
use std::ops::Add;
use std::str::Chars;

use std::collections::BinaryHeap;
use std::collections::HashMap;

pub type InternalEncodeCodebook<S> = HashMap<S, String>;
pub type InternalDecodeCodebook<S, W> = HashMap<String, (S, W)>;

pub struct Encoder<'a, S, I: Iterator<Item = &'a S>> {
    codebook: &'a InternalEncodeCodebook<S>,
    iterator: &'a mut I,
}

impl<'a, S: Clone + Eq + Hash, I: Iterator<Item = &'a S>> Iterator for Encoder<'a, S, I> {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        match self.iterator.next() {
            Some(symbol) => match self.codebook.get(&symbol) {
                Some(code) => Some(code.clone()),
                None => None,
            },
            None => return None,
        }
    }
}

pub struct EncodeCodebook<S> {
    book: InternalEncodeCodebook<S>,
}

impl<'a, S: Eq + Hash + Clone> EncodeCodebook<S> {
    pub fn encode_symbols_from_iter<I: Iterator<Item = &'a S>>(
        &'a self,
        iter: &'a mut I,
    ) -> Encoder<'a, S, I> {
        return Encoder {
            codebook: &self.book,
            iterator: iter,
        };
    }

    pub fn encode_symbols(&'a self, symbols: &Vec<S>) -> Vec<String> {
        return self.encode_symbols_from_iter(&mut symbols.iter()).collect();
    }

    pub fn encode_symbol(&'a self, symbol: &S) -> Option<&'a String> {
        return self.book.get(symbol);
    }

    pub fn encode_symbol_from_buffer(&'a self, symbol: &S, buffer: &mut Vec<&'a String>) -> () {
        match self.book.get(symbol) {
            Some(code) => buffer.push(code),
            None => {}
        };
    }
}

pub struct DecodeCodebook<S, W> {
    book: InternalDecodeCodebook<S, W>,
    max_code_len: usize,
}

pub struct Decoder<'a, S: Clone, W> {
    codebook: &'a DecodeCodebook<S, W>,
    iterator: Chars<'a>,
}

impl<'a, S: Clone, W> Iterator for Decoder<'a, S, W> {
    type Item = S;

    fn next(&mut self) -> Option<S> {
        // TODO: Allocate string of size string.len() / max_code_len() * max_symbol_len()
        let mut code: String = String::with_capacity(100);

        loop {
            match self.iterator.next() {
                Some(c) => code.push(c),
                None => return None,
            }

            match self.codebook.get(&code) {
                Some(a) => {
                    return Some(a.0.clone());
                }
                None => continue,
            }
        }
    }
}

impl<'a, S: Clone, W> DecodeCodebook<S, W> {
    fn get(&self, code: &String) -> Option<&(S, W)> {
        if code.len() > self.max_code_len {
            return None;
        }

        return self.book.get(code);
    }

    pub fn decode_iter(&'a self, string: &'a str) -> Decoder<'a, S, W> {
        return Decoder {
            codebook: &self,
            iterator: string.chars(),
        };
    }

    pub fn decode_str(&self, string: &str) -> Vec<S> {
        return self.decode_iter(string).collect();
    }
}

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

fn assign_code<S: Debug + Eq + Hash + Clone, W: Debug + Ord + Clone>(
    node: &Node<S, W>,
    encode_book: &mut InternalEncodeCodebook<S>,
    decode_book: &mut InternalDecodeCodebook<S, W>,
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
    encode_book: &mut InternalEncodeCodebook<S>,
    decode_book: &mut InternalDecodeCodebook<S, W>,
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

    let mut encode_book: InternalEncodeCodebook<S> = InternalEncodeCodebook::with_capacity(len);
    let mut decode_book: InternalDecodeCodebook<S, W> = InternalDecodeCodebook::with_capacity(len);
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

    let max_code_len = decode_book.keys().max_by_key(|k| k.len()).unwrap().len();

    return (
        EncodeCodebook { book: encode_book },
        DecodeCodebook {
            book: decode_book,
            max_code_len: max_code_len,
        },
    );
}

pub fn from_iter<
    'a,
    S: 'a + Debug + Ord + Clone + Hash,
    W: 'a + Debug + Ord + Clone + Add<Output = W>,
    I: IntoIterator<Item = (&'a S, &'a W)> + ExactSizeIterator,
>(
    iter: I,
) -> (EncodeCodebook<S>, DecodeCodebook<S, W>) {
    let mut heap = BinaryHeap::with_capacity(iter.len());

    for (symbol, weight) in iter {
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
        assert_eq!(encode_book.encode_symbols(&vec!["a1"]).join(""), "0");
        assert_eq!(encode_book.encode_symbols(&vec!["a2"]).join(""), "10");
        assert_eq!(encode_book.encode_symbols(&vec!["a3"]).join(""), "110");
        assert_eq!(encode_book.encode_symbols(&vec!["a4"]).join(""), "111");
        assert_eq!(
            encode_book
                .encode_symbols(&vec!["a1", "a2", "a3", "a4"])
                .join(""),
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
        assert_eq!(encode_book.encode_symbols(&vec!["a"]).join(""), "0");
        assert_eq!(encode_book.encode_symbols(&vec!["b"]).join(""), "10");
        assert_eq!(encode_book.encode_symbols(&vec!["c"]).join(""), "11");
        assert_eq!(
            encode_book
                .encode_symbols(&vec!["a", "a", "a", "b", "c"])
                .join(""),
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
        assert_eq!(decode_book.decode_str("0").join(""), "a");
        assert_eq!(decode_book.decode_str("10").join(""), "b");
        assert_eq!(decode_book.decode_str("11").join(""), "c");
        assert_eq!(decode_book.decode_str("000101011").join(""), "aaabbc");
    }

    #[test]
    fn decode_code_duplicate_weights() {
        let mut map: HashMap<&str, usize> = HashMap::new();
        map.insert("a", 1); // 111
        map.insert("b", 1); // 110
        map.insert("c", 1); // 10
        map.insert("d", 1); // 0

        let (_, decode_book) = from_iter(map.iter());
        assert_eq!(decode_book.decode_str("111").join(""), "a");
        assert_eq!(
            decode_book
                .decode_str("11111111111111011011010100")
                .join(""),
            "aaaabbbccd"
        );
    }
}
