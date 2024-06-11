use tree_sitter::Node as OtherNode;
use tree_sitter::Tree as OtherTree;
use tree_sitter::{Parser, TreeCursor};

use crate::checker::Checker;
use crate::traits::{LanguageInfo, Search};

/// An `AST`.
#[derive(Clone)]
pub struct Tree(OtherTree);

impl Tree {
    /// Creates a new [`Tree`] instance.
    pub fn new<T: LanguageInfo>(code: &[u8]) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&T::get_lang().get_ts_language())
            .unwrap();

        Self(parser.parse(code, None).unwrap())
    }

    /// Gets the tree's root.
    pub fn get_root(&self) -> Node {
        Node(self.0.root_node())
    }
}

/// An `AST` node.
#[derive(Clone, Copy)]
pub struct Node<'a>(OtherNode<'a>);

impl<'a> Node<'a> {
    /// Checks if a node represents a syntax error or contains any syntax errors
    /// anywhere within it.
    pub fn has_error(&self) -> bool {
        self.0.has_error()
    }

    /// Gets a numeric id for this node that is unique.
    pub fn id(&self) -> usize {
        self.0.id()
    }

    /// Gets this node's type as a string.
    pub fn kind(&self) -> &'static str {
        self.0.kind()
    }

    /// Gets this node's type as a numerical id.
    pub fn kind_id(&self) -> u16 {
        self.0.kind_id()
    }

    /// Gets this node's text.
    pub fn utf8_text(&self, data: &'a [u8]) -> Option<&'a str> {
        self.0.utf8_text(data).ok()
    }

    /// Gets the byte offsets where this node starts. 
    pub fn start_byte(&self) -> usize {
        self.0.start_byte()
    }

    /// Gets the byte offsets where this node end. 
    pub fn end_byte(&self) -> usize {
        self.0.end_byte()
    }

    /// Gets this node's start position in terms of rows and columns.
    pub fn start_position(&self) -> (usize, usize) {
        let temp = self.0.start_position();
        (temp.row, temp.column)
    }

    /// Gets this node's end position in terms of rows and columns.
    pub fn end_position(&self) -> (usize, usize) {
        let temp = self.0.end_position();
        (temp.row, temp.column)
    }

    /// Gets this node's start position in terms of rows and columns.
    pub fn start_row(&self) -> usize {
        self.0.start_position().row
    }

    /// Gets this node's end position in terms of rows and columns.
    pub fn end_row(&self) -> usize {
        self.0.end_position().row
    }

    /// Gets this node's immediate parent.
    pub fn parent(&self) -> Option<Node<'a>> {
        self.0.parent().map(Node)
    }

    /// Checks where this node has a sibling with the given id.
    #[inline(always)]
    pub fn has_sibling(&self, id: u16) -> bool {
        self.0.parent().map_or(false, |parent| {
            self.0
                .children(&mut parent.walk())
                .any(|child| child.kind_id() == id)
        })
    }

    /// Gets this node's previous sibling.
    pub fn previous_sibling(&self) -> Option<Node<'a>> {
        self.0.prev_sibling().map(Node)
    }

    /// Gets this node's previous sibling.
    pub fn next_sibling(&self) -> Option<Node<'a>> {
        self.0.next_sibling().map(Node)
    }

    /// Checks where this node has a child with the given id.
    #[inline(always)]
    pub fn is_child(&self, id: u16) -> bool {
        self.0
            .children(&mut self.0.walk())
            .any(|child| child.kind_id() == id)
    }

    /// Gets this node's number of children.
    pub fn child_count(&self) -> usize {
        self.0.child_count()
    }

    /// Gets the first child with the given field name.
    pub fn child_by_field_name(&self, name: &str) -> Option<Node> {
        self.0.child_by_field_name(name).map(Node)
    }

    /// Gets the node's child at the given position, where zero represents the first child.
    pub fn child(&self, pos: usize) -> Option<Node<'a>> {
        self.0.child(pos).map(Node)
    }

    /// Gets this node's children.
    pub fn children(&self) -> impl ExactSizeIterator<Item = Node<'a>> {
        let mut cursor = self.cursor();
        cursor.goto_first_child();
        (0..self.child_count()).map(move |_| {
            let result = cursor.node();
            cursor.goto_next_sibling();
            result
        })
    }

    /// Gets this node's cursor.
    pub fn cursor(&self) -> Cursor<'a> {
        Cursor(self.0.walk())
    }

    /// Gets this node's parent.
    #[allow(dead_code)]
    pub fn get_parent(&self, level: usize) -> Option<Node<'a>> {
        let mut level = level;
        let mut node = *self;
        while level != 0 {
            if let Some(parent) = node.parent() {
                node = parent;
            } else {
                return None;
            }
            level -= 1;
        }

        Some(node)
    }

    /// Counts how many ancestors meet the `check` predicate until one meets the `stop` one.
    pub fn count_specific_ancestors<T: crate::ParserTrait>(
        &self,
        check: fn(&Node) -> bool,
        stop: fn(&Node) -> bool,
    ) -> usize {
        let mut count = 0;
        let mut node = *self;
        while let Some(parent) = node.parent() {
            if stop(&parent) {
                break;
            }
            if check(&parent) && !T::Checker::is_else_if(&parent) {
                count += 1;
            }
            node = parent;
        }
        count
    }

    /// Checks wheter this node has an ancestor that meets the given predicate.
    pub fn has_ancestor<F: Fn(&Node<'a>) -> bool>(&self, pred: F) -> bool {
        if let Some(parent) = self.parent() {
            let mut ancestors = vec![parent];
            while let Some(ancestor) = ancestors.pop() {
                if pred(&ancestor) {
                    return true;
                } else {
                    if let Some(p) = ancestor.parent() {
                        ancestors.push(p);
                    }
                }
            }
        }

        false
    }

    /// Checks wheter this node's parent meets the `typ` predicate and the parent of the node's parent meets the `typs` predicate.
    pub fn has_ancestors(&self, typ: fn(&Node) -> bool, typs: fn(&Node) -> bool) -> bool {
        let mut res = false;
        let mut node = *self;
        if let Some(parent) = node.parent() {
            if typ(&parent) {
                node = parent;
            }
        }
        if let Some(parent) = node.parent() {
            if typs(&parent) {
                res = true;
            }
        }
        res
    }
}

/// An `AST` cursor.
#[derive(Clone)]
pub struct Cursor<'a>(TreeCursor<'a>);

impl<'a> Cursor<'a> {
    pub(crate) fn reset(&mut self, node: &Node<'a>) {
        self.0.reset(node.0);
    }

    pub(crate) fn goto_next_sibling(&mut self) -> bool {
        self.0.goto_next_sibling()
    }

    pub(crate) fn goto_first_child(&mut self) -> bool {
        self.0.goto_first_child()
    }

    pub(crate) fn node(&self) -> Node<'a> {
        Node(self.0.node())
    }
}

impl<'a> Search<'a> for Node<'a> {
    fn first_occurence<F: Fn(&Node<'a>) -> bool>(&self, pred: F) -> Option<Node<'a>> {
        let mut cursor = self.cursor();
        let mut stack = Vec::new();
        let mut children = Vec::new();

        stack.push(*self);

        while let Some(node) = stack.pop() {
            if pred(&node) {
                return Some(node);
            }
            cursor.reset(&node);
            if cursor.goto_first_child() {
                loop {
                    children.push(cursor.node());
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
                for child in children.drain(..).rev() {
                    stack.push(child);
                }
            }
        }

        None
    }

    fn all_occurrences<F: Fn(&Node<'a>) -> bool>(&self, pred: F) -> Vec<Node<'a>> {
        let mut nodes = Vec::new();
        let mut cursor = self.cursor();
        let mut stack = Vec::new();
        let mut children = Vec::new();

        stack.push(*self);

        while let Some(node) = stack.pop() {
            if pred(&node) {
                nodes.push(node);
            }
            cursor.reset(&node);
            if cursor.goto_first_child() {
                loop {
                    children.push(cursor.node());
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
                for child in children.drain(..).rev() {
                    stack.push(child);
                }
            }
        }

        nodes
    }

    fn act_on_node(&self, action: &mut dyn FnMut(&Node<'a>)) {
        let mut cursor = self.cursor();
        let mut stack = Vec::new();
        let mut children = Vec::new();

        stack.push(*self);

        while let Some(node) = stack.pop() {
            action(&node);
            cursor.reset(&node);
            if cursor.goto_first_child() {
                loop {
                    children.push(cursor.node());
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
                for child in children.drain(..).rev() {
                    stack.push(child);
                }
            }
        }
    }

    fn first_child<F: Fn(&Node<'a>) -> bool>(&self, pred: F) -> Option<Node<'a>> {
        self.children().find(|&child| pred(&child))
    }

    fn act_on_child(&self, action: &mut dyn FnMut(&Node<'a>)) {
        for child in self.children() {
            action(&child);
        }
    }
}
