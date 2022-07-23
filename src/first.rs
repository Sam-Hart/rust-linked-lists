use std::mem;

struct Node {
  elem: i32,
  next: Link,
}

enum Link {
  Empty,
  More(Box<Node>),
}

pub struct List {
  head: Link,
}

impl List {
  pub fn new() -> Self {
    List { head: Link::Empty }
  }

  pub fn push(&mut self, elem: i32) {
    let new_n = Box::new(Node {
      elem: elem,
      next: mem::replace(&mut self.head, Link::Empty),
    });
    self.head = Link::More(new_n);
  }

  pub fn pop(&mut self) -> Option<i32> {
    match mem::replace(&mut self.head, Link::Empty) {
      Link::Empty => None,
      Link::More(node) => {
        self.head = node.next;
        Some(node.elem)
      }
    }
  }
}

// problem: when List is dropped, it will drop head, which will recursively
// drop child nodes until it reaches the end
// This can result in a stack overflow
// demonstrated by this test
/*
  #[test]
  fn overflow() {
    let mut l = List::new();
    for n in 0..500000 {
      l.push(n);
    }
    assert_eq!(l.pop(), Some(500000 - 1));
  }
*/
impl Drop for List {
  fn drop(&mut self) {
    let mut cur_link = mem::replace(&mut self.head, Link::Empty);
    while let Link::More(mut boxed_node) = cur_link {
      cur_link = mem::replace(&mut boxed_node.next, Link::Empty);
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn basics() {
    let mut l = List::new();
    assert_eq!(l.pop(), None);

    l.push(1);
    l.push(2);
    l.push(3);

    assert_eq!(l.pop(), Some(3));
    assert_eq!(l.pop(), Some(2));

    l.push(4);
    l.push(5);

    assert_eq!(l.pop(), Some(5));
    assert_eq!(l.pop(), Some(4));

    assert_eq!(l.pop(), Some(1));
    assert_eq!(l.pop(), None);
  }
}
