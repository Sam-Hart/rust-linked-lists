struct Node<T> {
  elem: T,
  next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

pub struct List<T> {
  head: Link<T>,
}

impl<T> List<T> {
  pub fn new() -> Self {
    List { head: None }
  }

  pub fn push(&mut self, elem: T) {
    let new_n = Box::new(Node {
      elem: elem,
      next: self.head.take(),
    });
    self.head = Some(new_n);
  }

  pub fn pop(&mut self) -> Option<T> {
    self.head.take().map(|node| {
      self.head = node.next;
      node.elem
    })
  }

  pub fn peek(&self) -> Option<&T> {
    self.head.as_ref().map(|node| &node.elem)
  }

  pub fn peek_mut(&mut self) -> Option<&mut T> {
    self.head.as_mut().map(|node| &mut node.elem)
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
impl<T> Drop for List<T> {
  fn drop(&mut self) {
    let mut cur_link = self.head.take();
    while let Some(mut boxed_node) = cur_link {
      cur_link = boxed_node.next.take();
    }
  }
}

// Tuple struct
// Tuple structs have a name, but their fields have no name
// hybrid between a struct and a tuple

// Almost always better to use a struct except in the case of the struct
// having only one field
// This is the `newtype` pattern, named as such
// because a new type is created, distinct from its contained value,
// and expresses some semantic meaning
pub struct IntoIter<T>(List<T>);

impl<T> List<T> {
  pub fn into_iter(self) -> IntoIter<T> {
    IntoIter(self)
  }
}

impl<T> Iterator for IntoIter<T> {
  type Item = T;
  fn next(&mut self) -> Option<Self::Item> {
    self.0.pop()
  }
}

// Implementing Iter
// Hold a pointer to the current Node we want to yield next
// That node might not exist, so this should be an Option
// When an element is yielded, proceed to the current node's next

// Iter is generic over some lifetime, it doesn't care
pub struct Iter<'a, T> {
  next: Option<&'a Node<T>>,
}

// No lifetime for the impl, List has no associated lifetimes
impl<T> List<T> {
  // Declare a fresh lifteime here for the exact borrow that
  // creates the iter. Now &self must be valid as long as the Iter is around
  // pub fn iter<'a>(&'a self) -> Iter<'a, T> {
  //   Iter {
  //     next: self.head.as_deref(),
  //   }
  // }

  // this method signature can be rewritten to elide lifetimes
  // Lifetime elision rules are as follows
  // - Each elided lifetime in input position becomes a distinct lifetime param
  // - if there is exactly one input lifetime position (elided or not),
  //   then that lifetime is assigned to all elided output lifetimes
  // - If there are multiple input lifetime positions, but one of them is &self,
  //   then the lifetime of self is assigned to all elided output lifetimes
  // - if any of the above rules can't be met, it's an error to elide
  //   output an output lifetime

  // for fn definitions, fn types, and the following traits: `Fn`, `FnMut`, and `FnOnce`
  // an input refers to the types of the formal args, output refers to the result types
  // ex: `fn foo(s: &str) -> (&str, &str) has elided one lifetime input position
  //    and two lifetimes in output position.
  // Note: the input positions of a `fn` method definition do not include the lifetimes
  // that occur in the method's `impl` header

  // for `impl` headers, all types are input.
  // ex: `impl Trait<&T> for Struct<&T>` has elided two lifetimes in input position
  // ex: `impl Struct<&T>` has elided one

  // expanded form of this method:
  // `pub fn iter<'a>(&'a self) -> Iter<'a, T>`
  pub fn iter(&self) -> Iter<'_, T> {
    Iter {
      next: self.head.as_deref(),
    }
  }
}

// We do have a lifetime here because iter has one that we neeed to define
impl<'a, T> Iterator for Iter<'a, T> {
  // Lifetime here as this is a type declaration
  type Item = &'a T;

  // None of this needs to change, Self gets its lifetime from Iter<'a, T>
  // declaration

  // Desugared
  // shows that there is no relation between the lifetime of the output
  // and the lifetime of the input
  // fn next<'b>(&'b mut self) -> Option<&'a T>
  fn next(&mut self) -> Option<Self::Item> {
    self.next.map(|node| {
      // This type of deref can be avoided with deref coercion
      // self.next = node.next.as_ref().map(|node| &**node);

      // deref coercion can be avoided with two methods:
      // 1) The turbofish ::<>
      // tells the compiler what we think the types of generics are
      // Here, we're telling map that it will be emitting an &Node<T>
      // and the other type is unimportant/doesn't matter
      // Telling the compiler this lets rust know that &node should have
      // deref coercion applied to it, meaning we don't have to deref with *'s
      // ourselves

      // self.next = node.next.as_ref().map::<&Node<T>, _>(|node| &node);

      // 2) Another possibility is to use as_deref()
      // This replaces the above needing to do &** as well
      self.next = node.next.as_deref();
      &node.elem
    })
  }
}

pub struct IterMut<'a, T> {
  next: Option<&'a mut Node<T>>,
}

impl<T> List<T> {
  pub fn iter_mut(&mut self) -> IterMut<'_, T> {
    IterMut {
      next: self.head.as_deref_mut(),
    }
  }
}

impl<'a, T> Iterator for IterMut<'a, T> {
  type Item = &'a mut T;

  fn next(&mut self) -> Option<Self::Item> {
    // calling .take() is necessary here because our type
    // `Option<&mut Node<T>>` doesn't implement the Copy trait
    // This was working before in Iterator for Iter because
    // next there is `Option<&Node<T>>`, and shared references _do_ implement
    // the Copy trait as its simply a memory address, and shared references
    // can be referred to in many places. Mutable shared references cannot be
    // referred to in multiple places, but instead just one, and as a result,
    // we must call .take() on next before mutably dereferencing it
    // calling take() removes it from the Option holding onto the value and
    // replaces it with None, meaning an execution of next is guaranteed
    // to get the only reference to a specific element, allowing it to be mut
    self.next.take().map(|node| {
      self.next = node.next.as_deref_mut();
      &mut node.elem
    })
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

  #[test]
  fn peek() {
    let mut l = List::new();
    assert_eq!(l.peek(), None);
    assert_eq!(l.peek_mut(), None);

    l.push(1);
    l.push(2);
    l.push(3);

    assert_eq!(l.peek(), Some(&3));
    assert_eq!(l.peek_mut(), Some(&mut 3));

    l.peek_mut().map(|val| *val = 42);

    assert_eq!(l.peek(), Some(&42));
    assert_eq!(l.pop(), Some(42));
  }

  #[test]
  fn into_iter() {
    let mut l = List::new();
    l.push(1);
    l.push(2);
    l.push(3);

    let mut iter = l.into_iter();

    for x in (1..4).rev() {
      assert_eq!(iter.next(), Some(x));
    }

    assert_eq!(iter.next(), None);
  }

  #[test]
  fn iter() {
    let mut l = List::new();
    l.push(1);
    l.push(2);
    l.push(3);

    let mut iter = l.iter();

    for x in (1..4).rev() {
      assert_eq!(iter.next(), Some(&x));
    }

    assert_eq!(iter.next(), None);
  }

  #[test]
  fn iter_mut() {
    let mut l = List::new();
    l.push(1);
    l.push(2);
    l.push(3);

    let mut iter = l.iter_mut();
    for mut x in (1..4).rev() {
      assert_eq!(iter.next(), Some(&mut x));
    }

    assert_eq!(iter.next(), None);
  }
}
