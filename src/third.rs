// Creating a persistent list

// A common workload for a persistent list is something like this
// list1 = A -> B -> C -> D
// list2 = tail(list1) = B -> C -> D
// list3 = push(list2, X) = X -> B -> C -> D

// which would result in something that looks like this

/*
list1 -> A ------+
                 |
                 v
list2 ---------> B -> C -> D
                 ^
                 |
list3 -> X ------+
*/

// This will not work with Boxes, the ownership of B is shared by all 3 lists
// If list2 is dropped, should it free B? With boxes, it would need to

// In higher level languages, garbage collection would be watching B and
// deallocate B when everything looking at B stops looking at B

// Rust must get this behavior from the only tool it has, which is Reference Counting

// Reference counting is a simplistic form of garbage collection, and it's really slow
// RC is just like box, but it can be duplicated
// and its memory will only be freed when all the RCs derived from it are dropped
// The cost of this is only a shared reference to its internals can be taken
// This means we can never actually get data out of one of our lists,
// and they can't be mutated either

use std::sync::Arc;

type Link<T> = Option<Arc<Node<T>>>;

struct Node<T> {
  elem: T,
  next: Link<T>,
}

pub struct List<T> {
  head: Link<T>,
}

// Previous list's implementation of Drop
// we pull the data out of each Box as mutable, which will not be possible
// with our Rc storage in this implementation
// impl<T> Drop for List<T> {
//   fn drop(&mut self) {
//     let mut cur_link = self.head.take();
//     while let Some(mut boxed_node) = cur_link {
//       // This is mutating the Node inside the box
//       // not able to be done with an Rc as Rc doesn't implement DerefMut
//       cur_link = boxed_node.next.take();
//     }
//   }
// }

// This implementation of drop will iteratively traverse a the list, dropping
// values along the way until a node is reached in which more than one strong
// reference exists, at which point it will stop.
// If this never happens, we'll go until head is None, which indicates the
// end of the list
impl<T> Drop for List<T> {
  fn drop(&mut self) {
    let mut head = self.head.take();
    while let Some(node) = head {
      if let Ok(mut node) = Arc::try_unwrap(node) {
        head = node.next.take();
      } else {
        break;
      }
    }
  }
}

impl<T> List<T> {
  pub fn new() -> Self {
    List { head: None }
  }

  // Approximation of push
  // Takes a List and an Element to prepend
  // this will create a new list with the old list as its next value
  // the only difference is how to get the next value, as mutation isn't allowed
  // with an `Rc`
  pub fn prepend(&self, elem: T) -> List<T> {
    List {
      head: Some(Arc::new(Node {
        elem: elem,
        next: self.head.clone(),
      })),
    }
  }

  // Approximation of pop
  // Inverse of prepend, a list is taken in and returns the whole list with the
  // first element removed
  // Clone the second element in the list if it exists
  pub fn tail(&self) -> List<T> {
    List {
      // and_then() is another common pattern with Options
      // returns None if the Option is None
      // Otherwise, it calls our closure with the wrapped value
      // map's signature:
      // `fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U>`
      // `fn and_then<U, F: FnOnce(T) -> Option<U>>(self, f: F) -> Option<U>

      // the longform of and_then is something like
      /*
        match option {
          None => None,
          val => match val.map(f1) {
            None => None,
            val => match val.map(f2) {
              None => None,
              val => va.map(f3)
            }
          }
        }
      */
      // the above can be written like this, instead
      // option.and_then(f1).and_then(f2).and_then(f3);
      // head: match self.head.as_ref() {
      //   None => None,
      //   val => match val.map(|node| node.next.clone()) {
      //     None => None,
      //     Some(val) => val,
      //   },
      // },

      // and_then is called flatmap in other languages
      head: self.head.as_ref().and_then(|node| node.next.clone()),
    }
  }
  // head returns a reference to the first element.
  // basically just peek
  pub fn head(&self) -> Option<&T> {
    self.head.as_ref().map(|node| &node.elem)
  }
}

// iter is identical to how it was for the mutable list
pub struct Iter<'a, T> {
  next: Option<&'a Node<T>>,
}

impl<T> List<T> {
  pub fn iter(&self) -> Iter<'_, T> {
    Iter {
      next: self.head.as_deref(),
    }
  }
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    self.next.map(|node| {
      self.next = node.next.as_deref();
      &node.elem
    })
  }
}

// IntoIter and IterMut can't be implemented for this type
// we only have shared access to elements

// pub struct IterMut<'a, T> {
//   next: Option<&'a mut Node<T>>,
// }

// impl<T> List<T> {
//   pub fn iter_mut(&mut self) -> IterMut<'_, T> {
//     IterMut {
//       // This can't be done as Rc doesn't implement the DerefMut trait
//       next: self.head.as_deref_mut(),
//     }
//   }
// }

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn basics() {
    let l = List::new();
    assert_eq!(l.head(), None);

    let l = l.prepend(1).prepend(2).prepend(3);

    assert_eq!(l.head(), Some(&3));

    let l = l.tail();

    assert_eq!(l.head(), Some(&2));

    let l = l.tail();
    assert_eq!(l.head(), Some(&1));

    let l = l.tail();
    assert_eq!(l.head(), None);

    let l = l.tail();
    assert_eq!(l.head(), None);
  }

  #[test]
  fn iter() {
    let l = List::new().prepend(1).prepend(2).prepend(3);
    let mut iter = l.iter();
    for x in (1..4).rev() {
      assert_eq!(iter.next(), Some(&x));
    }

    assert_eq!(iter.next(), None);
  }

  // Understanding take()
  #[test]
  fn opt_test() {
    let mut m = Some(5);
    let v = m.take();
    assert_eq!(m, None);
    assert_eq!(v, Some(5));
  }

  // #[test]
  // fn overflow() {
  //   let mut l = List::new();
  //   for n in 0..500000 {
  //     l = l.prepend(n);
  //   }
  //   assert_eq!(l.head(), Some(&(500000 - 1)));
  // }
}

// Thread safety
// A type is thread-safe if it implements Send and Sync traits
// These are inherited automatically if all of the components of a type
// are Send and Sync, similarly to Copy
// A type is send if it's safe to move to another thread
// A type is Sync if it's safe to share between multiple threads

// If T is Sync, &T is implicitly Send
// safe in this context means impossible to cause data races

// Send and Sync are marker traits, which means they're traits with absolutely
// no interface. A type is either Send or it isn't. It's a property other APIs
// can require of a type. If a type isn't send, it's statically impossible
// to be sent to a different thread

// most types are Send and Sync
// Most are send because they totally own their data.
// Most are Sync because the only way to share data across threads is to put
// them behind a shared reference, making them immutable

// Some types have interior mutability, which breaks these properties
// inherited mutability, in contrast, means the mutability of a value is
// inherited from the mutability of its container

// Interior mutability allows you to mutate through a shared reference
// Two classes of interior mutability
// - Cells: only work in a single-threaded context
// - Locks: work in a multi-threaded context

// cells are cheaper when they can be used.

// atomics are also an available primitive that acts like a lock

// Rc and Arc both use interior mutability for their reference count
// and this reference count is shared between every instance
// Rc uses a cell to accomplish this, making it not thread safe
// Arc uses an atomic, making it thread safe.
