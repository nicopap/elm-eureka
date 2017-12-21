//! A clone of itertools' peeking_take_while but ignores the first element
use itertools::PeekingNext;

#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
pub struct AppartFirstTake<'a, I:'a, F>
    where I: Iterator,
{
    handled_first: bool,
    iter: &'a mut I,
    f: F
}

/// Create a new `AppartFirst` iterator.
pub fn appart_first_take<I, F>(iter: &mut I, f: F) -> AppartFirstTake<I, F>
    where I: Iterator
{
    AppartFirstTake { iter, f, handled_first: false }
}

impl<'a, I, F> Iterator for AppartFirstTake<'a, I, F>
    where I: PeekingNext,
          F: FnMut(&I::Item) -> bool
{
    type Item =I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.handled_first {
            self.handled_first = true;
            self.iter.next()
        } else {
            self.iter.peeking_next(&mut self.f)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (_,hi) = self.iter.size_hint();
        (0, hi)
    }
}

pub trait Iterplus : Iterator {
    fn appart_first_take<F>(&mut  self, accept: F) -> AppartFirstTake<Self, F>
        where Self: Sized + PeekingNext,
              F: FnMut(&Self::Item) -> bool,
        {
            appart_first_take(self, accept)
        }
}

impl<T: ?Sized> Iterplus for T where T : Iterator {}
