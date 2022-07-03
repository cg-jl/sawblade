use std::{
    alloc::Allocator,
    iter::TrustedLen,
    mem::{ManuallyDrop, MaybeUninit},
    slice::SliceIndex,
};

#[derive(Clone)]
pub struct FixedArray<T>(Box<[T]>);

impl<T> std::ops::Deref for FixedArray<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> FixedArray<T> {
    pub fn single(value: T) -> Self {
        Self({
            let mut slice = Box::new_uninit_slice(1);
            slice[0].write(value);
            unsafe { slice.assume_init() }
        })
    }
    pub fn empty() -> Self {
        Self(Box::from([]))
    }
}

impl<T, I: SliceIndex<[T]>> std::ops::Index<I> for FixedArray<T> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<Coll, Iter, T> From<Coll> for FixedArray<T>
where
    Coll: IntoIterator<Item = T, IntoIter = Iter>,
    Iter: Iterator<Item = T> + ExactSizeIterator + TrustedLen,
{
    fn from(coll: Coll) -> Self {
        let iter = coll.into_iter();
        let len = iter.len();
        Self({
            let mut array = Box::new_uninit_slice(len);
            for (target, value) in array.iter_mut().zip(iter) {
                target.write(value);
            }
            // SAFE: we know from `TrustedLen` that the length that is
            // reported by `len` is its actual bounded size.
            unsafe { array.assume_init() }
        })
    }
}
