use std::{
    any::Any,
    ops::{Deref, DerefMut, CoerceUnsized},
    marker::Unsize,
    convert::AsRef,
    fmt,
};
use smallbox::{SmallBox, space::S4};

#[repr(transparent)]
#[derive(Clone)]
pub struct BoxVal<T: ?Sized>(SmallBox<T, S4>);

impl<T> BoxVal<T> {
    pub fn new(item: T) -> Self {
        BoxVal(SmallBox::new(item))
    }

    pub unsafe fn from_raw(raw: *mut T) -> BoxVal<T> {
        BoxVal(SmallBox::new_unchecked(raw.read(), raw))
    }

    pub fn take(mut self) -> T {
        let x = unsafe { (self.deref_mut() as *mut T).read() };
        std::mem::forget(self);
        x
    }
}

impl<T: ?Sized> BoxVal<T> {
    pub fn into_raw(mut this: BoxVal<T>) -> *mut T {
        this.deref_mut() as *mut _
    }
}

impl BoxVal<dyn Any> {
    pub fn downcast<T: Any>(self) -> Result<BoxVal<T>, Self> {
        Ok(BoxVal(self.0.downcast().map_err(BoxVal)?))
    }
}

impl<T: ?Sized> Deref for BoxVal<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<T: ?Sized> DerefMut for BoxVal<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut()
    }
}

impl<T: ?Sized> AsRef<T> for BoxVal<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T: ?Sized> AsMut<T> for BoxVal<T> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for BoxVal<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T: ?Sized + Iterator> Iterator for BoxVal<T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> From<T> for BoxVal<T> {
    fn from(item: T) -> Self {
        BoxVal(SmallBox::new(item))
    }
}

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<BoxVal<U>> for BoxVal<T> {}
