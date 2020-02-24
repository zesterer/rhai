use std::any::{type_name, Any as StdAny, TypeId};
use std::fmt;

pub trait Any: StdAny {
    fn type_id(&self) -> TypeId;

    fn type_name(&self) -> String;

    fn box_clone(&self) -> Box<dyn Any>;

    /// This type may only be implemented by `rhai`.
    #[doc(hidden)]
    fn _closed(&self) -> _Private;
}

impl<T> Any for T
where
    T: Clone + StdAny + ?Sized,
{
    #[inline]
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }

    fn type_name(&self) -> String {
        type_name::<T>().to_string()
    }

    #[inline]
    fn box_clone(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    fn _closed(&self) -> _Private {
        _Private
    }
}

impl dyn Any {
    //#[inline]
    // fn box_clone(&self) -> Box<dyn Any> {
    //     Any::box_clone(self)
    // }
    #[inline]
    pub fn is<T: Any>(&self) -> bool {
        let t = TypeId::of::<T>();
        let boxed = <dyn Any as Any>::type_id(self);

        t == boxed
    }

    #[inline]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const dyn Any as *const T)) }
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(&mut *(self as *mut dyn Any as *mut T)) }
        } else {
            None
        }
    }
}

impl Clone for Box<dyn Any> {
    fn clone(&self) -> Self {
        Any::box_clone(self.as_ref() as &dyn Any)
    }
}

impl fmt::Debug for dyn Any {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("?")
    }
}

pub trait AnyExt: Sized {
    fn downcast<T: Any + Clone>(self) -> Result<Box<T>, Self>;
}

impl AnyExt for Box<dyn Any> {
    fn downcast<T: Any + Clone>(self) -> Result<Box<T>, Self> {
        if self.is::<T>() {
            unsafe {
                let raw: *mut dyn Any = Box::into_raw(self);
                Ok(Box::from_raw(raw as *mut T))
            }
        } else {
            Err(self)
        }
    }
}

/// Private type which ensures that `rhai::Any` can only
/// be implemented by this crate.
#[doc(hidden)]
pub struct _Private;
