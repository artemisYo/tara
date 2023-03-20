use std::cell::{Ref, RefCell};
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
pub struct InitOnce<T: 'static + std::default::Default> {
    data: RefCell<Option<T>>,
    is_init: AtomicBool,
    func: RefCell<&'static dyn Fn() -> T>,
}
unsafe impl<T: Sync + std::default::Default> Sync for InitOnce<T> {}
impl<T: std::default::Default> InitOnce<T> {
    pub const fn new(init: &'static dyn Fn() -> T) -> Self {
        return Self {
            data: RefCell::new(None),
            is_init: AtomicBool::new(false),
            func: RefCell::new(init),
        };
    }
    pub fn get<'a>(&'a self) -> Ref<Option<T>> {
        if self
            .is_init
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            *self.data.borrow_mut() = Some((self.func.borrow())());
        }
        return self.data.borrow();
    }
}
