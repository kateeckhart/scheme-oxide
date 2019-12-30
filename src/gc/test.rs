use std::cell::Cell;
use std::mem;
use std::ptr;
use std::slice;

use lazy_static::lazy_static;

use crate::gc::{GcHeap, GcObj, GcObjHead, GcVtable, Traceable};

struct LargeObject(usize);

unsafe impl Traceable for LargeObject {
    fn extra_size(&self) -> usize {
        self.0
    }

    fn get_vtable() -> &'static GcVtable {
        lazy_static! {
            static ref VTABLE: GcVtable = LargeObject::gen_vtable();
        }
        &VTABLE
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
struct TestGcLeaf(u8);

unsafe impl Traceable for TestGcLeaf {
    fn get_vtable() -> &'static GcVtable {
        lazy_static! {
            static ref VTABLE: GcVtable = TestGcLeaf::gen_vtable();
        }
        &VTABLE
    }
}

struct GcContainer {
    one: Cell<*const GcObj<TestGcLeaf>>,
    two: Cell<*const GcObj<TestGcLeaf>>,
    three: Cell<*const GcObj<TestGcLeaf>>,
}

unsafe impl Traceable for GcContainer {
    unsafe fn for_each_pointer<T>(&self, mut fun: T)
    where
        T: FnMut(*const GcObjHead) -> *const GcObjHead,
    {
        self.one.set(GcObjHead::downcast_ptr::<TestGcLeaf>(fun(
            GcObj::erase_ptr(self.one.get()),
        )));
        self.two.set(GcObjHead::downcast_ptr::<TestGcLeaf>(fun(
            GcObj::erase_ptr(self.two.get()),
        )));
        self.three.set(GcObjHead::downcast_ptr::<TestGcLeaf>(fun(
            GcObj::erase_ptr(self.three.get()),
        )));
    }

    fn get_vtable() -> &'static GcVtable {
        lazy_static! {
            static ref VTABLE: GcVtable = GcContainer::gen_vtable();
        }
        &VTABLE
    }
}

#[test]
fn main_test() {
    unsafe {
        let gc = GcHeap::new(1024 * 400, 10);
        drop(gc.put_on_heap(TestGcLeaf(8)).unwrap());
        let obj1 = gc.put_on_heap(TestGcLeaf(0)).unwrap();
        let obj2 = gc.put_on_heap(TestGcLeaf(1)).unwrap();
        drop(gc.put_on_heap(TestGcLeaf(48)).unwrap());
        let obj3 = gc.put_on_heap(TestGcLeaf(2)).unwrap();
        assert_eq!((*obj1.to_ptr()).inner, TestGcLeaf(0));
        assert_eq!((*obj2.to_ptr()).inner, TestGcLeaf(1));
        assert_eq!((*obj3.to_ptr()).inner, TestGcLeaf(2));
        drop(
            gc.put_on_heap(LargeObject(
                1024 * 400
                    - mem::size_of::<GcObj<TestGcLeaf>>() * 3
                    - mem::size_of::<GcObj<LargeObject>>(),
            ))
            .unwrap(),
        );
        let container = gc
            .put_on_heap(GcContainer {
                one: Cell::new(obj1.to_ptr()),
                two: Cell::new(obj2.to_ptr()),
                three: Cell::new(obj3.to_ptr()),
            })
            .unwrap();
        drop(obj1);
        drop(obj2);
        drop(obj3);
        for _ in 0..50 {
            drop(gc.put_on_heap(LargeObject(1024 * 300)))
        }
        let container_ref = &(container.to_ref()).inner;
        assert_eq!((*container_ref.one.get()).inner, TestGcLeaf(0));
        assert_eq!((*container_ref.two.get()).inner, TestGcLeaf(1));
        assert_eq!((*container_ref.three.get()).inner, TestGcLeaf(2));
    }
}

#[test]
fn out_of_ram() {
    unsafe {
        let gc = GcHeap::new(1024 * 400, 2);
        let _obj1 = gc.put_on_heap(LargeObject(1024 * 300)).unwrap();
        gc.put_on_heap(LargeObject(1024 * 300)).err().unwrap()
    }
}

struct DynamicSizedHead {
    size: usize,
}

impl DynamicSizedHead {
    unsafe fn leafs_ref(&self) -> &[Cell<*const GcObj<TestGcLeaf>>] {
        slice::from_raw_parts(
            (self as *const _ as *const u8)
                .add(mem::size_of::<DynamicSizedHead>())
                .cast::<Cell<*const GcObj<TestGcLeaf>>>(),
            self.size,
        )
    }
}

unsafe impl Traceable for DynamicSizedHead {
    unsafe fn for_each_pointer<T>(&self, mut fun: T)
    where
        T: FnMut(*const GcObjHead) -> *const GcObjHead,
    {
        for leaf in self
            .leafs_ref()
            .iter()
            .take_while(|leaf| !leaf.get().is_null())
        {
            leaf.set(GcObjHead::downcast_ptr::<TestGcLeaf>(fun(
                GcObj::erase_ptr(leaf.get()),
            )))
        }
    }

    fn extra_size(&self) -> usize {
        mem::size_of::<*const GcObj<TestGcLeaf>>() * self.size
    }

    fn init_extra(&mut self) {
        unsafe {
            let child_ptr = (self as *mut _ as *mut u8)
                .add(mem::size_of::<DynamicSizedHead>())
                .cast::<*const GcObj<TestGcLeaf>>();

            for x in 0..self.size {
                ptr::write(child_ptr.add(x), ptr::null())
            }
        }
    }

    fn get_vtable() -> &'static GcVtable {
        lazy_static! {
            static ref VTABLE: GcVtable = DynamicSizedHead::gen_vtable();
        }
        &VTABLE
    }
}

#[test]
fn dynamic_sized() {
    unsafe {
        let gc = GcHeap::new(1024 * 40, 10);
        let _zero_children = gc.put_on_heap(DynamicSizedHead { size: 0 }).unwrap();
        //Induce collection during child allocation
        drop(gc.put_on_heap(LargeObject(
            1024 * 40 - mem::size_of::<DynamicSizedHead>() * 2 - mem::size_of::<usize>() * 1090,
        )));

        let many_children = gc.put_on_heap(DynamicSizedHead { size: 1024 }).unwrap();

        for x in 0..1024 {
            let child = gc.put_on_heap(TestGcLeaf(x as u8)).unwrap();
            many_children.to_ref().inner.leafs_ref()[x].set(child.to_ptr());
        }

        for _ in 0..50 {
            drop(gc.put_on_heap(LargeObject(1024 * 3)).unwrap())
        }

        for (i, leaf) in many_children.to_ref().inner.leafs_ref().iter().enumerate() {
            assert_eq!(i as u8, (*leaf.get()).inner.0)
        }
    }
}
