use std::mem;
use std::ptr;
use std::slice;

use lazy_static::lazy_static;

use crate::gc::{GcHeap, GcObj, GcObjHead, GcVtable, Traceable};

struct LargeObject(usize);

unsafe impl Traceable for LargeObject {
    fn size(&mut self) -> usize {
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
    one: *mut GcObj<TestGcLeaf>,
    two: *mut GcObj<TestGcLeaf>,
    three: *mut GcObj<TestGcLeaf>,
}

unsafe impl Traceable for GcContainer {
    fn for_each_pointer<T>(&mut self, mut fun: T)
        where
            T: FnMut(&mut *mut GcObjHead),
    {
        unsafe {
            fun(&mut *(&mut self.one as *mut _ as *mut *mut GcObjHead));
            fun(&mut *(&mut self.two as *mut _ as *mut *mut GcObjHead));
            fun(&mut *(&mut self.three as *mut _ as *mut *mut GcObjHead));
        }
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
                1024 * 400 - mem::size_of::<GcObj<TestGcLeaf>>() * 3,
            ))
                .unwrap(),
        );
        let container = gc
            .put_on_heap(GcContainer {
                one: obj1.to_ptr(),
                two: obj2.to_ptr(),
                three: obj3.to_ptr(),
            })
            .unwrap();
        drop(obj1);
        drop(obj2);
        drop(obj3);
        for _ in 0..50 {
            drop(gc.put_on_heap(LargeObject(1024 * 300)))
        }
        let container_ref = &(*container.to_ptr()).inner;
        assert_eq!((*container_ref.one).inner, TestGcLeaf(0));
        assert_eq!((*container_ref.two).inner, TestGcLeaf(1));
        assert_eq!((*container_ref.three).inner, TestGcLeaf(2));
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
    unsafe fn leafs_mut(&mut self) -> &mut [*mut GcObj<TestGcLeaf>] {
        slice::from_raw_parts_mut(
            (self as *mut _ as *mut u8)
                .add(mem::size_of::<DynamicSizedHead>())
                .cast::<*mut GcObj<TestGcLeaf>>(),
            self.size,
        )
    }
}

unsafe impl Traceable for DynamicSizedHead {
    fn for_each_pointer<T>(&mut self, mut fun: T)
        where
            T: FnMut(&mut *mut GcObjHead),
    {
        for leaf in unsafe { self.leafs_mut() }.iter_mut().take_while(|leaf| !leaf.is_null()) {
            unsafe {
                fun(&mut *(leaf as *mut _ as *mut *mut GcObjHead));
            }
        }
    }

    fn size(&mut self) -> usize {
        let base_size = mem::size_of::<GcObj<DynamicSizedHead>>();
        base_size + mem::size_of::<usize>() * self.size
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
        drop(gc.put_on_heap(LargeObject(1024 * 40 - mem::size_of::<DynamicSizedHead>() * 2 - mem::size_of::<usize>() * 1090)));

        let many_children = gc.put_on_heap(DynamicSizedHead { size: 1024 }).unwrap();

        let children_ptr = (&mut (*many_children.to_ptr()).inner as *mut _ as *mut u8)
            .add(mem::size_of::<DynamicSizedHead>())
            .cast::<*mut GcObj<TestGcLeaf>>();
        for x in 0..1024 {
            ptr::write(
                children_ptr.add(x),
                ptr::null_mut(),
            )
        }

        for x in 0..1024 {
            (*many_children.to_ptr()).inner.leafs_mut()[x] = gc.put_on_heap(TestGcLeaf(x as u8)).unwrap().to_ptr();
        }

        for _ in 0..50 {
            drop(gc.put_on_heap(LargeObject(1024 * 3)).unwrap())
        }

        for (i, leaf) in (*many_children.to_ptr()).inner.leafs_mut().iter().enumerate() {
            assert_eq!(i as u8, (**leaf).inner.0)
        }
    }
}
