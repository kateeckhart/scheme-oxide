/*
    Copyright 2019 Alexander Eckhart

    This file is part of scheme-oxide.

    Scheme-oxide is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scheme-oxide is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with scheme-oxide.  If not, see <https://www.gnu.org/licenses/>.
*/

use std::alloc;
use std::alloc::Layout;
use std::cell::{Cell, RefCell};
use std::marker::PhantomData;
use std::mem;
use std::mem::align_of;
use std::ptr;

pub unsafe trait Traceable: Sized {
    fn for_each_pointer<T>(&mut self, _: T)
        where
            T: FnMut(&mut *mut GcObjHead),
    {}

    fn size(&mut self) -> usize {
        mem::size_of::<GcObj<Self>>()
    }

    fn get_vtable() -> &'static GcVtable;

    fn gen_vtable() -> GcVtable {
        unsafe {
            GcVtable {
                size: |head| GcObj::<Self>::mut_from_head(head).inner.size(),
                mark: |head, current_mark| {
                    GcObj::<Self>::mut_from_head(head)
                        .inner
                        .for_each_pointer(|child| {
                            let child_ref = &mut **child;
                            if child_ref.forward.is_null() {
                                child_ref.forward = *current_mark;
                                *current_mark = *child;
                            }
                        })
                },
                update: |head| {
                    GcObj::<Self>::mut_from_head(head)
                        .inner
                        .for_each_pointer(|child| {
                            *child = (**child).forward;
                        })
                },
            }
        }
    }
}

#[cfg(test)]
mod test;

#[derive(Copy, Clone)]
enum RootEntry {
    Object(*mut GcObjHead),
    Free(usize),
}

impl RootEntry {
    fn to_option(self) -> Option<*mut GcObjHead> {
        match self {
            RootEntry::Object(obj) => Some(obj),
            RootEntry::Free(_) => None,
        }
    }

    fn get_free(self) -> Option<usize> {
        match self {
            RootEntry::Object(_) => None,
            RootEntry::Free(entry) => Some(entry),
        }
    }

    fn mut_option(&mut self) -> Option<&mut *mut GcObjHead> {
        match self {
            RootEntry::Object(obj) => Some(obj),
            RootEntry::Free(_) => None,
        }
    }
}

struct RootObject<'a, T>
    where
        T: Traceable,
{
    heap: &'a GcHeap,
    index: usize,
    _phantom: PhantomData<*mut GcObj<T>>,
}

impl<'a, T> RootObject<'a, T>
    where
        T: Traceable,
{
    fn to_ptr(&self) -> *mut GcObj<T> {
        self.heap.roots.borrow()[self.index]
            .to_option()
            .unwrap()
            .cast::<GcObj<T>>()
    }
}

impl<'a, T> Clone for RootObject<'a, T>
    where
        T: Traceable,
{
    fn clone(&self) -> Self {
        unsafe { self.heap.make_root(self.to_ptr()) }
    }
}

impl<'a, T> Drop for RootObject<'a, T>
    where
        T: Traceable,
{
    fn drop(&mut self) {
        unsafe {
            (*self.to_ptr()).head.forward = ptr::null_mut();
        }
        let mut roots = self.heap.roots.borrow_mut();
        roots[self.index] = RootEntry::Free(self.heap.free_root.get());
        self.heap.free_root.set(self.index);
    }
}

#[derive(Clone)]
struct GcHeap {
    begin: Cell<*mut u8>,
    free: Cell<*mut u8>,
    end: Cell<*mut u8>,
    roots: RefCell<Vec<RootEntry>>,
    free_root: Cell<usize>,
}

pub struct GcVtable {
    size: unsafe fn(&mut GcObjHead) -> usize,
    mark: unsafe fn(&mut GcObjHead, &mut *mut GcObjHead),
    update: unsafe fn(&mut GcObjHead),
}

pub struct GcObjHead {
    vtable: &'static GcVtable,
    forward: *mut GcObjHead,
}

#[repr(C)]
pub struct GcObj<T>
    where
        T: Traceable,
{
    head: GcObjHead,
    inner: T,
}

impl<T> GcObj<T>
    where
        T: Traceable,
{
    unsafe fn mut_from_head(head: &mut GcObjHead) -> &mut GcObj<T> {
        &mut *(head as *mut _ as *mut GcObj<T>)
    }
}

fn round_to_align(align: usize, size: usize) -> usize {
    if size % align != 0 {
        size + align
    } else {
        size
    }
}

impl GcHeap {
    fn new(heap_size: usize, root_count: usize) -> Self {
        let layout = alloc::Layout::from_size_align(heap_size, mem::align_of::<usize>()).unwrap();

        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }

        let mut roots = Vec::with_capacity(root_count);
        for x in 1..=root_count {
            roots.push(RootEntry::Free(x));
        }

        GcHeap {
            begin: Cell::new(ptr),
            free: Cell::new(ptr),
            end: Cell::new(unsafe { ptr.add(heap_size) }),
            roots: RefCell::new(roots),
            free_root: Cell::new(0),
        }
    }

    fn free_space(&self) -> usize {
        self.end.get() as usize - self.free.get() as usize
    }

    unsafe fn collect(&self) {
        let mut end_maker: usize = 0;
        let mut current_mark = &mut end_maker as *mut _ as *mut GcObjHead;

        for root in self.roots.borrow().iter().filter_map(|x| x.to_option()) {
            let head = &mut *root;
            if head.forward.is_null() {
                head.forward = current_mark;
                current_mark = root;
            }
        }

        while !ptr::eq(&end_maker as *const _ as *const GcObjHead, current_mark) {
            let head = &mut *current_mark;
            mem::swap(&mut head.forward, &mut current_mark);
            (head.vtable.mark)(head, &mut current_mark);
        }
        let heap_end = self.free.get();
        let mut current_obj = self.begin.get();
        let mut free = self.begin.get();
        while current_obj < heap_end {
            let current_head = &mut *(current_obj as *mut () as *mut GcObjHead);
            let size = (current_head.vtable.size)(current_head);
            if !current_head.forward.is_null() {
                current_head.forward = free as *mut () as *mut GcObjHead;
                free = (free as usize + size) as *mut u8;
            }
            current_obj = current_obj.add(size);
        }
        self.free.set(free);

        current_obj = self.begin.get();
        while current_obj < heap_end {
            let current_head = &mut *(current_obj as *mut () as *mut GcObjHead);
            (current_head.vtable.update)(current_head);
            current_obj = current_obj.add((current_head.vtable.size)(current_head))
        }

        for root in self
            .roots
            .borrow_mut()
            .iter_mut()
            .filter_map(RootEntry::mut_option)
            {
                *root = (**root).forward;
            }

        current_obj = self.begin.get();
        free = self.begin.get();
        while current_obj < heap_end {
            let (size, is_dead) = {
                let head = &mut *(current_obj as *mut () as *mut GcObjHead);
                let is_dead = head.forward.is_null();
                head.forward = ptr::null_mut();
                ((head.vtable.size)(head), is_dead)
            };

            if !is_dead {
                ptr::copy(current_obj, free, size);
                free = free.add(size);
            }
            current_obj = current_obj.add(size);
        }
    }

    // Obj must point to an object that either is not moved for the live time of the return type
    // or obj must point to an object on the gc heap.
    unsafe fn make_root<T>(&self, obj: *mut GcObj<T>) -> RootObject<T>
        where
            T: Traceable,
    {
        let mut roots = self.roots.borrow_mut();
        let free_root = self.free_root.get();
        if free_root >= roots.len() {
            panic!("Out of roots.")
        }
        let new_free = roots[free_root].get_free().unwrap();
        roots[free_root] = RootEntry::Object(obj as *mut GcObjHead);
        self.free_root.set(new_free);

        RootObject {
            heap: self,
            index: free_root,
            _phantom: PhantomData,
        }
    }

    // After the call to put_on_heap, all pointers to the heap
    // except for pointers on objects traceable from the roots
    // and the pointers in new_obj are invalid.
    unsafe fn put_on_heap<T>(&self, mut new_obj: T) -> Result<RootObject<T>, ()>
        where
            T: Traceable,
    {
        let head = GcObjHead {
            vtable: T::get_vtable(),
            forward: ptr::null_mut(),
        };
        let size = new_obj.size();
        let mut obj = GcObj {
            head,
            inner: new_obj,
        };
        let root_ref = self.make_root(&mut obj);

        if size > self.free_space() {
            self.collect();
            if size > self.free_space() {
                return Err(());
            }
        }

        let ret = self.free.get() as *mut GcObj<T>;
        drop(root_ref);
        ptr::write(ret, obj);
        self.free.set(self.free.get().add(size));

        Ok(self.make_root(ret))
    }
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        let size = self.end.get() as usize - self.begin.get() as usize;
        let layout = Layout::from_size_align(size, align_of::<usize>()).unwrap();
        unsafe { alloc::dealloc(self.begin.get(), layout) };
    }
}
