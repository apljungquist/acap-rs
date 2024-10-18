//! Bindings for the [Bounding Box API](https://axiscommunications.github.io/acap-documentation/docs/api/src/api/bbox/html/bbox_8h.html).
use std::{ffi::CString, ptr};

use axstorage_sys::{
    ax_storage_get_path, ax_storage_get_status, ax_storage_get_storage_id, ax_storage_get_type,
    ax_storage_list, ax_storage_release_async, ax_storage_setup_async, ax_storage_subscribe,
    ax_storage_unsubscribe, gchar, guint, AXStorage, AXStorageStatusEventId,
    AXStorageStatusEventId_AX_STORAGE_AVAILABLE_EVENT,
    AXStorageStatusEventId_AX_STORAGE_EXITING_EVENT, AXStorageStatusEventId_AX_STORAGE_FULL_EVENT,
    AXStorageStatusEventId_AX_STORAGE_WRITABLE_EVENT, AXStorageType, AXStorageType_EXTERNAL_TYPE,
    AXStorageType_LOCAL_TYPE, AXStorageType_UNKNOWN_TYPE,
};
use glib::{
    ffi::GError,
    translate::{from_glib_full, FromGlibPtrFull},
    Error, GStringPtr, List,
};
use glib_sys::{gpointer, GTRUE};

macro_rules! try_func {
    ($func:ident, $($arg:expr),+ $(,)?) => {{
        let mut error: *mut GError = ptr::null_mut();
        let retval = $func($( $arg ),+, &mut error);
        if !error.is_null() {
            return Err(from_glib_full(error));
        }
        retval
    }};
}

#[derive(Debug)]
pub struct Storage {
    raw: *mut AXStorage,
}

unsafe impl Send for Storage {}

#[non_exhaustive]
pub enum StatusEventId {
    Available,
    Exiting,
    Full,
    Writable,
}

impl StatusEventId {
    fn into_raw(self) -> AXStorageStatusEventId {
        match self {
            Self::Available => AXStorageStatusEventId_AX_STORAGE_AVAILABLE_EVENT,
            Self::Exiting => AXStorageStatusEventId_AX_STORAGE_EXITING_EVENT,
            Self::Full => AXStorageStatusEventId_AX_STORAGE_FULL_EVENT,
            Self::Writable => AXStorageStatusEventId_AX_STORAGE_WRITABLE_EVENT,
        }
    }
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub enum Type {
    Local,
    External,
    Unknown,
}

impl Type {
    fn from_raw(value: AXStorageType) -> Self {
        match value {
            v if v == AXStorageType_LOCAL_TYPE => Self::Local,
            v if v == AXStorageType_EXTERNAL_TYPE => Self::External,
            v if v == AXStorageType_UNKNOWN_TYPE => Self::Unknown,
            _ => unreachable!(),
        }
    }
}

pub fn list() -> Result<List<GStringPtr>, Error> {
    unsafe {
        let mut error: *mut GError = ptr::null_mut();
        let list = ax_storage_list(&mut error);
        if !error.is_null() {
            debug_assert!(list.is_null());
            return Err(Error::from_glib_full(error));
        }
        Ok(List::from_glib_full(list))
    }
}

pub fn subscribe<F>(storage_id: &mut GStringPtr, callback: F) -> Result<SubscriptionId, Error>
where
    F: FnMut(&GStringPtr, Option<Error>) + Send,
{
    unsafe {
        let callback = Box::into_raw(Box::new(callback)) as gpointer;
        // Note that callback will be called anytime the status changes
        let subscription_id = try_func!(
            ax_storage_subscribe,
            storage_id.as_ptr() as *mut _,
            Some(subscribe_callback_trampoline::<F>),
            callback
        );
        debug_assert_ne!(subscription_id, 0);
        Ok(SubscriptionId(subscription_id))
        // TODO: Drop callback.
    }
}

unsafe extern "C" fn subscribe_callback_trampoline<F>(
    storage_id: *mut gchar,
    user_data: gpointer,
    error: *mut GError,
) where
    F: FnMut(&GStringPtr, Option<Error>) + Send,
{
    let storage_id: &GStringPtr =
        &*(&(storage_id as gpointer) as *const gpointer as *const GStringPtr);
    let error = if error.is_null() {
        None
    } else {
        Some(Error::from_glib_full(error))
    };
    let callback = &mut *(user_data as *mut F);
    callback(storage_id.as_ref(), error);
}

pub fn unsubscribe(id: &SubscriptionId) -> Result<(), Error> {
    unsafe {
        let success = try_func!(ax_storage_unsubscribe, id.0);
        debug_assert_eq!(success, GTRUE);
        Ok(())
    }
}

pub fn setup_async<F: FnMut(Result<Storage, Error>)>(
    storage_id: &GStringPtr,
    callback: F,
) -> Result<(), Error> {
    unsafe {
        let callback = Box::into_raw(Box::new(callback)) as gpointer;
        let success = try_func!(
            ax_storage_setup_async,
            // FIXME: Verify that we can cast to mut her and in places like this.
            // ax_storage_setup_async never makes use of the mutability
            storage_id.as_ptr() as *mut _,
            Some(setup_async_callback_trampoline::<F>),
            callback
        );
        debug_assert_eq!(success, GTRUE);
        Ok(())
    }
}
unsafe extern "C" fn setup_async_callback_trampoline<F>(
    storage: *mut AXStorage,
    user_data: gpointer,
    error: *mut GError,
) where
    F: FnMut(Result<Storage, Error>),
{
    let result = if error.is_null() {
        debug_assert!(!storage.is_null());
        Ok(Storage { raw: storage })
    } else {
        debug_assert!(storage.is_null());
        Err(Error::from_glib_full(error))
    };
    let callback = &mut *(user_data as *mut F);
    callback(result);
}

pub fn release_async<F>(storage: &mut Storage, callback: F) -> Result<(), Error>
where
    F: FnMut(Option<Error>) + Send,
{
    unsafe {
        let callback = Box::into_raw(Box::new(callback)) as gpointer;
        let success = try_func!(
            ax_storage_release_async,
            storage.raw,
            Some(release_async_trampoline::<F>),
            callback
        );
        debug_assert_eq!(success, GTRUE);
        Ok(())
    }
}

unsafe extern "C" fn release_async_trampoline<F>(user_data: gpointer, error: *mut GError)
where
    F: FnMut(Option<Error>) + Send,
{
    let error = if error.is_null() {
        None
    } else {
        Some(Error::from_glib_full(error))
    };
    let callback = &mut *(user_data as *mut F);
    callback(error);
}
pub fn get_path(storage: &Storage) -> Result<CString, Error> {
    unsafe {
        let path = try_func!(ax_storage_get_path, storage.raw);
        Ok(CString::from_raw(path))
    }
}

pub fn get_status(storage_id: &GStringPtr, event: StatusEventId) -> Result<bool, Error> {
    unsafe {
        let mut error: *mut GError = ptr::null_mut();
        let status =
            ax_storage_get_status(storage_id.as_ptr() as *mut _, event.into_raw(), &mut error);
        if !error.is_null() {
            return Err(Error::from_glib_full(error));
        }
        Ok(status == GTRUE)
    }
}

pub fn get_storage_id(storage: &mut Storage) -> Result<GStringPtr, Error> {
    unsafe {
        let mut storage_id = try_func!(ax_storage_get_storage_id, storage.raw);
        let storage_id: *mut gpointer = &mut (storage_id as gpointer);
        let p: GStringPtr = ptr::read(storage_id as *mut GStringPtr);
        Ok(p)
    }
}

pub fn get_type(storage: &mut Storage) -> Result<Type, Error> {
    unsafe {
        let storage_type = try_func!(ax_storage_get_type, storage.raw);
        Ok(Type::from_raw(storage_type))
    }
}

#[derive(Debug)]
pub struct SubscriptionId(guint);
