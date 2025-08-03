use std::ffi::CStr;
use std::os::raw::c_char;

/// Runtime support functions for Keen language
/// These functions are called from compiled Keen code

/// Print a string to stdout
/// This function is called from compiled Keen code
#[no_mangle]
pub extern "C" fn keen_print_string(ptr: *const c_char) -> i32 {
    if ptr.is_null() {
        eprintln!("Error: null pointer passed to keen_print_string");
        return -1;
    }

    unsafe {
        match CStr::from_ptr(ptr).to_str() {
            Ok(s) => {
                println!("{}", s);
                0 // success
            }
            Err(_) => {
                eprintln!("Error: invalid UTF-8 string passed to keen_print_string");
                -1 // error
            }
        }
    }
}

/// Print an integer to stdout
#[no_mangle]
pub extern "C" fn keen_print_int(value: i64) -> i32 {
    println!("{}", value);
    0 // success
}

/// Print a float to stdout
#[no_mangle]
pub extern "C" fn keen_print_float(value: f64) -> i32 {
    println!("{}", value);
    0 // success
}

/// Print a boolean to stdout
#[no_mangle]
pub extern "C" fn keen_print_bool(value: i32) -> i32 {
    if value != 0 {
        println!("true");
    } else {
        println!("false");
    }
    0 // success
}

/// Memory allocation for strings and data structures
#[no_mangle]
pub extern "C" fn keen_alloc(size: usize) -> *mut u8 {
    let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
    unsafe { std::alloc::alloc(layout) }
}

/// Memory deallocation
#[no_mangle]
pub extern "C" fn keen_free(ptr: *mut u8, size: usize) {
    if !ptr.is_null() {
        let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
        unsafe { std::alloc::dealloc(ptr, layout) };
    }
}

/// Create a string on the heap and return its pointer
/// This allocates memory for the string and copies the data
#[no_mangle]
pub extern "C" fn keen_create_string(data: *const u8, len: usize) -> *mut c_char {
    if data.is_null() || len == 0 {
        return std::ptr::null_mut();
    }

    unsafe {
        // Allocate memory for string + null terminator
        let ptr = keen_alloc(len + 1) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }

        // Copy the string data
        std::ptr::copy_nonoverlapping(data, ptr as *mut u8, len);

        // Add null terminator
        *ptr.add(len) = 0;

        ptr
    }
}

/// String concatenation
#[no_mangle]
pub extern "C" fn keen_concat_strings(s1: *const c_char, s2: *const c_char) -> *mut c_char {
    if s1.is_null() || s2.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let str1 = CStr::from_ptr(s1);
        let str2 = CStr::from_ptr(s2);

        let len1 = str1.to_bytes().len();
        let len2 = str2.to_bytes().len();
        let total_len = len1 + len2;

        let result_ptr = keen_alloc(total_len + 1) as *mut c_char;
        if result_ptr.is_null() {
            return std::ptr::null_mut();
        }

        // Copy first string
        std::ptr::copy_nonoverlapping(str1.as_ptr(), result_ptr, len1);
        // Copy second string
        std::ptr::copy_nonoverlapping(str2.as_ptr(), result_ptr.add(len1), len2);
        // Add null terminator
        *result_ptr.add(total_len) = 0;

        result_ptr
    }
}

/// Convert integer to string
#[no_mangle]
pub extern "C" fn keen_int_to_string(value: i64) -> *mut c_char {
    let s = value.to_string();
    let bytes = s.as_bytes();
    keen_create_string(bytes.as_ptr(), bytes.len())
}

/// Convert float to string
#[no_mangle]
pub extern "C" fn keen_float_to_string(value: f64) -> *mut c_char {
    let s = value.to_string();
    let bytes = s.as_bytes();
    keen_create_string(bytes.as_ptr(), bytes.len())
}

/// Convert boolean to string
#[no_mangle]
pub extern "C" fn keen_bool_to_string(value: i32) -> *mut c_char {
    let s = if value != 0 { "true" } else { "false" };
    let bytes = s.as_bytes();
    keen_create_string(bytes.as_ptr(), bytes.len())
}

/// Debug function to print a message with a label
#[no_mangle]
pub extern "C" fn keen_debug_print(label: *const c_char, value: i64) -> i32 {
    if label.is_null() {
        println!("DEBUG: {}", value);
    } else {
        unsafe {
            match CStr::from_ptr(label).to_str() {
                Ok(label_str) => println!("DEBUG {}: {}", label_str, value),
                Err(_) => println!("DEBUG: {}", value),
            }
        }
    }
    0
}

/// Runtime initialization
#[no_mangle]
pub extern "C" fn keen_runtime_init() -> i32 {
    // Initialize any global runtime state if needed
    0
}

/// Runtime cleanup
#[no_mangle]
pub extern "C" fn keen_runtime_cleanup() -> i32 {
    // Clean up any global runtime state if needed
    0
}

/// String builder for interpolation
#[repr(C)]
pub struct StringBuilder {
    parts: Vec<String>,
    capacity: usize,
}

/// Create a new string builder
#[no_mangle]
pub extern "C" fn keen_string_builder_new() -> *mut StringBuilder {
    let builder = Box::new(StringBuilder {
        parts: Vec::new(),
        capacity: 0,
    });
    Box::into_raw(builder)
}

/// Append a literal string to the builder
#[no_mangle]
pub extern "C" fn keen_string_builder_append_literal(
    builder: *mut StringBuilder,
    literal: *const c_char,
) -> i32 {
    if builder.is_null() || literal.is_null() {
        return -1;
    }

    unsafe {
        let builder_ref = &mut *builder;
        match CStr::from_ptr(literal).to_str() {
            Ok(s) => {
                builder_ref.parts.push(s.to_string());
                builder_ref.capacity += s.len();
                0
            }
            Err(_) => -1,
        }
    }
}

/// Append an integer value to the builder
#[no_mangle]
pub extern "C" fn keen_string_builder_append_int(builder: *mut StringBuilder, value: i64) -> i32 {
    if builder.is_null() {
        return -1;
    }

    unsafe {
        let builder_ref = &mut *builder;
        let s = value.to_string();
        builder_ref.capacity += s.len();
        builder_ref.parts.push(s);
        0
    }
}

/// Append a float value to the builder
#[no_mangle]
pub extern "C" fn keen_string_builder_append_float(builder: *mut StringBuilder, value: f64) -> i32 {
    if builder.is_null() {
        return -1;
    }

    unsafe {
        let builder_ref = &mut *builder;
        let s = value.to_string();
        builder_ref.capacity += s.len();
        builder_ref.parts.push(s);
        0
    }
}

/// Append a boolean value to the builder
#[no_mangle]
pub extern "C" fn keen_string_builder_append_bool(builder: *mut StringBuilder, value: i32) -> i32 {
    if builder.is_null() {
        return -1;
    }

    unsafe {
        let builder_ref = &mut *builder;
        let s = if value != 0 { "true" } else { "false" };
        builder_ref.capacity += s.len();
        builder_ref.parts.push(s.to_string());
        0
    }
}

/// Append a string value to the builder
#[no_mangle]
pub extern "C" fn keen_string_builder_append_string(
    builder: *mut StringBuilder,
    value: *const c_char,
) -> i32 {
    if builder.is_null() || value.is_null() {
        return -1;
    }

    unsafe {
        let builder_ref = &mut *builder;
        match CStr::from_ptr(value).to_str() {
            Ok(s) => {
                builder_ref.parts.push(s.to_string());
                builder_ref.capacity += s.len();
                0
            }
            Err(_) => -1,
        }
    }
}

/// Finish building and return the final string
#[no_mangle]
pub extern "C" fn keen_string_builder_finish(builder: *mut StringBuilder) -> *mut c_char {
    if builder.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let builder_box = Box::from_raw(builder);
        let final_string = builder_box.parts.join("");
        keen_create_string(final_string.as_ptr(), final_string.len())
    }
}

/// Destroy a string builder without finishing (cleanup)
#[no_mangle]
pub extern "C" fn keen_string_builder_destroy(builder: *mut StringBuilder) -> i32 {
    if builder.is_null() {
        return -1;
    }

    unsafe {
        let _ = Box::from_raw(builder);
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_print_functions() {
        // Test integer printing
        assert_eq!(keen_print_int(42), 0);

        // Test float printing
        assert_eq!(keen_print_float(3.14), 0);

        // Test boolean printing
        assert_eq!(keen_print_bool(1), 0);
        assert_eq!(keen_print_bool(0), 0);
    }

    #[test]
    fn test_string_builder() {
        // Create a new string builder
        let builder = keen_string_builder_new();
        assert!(!builder.is_null());

        // Append various types
        let literal = CString::new("Hello, ").unwrap();
        assert_eq!(
            keen_string_builder_append_literal(builder, literal.as_ptr()),
            0
        );
        assert_eq!(keen_string_builder_append_int(builder, 42), 0);
        assert_eq!(
            keen_string_builder_append_literal(
                builder,
                CString::new(" is the answer").unwrap().as_ptr()
            ),
            0
        );

        // Finish and get result
        let result = keen_string_builder_finish(builder);
        assert!(!result.is_null());

        unsafe {
            let final_str = CStr::from_ptr(result).to_str().unwrap();
            assert_eq!(final_str, "Hello, 42 is the answer");
            keen_free(result as *mut u8, final_str.len() + 1);
        }
    }

    #[test]
    fn test_string_creation() {
        let test_str = "Hello, World!";
        let c_str = keen_create_string(test_str.as_ptr(), test_str.len());

        assert!(!c_str.is_null());

        unsafe {
            let result = CStr::from_ptr(c_str).to_str().unwrap();
            assert_eq!(result, test_str);
            keen_free(c_str as *mut u8, test_str.len() + 1);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let str1 = CString::new("Hello, ").unwrap();
        let str2 = CString::new("World!").unwrap();

        let result = keen_concat_strings(str1.as_ptr(), str2.as_ptr());
        assert!(!result.is_null());

        unsafe {
            let concatenated = CStr::from_ptr(result).to_str().unwrap();
            assert_eq!(concatenated, "Hello, World!");
            keen_free(result as *mut u8, concatenated.len() + 1);
        }
    }

    #[test]
    fn test_conversion_functions() {
        // Test integer to string
        let int_str = keen_int_to_string(42);
        assert!(!int_str.is_null());
        unsafe {
            let result = CStr::from_ptr(int_str).to_str().unwrap();
            assert_eq!(result, "42");
            keen_free(int_str as *mut u8, result.len() + 1);
        }

        // Test boolean to string
        let bool_str = keen_bool_to_string(1);
        assert!(!bool_str.is_null());
        unsafe {
            let result = CStr::from_ptr(bool_str).to_str().unwrap();
            assert_eq!(result, "true");
            keen_free(bool_str as *mut u8, result.len() + 1);
        }
    }
}
