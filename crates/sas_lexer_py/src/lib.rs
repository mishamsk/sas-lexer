use pyo3::{
    exceptions::PyRuntimeError,
    prelude::*,
    types::{PyBytes, PyString},
};
use sas_lexer::lex;

/// TODO
#[pyfunction]
fn lex_str<'py>(py: Python<'py>, src: &Bound<'py, PyString>) -> PyResult<Bound<'py, PyBytes>> {
    let src: &str = src.extract()?;

    let (buf, _) = lex(&src).map_err(|e| PyRuntimeError::new_err(e.to_string()))?;

    let tok_vec = buf.into_resolved_token_vec();

    let data = rmp_serde::encode::to_vec(&tok_vec)
        .map_err(|e| PyRuntimeError::new_err(format!("Failed to serialize to msgpack: {e}")))?;

    Ok(PyBytes::new_bound(py, &data))
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn _sas_lexer_rust(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(lex_str, m)?)
}
