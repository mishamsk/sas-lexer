use pyo3::{
    exceptions::PyRuntimeError,
    prelude::*,
    types::{PyBytes, PyString},
};
use sas_lexer::{lex_program, LexResult};

/// TODO
#[pyfunction]
fn lex_program_from_str<'py>(
    py: Python<'py>,
    src: &Bound<'py, PyString>,
) -> PyResult<Bound<'py, PyBytes>> {
    let src: &str = src.extract()?;

    let LexResult { buffer, .. } =
        lex_program(&src).map_err(|e| PyRuntimeError::new_err(e.to_string()))?;

    let tok_vec = buffer.into_resolved_token_vec();

    let data = rmp_serde::encode::to_vec(&tok_vec)
        .map_err(|e| PyRuntimeError::new_err(format!("Failed to serialize to msgpack: {e}")))?;

    Ok(PyBytes::new_bound(py, &data))
}

#[pymodule]
fn _sas_lexer_rust(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(lex_program_from_str, m)?)
}
