#!/bin/sh
# Install one just-built sas-lexer artifact (wheel or sdist) into a fresh venv and test it.
#
# Usage: test-built-package.sh PYTHON EXPECTED_MINOR ARTIFACT
#   PYTHON          interpreter that creates the venv, e.g. python
#   EXPECTED_MINOR  the Python minor the job targets, e.g. 3.13 (checked, so a runner or
#                   container that silently provides another interpreter fails the job)
#   ARTIFACT        path to the .whl or .tar.gz to install
#
# Run from the repository root (the tests and samples come from the checkout). POSIX sh, so it
# also runs in Alpine (busybox) containers and in Git Bash on Windows.
set -eu

py="$1"
expected="$2"
artifact="$3"

test -f "$artifact" || { echo "artifact not found: $artifact" >&2; exit 1; }

actual="$("$py" -c 'import sys; print("%d.%d" % sys.version_info[:2])')"
if [ "$actual" != "$expected" ]; then
    echo "interpreter is Python $actual, expected $expected" >&2
    exit 1
fi

venv="$(mktemp -d)/venv"
"$py" -m venv "$venv"
if [ -d "$venv/Scripts" ]; then bin="$venv/Scripts"; else bin="$venv/bin"; fi
vpy="$bin/python"

"$vpy" -m pip install --disable-pip-version-check --quiet --upgrade pip

# The package itself only ever comes from the given file: never from an index, and without its
# dependencies, so nothing can substitute a published release. An sdist still needs the index
# for its build backend (maturin), which the isolated build environment installs.
case "$artifact" in
    *.whl) "$vpy" -m pip install --disable-pip-version-check --no-index --no-deps "$artifact" ;;
    *.tar.gz) "$vpy" -m pip install --disable-pip-version-check --no-deps --no-binary sas-lexer "$artifact" ;;
    *) echo "unknown artifact type: $artifact" >&2; exit 1 ;;
esac

# Then the runtime dependencies the installed artifact declares, plus the test runner pinned
# like the dev dependency group.
reqs="$(dirname "$venv")/requirements.txt"
"$vpy" -I -c '
import tomllib
from importlib.metadata import requires

for req in requires("sas-lexer") or []:
    print(req)
with open("pyproject.toml", "rb") as f:
    dev = tomllib.load(f)["dependency-groups"]["dev"]
print(next(r for r in dev if r.startswith("pytest")))
' > "$reqs"
"$vpy" -m pip install --disable-pip-version-check -r "$reqs"
"$vpy" -m pip check

# Import from the venv (not the checkout's src/), and check the version.
"$vpy" -I -c '
import sys
from importlib.metadata import version
import sas_lexer
from sas_lexer import _sas_lexer_rust

print(sys.version)
print(sas_lexer.__file__)
print(_sas_lexer_rust.__file__)
assert sas_lexer.__file__.startswith(sys.prefix), "sas_lexer not imported from the venv"
print("sas_lexer", sas_lexer.__version__, "(distribution", version("sas-lexer") + ")")
'

"$vpy" -m pytest -p no:cacheprovider tests
