# Linux environment for `just perf-docker ...` (Valgrind does not run on macOS).
# Same distribution as the CI perf job (runs-on: ubuntu-26.04), so both get the same apt
# Valgrind; `just perf-check` refuses a Valgrind other than `perf_valgrind` in the justfile.
# Rust and gungraun-runner versions come from rust-toolchain.toml and Cargo.toml (see
# `just perf-docker-image`).
FROM ubuntu:26.04

ARG RUST_VERSION=1.92.0
ARG GUNGRAUN_VERSION=0.20.0
ARG JUST_VERSION=1.59.0

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl gcc libc6-dev git valgrind \
    && rm -rf /var/lib/apt/lists/*
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --no-modify-path --profile minimal --default-toolchain "${RUST_VERSION}" \
        --component clippy,rustfmt
RUN cargo install gungraun-runner --version "${GUNGRAUN_VERSION}" --locked \
    && cargo install just --version "${JUST_VERSION}" --locked \
    && rm -rf /usr/local/cargo/registry
COPY --from=ghcr.io/astral-sh/uv:0.13.7 /uv /usr/local/bin/uv
# The repository is bind-mounted with the host's ownership.
RUN git config --system --add safe.directory '*'
