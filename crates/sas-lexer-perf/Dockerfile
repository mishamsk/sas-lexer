# Linux environment for `just perf-docker ...` (Valgrind does not run on macOS).
# Same distribution as the CI perf job (runs-on: ubuntu-26.04), so both get the same apt
# Valgrind; `just perf-check` refuses a Valgrind other than the one pinned in
# [package.metadata.perf] of Cargo.toml. `just perf-docker-image` passes the Rust, gungraun-runner
# and uv versions from rust-toolchain.toml, Cargo.toml and mise.toml.
ARG UV_VERSION=latest
FROM ghcr.io/astral-sh/uv:${UV_VERSION} AS uv

FROM ubuntu:26.04

ARG RUST_VERSION
ARG GUNGRAUN_VERSION

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl gcc libc6-dev git valgrind \
    && rm -rf /var/lib/apt/lists/*
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --no-modify-path --profile minimal --default-toolchain "${RUST_VERSION:?}" \
        --component clippy,rustfmt
# just is a prerequisite, not pinned (like on the host).
RUN cargo install gungraun-runner --version "${GUNGRAUN_VERSION:?}" --locked \
    && cargo install just --locked \
    && rm -rf /usr/local/cargo/registry
COPY --from=uv /uv /usr/local/bin/uv
# The repository is bind-mounted with the host's ownership.
RUN git config --system --add safe.directory '*'
