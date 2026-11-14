# Linux environment for `just perf-check-docker` (Valgrind does not run on macOS).
# Mirrors the CI perf job: pinned Rust, valgrind, gungraun-runner, just and uv.
ARG RUST_VERSION=1.92.0
FROM rust:${RUST_VERSION}-bookworm

ARG GUNGRAUN_VERSION
ARG JUST_VERSION=1.59.0

RUN rustup component add clippy rustfmt
RUN apt-get update \
    && apt-get install -y --no-install-recommends valgrind \
    && rm -rf /var/lib/apt/lists/*
RUN cargo install gungraun-runner --version "${GUNGRAUN_VERSION}" --locked \
    && cargo install just --version "${JUST_VERSION}" --locked \
    && rm -rf /usr/local/cargo/registry
COPY --from=ghcr.io/astral-sh/uv:0.13.7 /uv /usr/local/bin/uv
# The repository is bind-mounted with the host's ownership.
RUN git config --system --add safe.directory '*'
