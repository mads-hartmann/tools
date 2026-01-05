FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

# Install OCaml and opam
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
       opam \
       build-essential \
       pkg-config \
       libgmp-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Initialize opam and install OCaml
USER vscode
RUN opam init --disable-sandboxing -y \
    && opam switch create 5.2.0 \
    && eval $(opam env) \
    && opam install -y dune cmdliner yojson minttea spices

# Add opam env to bashrc
RUN echo 'eval $(opam env)' >> ~/.bashrc
