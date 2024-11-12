ARG VARIANT=ubuntu-20.04
FROM mcr.microsoft.com/vscode/devcontainers/cpp:0-${VARIANT}

RUN apt-get update && apt-get install -y \
    python python3.8-venv pip pipx \
    && rm -rf /var/lib/apt/lists/*

RUN pip install clang-format pre-commit

WORKDIR /temp-repo

COPY .pre-commit-config.yaml .

RUN git init . && pre-commit install --install-hooks

WORKDIR /

RUN rm -rf temp-repo
