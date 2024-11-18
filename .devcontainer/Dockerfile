ARG VARIANT=ubuntu-20.04
FROM mcr.microsoft.com/vscode/devcontainers/cpp:0-${VARIANT}

RUN apt-get update && apt-get install -y \
    lcov python python3.8-venv pip pipx \
    && rm -rf /var/lib/apt/lists/*

RUN pip install clang-format pre-commit

COPY post-start.sh /
RUN chmod +x /post-start.sh
