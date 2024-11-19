ARG VARIANT=ubuntu-20.04
ARG CMAKE_VERSION=3.30.5

FROM mcr.microsoft.com/vscode/devcontainers/cpp:0-${VARIANT}

RUN apt-get update && apt-get install --yes \
    lcov python python3.8-venv pip pipx \
    && rm -rf /var/lib/apt/lists/*

RUN pip install clang-format pre-commit

COPY ./reinstall-cmake.sh /tmp/
RUN chmod +x /tmp/reinstall-cmake.sh && /tmp/reinstall-cmake.sh ${CMAKE_VERSION} && rm -f /tmp/reinstall-cmake.sh

COPY post-start.sh /
RUN chmod +x /post-start.sh
