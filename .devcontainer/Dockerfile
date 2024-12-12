# FROM ubuntu:24.04
FROM mcr.microsoft.com/devcontainers/cpp:ubuntu-24.04

# Install additional packages

RUN apt-get update \
    && apt-get upgrade --yes \
    && apt-get install --yes --no-install-recommends adr-tools clang-format clang-tidy cppcheck gcc-14 g++-14 iwyu lcov pre-commit \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Choose default gcc and g++ version

RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-13 13 \
    && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-14 14 \
    && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-13 13 \
    && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-14 14 \
    && update-alternatives --install /usr/bin/gcov gcov /usr/bin/gcov-13 13 \
    && update-alternatives --install /usr/bin/gcov gcov /usr/bin/gcov-14 14

# Download hadolint

RUN curl -Lo /bin/hadolint https://github.com/hadolint/hadolint/releases/latest/download/hadolint-Linux-x86_64 \
    && chmod +x /bin/hadolint

# Post-start script

COPY post-start.sh /
RUN chmod +x /post-start.sh
