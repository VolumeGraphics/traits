# FROM ubuntu:24.04
FROM mcr.microsoft.com/devcontainers/cpp:ubuntu-24.04

# Update and upgrade packages

RUN apt update && apt upgrade --yes

# Install additional packages

RUN apt install --yes gcc-14 g++-14
RUN apt install --yes clang-format clang-tidy cppcheck iwyu lcov pre-commit

# Choose default gcc and g++ version

RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-13 13 \
    && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-14 14

RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-13 13 \
    && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-14 14

RUN update-alternatives --install /usr/bin/gcov gcov /usr/bin/gcov-13 13 \
    && update-alternatives --install /usr/bin/gcov gcov /usr/bin/gcov-14 14

# Post-start script

COPY post-start.sh /
RUN chmod +x /post-start.sh
