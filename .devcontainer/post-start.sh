#!/usr/bin/env bash

echo "Installing pre-commit hooks"
pre-commit install --install-hooks

echo "Installing conan"
pipx install conan
