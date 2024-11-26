#!/bin/bash

# Exit on any error
set -e

# Check if VERSION parameter is provided
if [ $# -eq 0 ]; then
    echo "Error: VERSION parameter is required"
    echo "Usage: $0 <VERSION>"
    exit 1
fi

VERSION=$1

# Check for uncommitted changes
if ! git diff-index --quiet HEAD --; then
    echo "Error: There are uncommitted changes in the repository"
    exit 1
fi

# Check if on main branch
if [ "$(git branch --show-current)" != "main" ]; then
    echo "Error: You must be on the main branch"
    exit 1
fi

# Check for unpushed commits
if [ -n "$(git cherry -v origin/main)" ]; then
    echo "Error: There are unpushed commits"
    exit 1
fi

# Create and push tag
git tag "v$VERSION"
git push origin "v$VERSION"

echo "Successfully created and pushed tag v$VERSION"
