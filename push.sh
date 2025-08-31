#!/bin/bash

# Ask for branch name
read -p "Enter branch name: " branch

# Ask for commit message
read -p "Enter commit message: " message

# Fetch remote branches first
git pull

# Check if branch exists locally
if git show-ref --verify --quiet refs/heads/"$branch"; then
    echo "Switching to existing local branch '$branch'."
    git checkout "$branch"
else
    # Check if branch exists on remote
    if git ls-remote --exit-code --heads origin "$branch" >/dev/null; then
        echo "Branch exists on remote. Checking out."
        git checkout -b "$branch" origin/"$branch"
    else
        echo "Branch does not exist. Creating locally and on remote."
        git checkout -b "$branch"
    fi
fi

# Add all changes
git add .

# Commit with the provided message
git commit -m "$message"

# Push to origin (creates branch remotely if it doesn't exist)
git push -u origin "$branch"
