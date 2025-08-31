#!/bin/bash

read -p "Enter branch name: " branch
read -p "Enter commit message: " message

git add .
git commit -m "$message"
git push origin "$branch"
