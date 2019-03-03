# Hg README

Hg is a super basic mercurial extension for VS Code to try to smooth developer workflows.  It is designed for use in the development flow, e.g. working on a specific branch 

It supports pulling, committing and pushing but doesn't yet support elements around branching, complex updates and merges

More complex features should be done through the CLI.

## Features

If you want to pull changes and update to the latest version (requires ssh)

`
Hg - Pull
`

To commit changes type the command below and then enter your commit message into the box

`
Hg - Commit
`

Finally, to push your changes to the repo type the command (requires ssh)

`
Hg - Push
`

## TODO

Add support for non-linear development, e.g. updating and branching

## Requirements

The only requirement is that you have mercurial installed and are working in a mercurial repo.

## Version History

0.0.1 - Add and commit 

0.0.2 - Support for commit messages with spaces in (useful!)

0.0.3 - Push and pull support is ssh enable

0.0.4 - Spelling