# turbokit

> turbokit

# ![](http://cranlogs-dev.r-pkg.org/badges/%3Cpackagename%3E)

# Overview

Turbokit introduces a set of tools for quickly inserting functions, function chains and reducing redundancy in formatting tasks. Heavily reduce keypresses and speed up frequent tasks.

-   `turbo()` inserts a function call in the active document using abbreviations. Abbreviations are flexible. Works without attaching packages in which functions live. Works without attaching the turbokit package.

-   `read()` and `boot()` provide quick setup to give the data science workflow a jumpstart. Specify a set of shortcuts and get the appropriate library calls.

<!-- -->

-   `%>>%` superpipe operator translates a complex pipe statement into full length snippets

![tidymath](https://user-images.githubusercontent.com/59521296/115900266-55682080-a492-11eb-9900-132af0de617d.gif)

-   `insert_pipe()` inserts the appropriate pipe (`%>>%` or `+`) based on document context. Usable from within function brackets. Gone are keypresses for redundant formatting.

-   Keyboard shortcuts for core functions, as well as frequently used dplyr functions. Extremely fast code input.

`vignette("chaining")`

`vignette("turbo")`

# Installation

`devtools::install_github("D-se/turbokit")`

# Cheat Sheet

[![](images/cheatsheetthumb.png)](https://github.com/D-Se/turbokit/blob/main/turbokit.pdf)
