## block-nav.el

This is a package for navigating through code based on indentation.
I made this after seeing this post on [reddit](https://old.reddit.com/r/emacs/comments/izwrlf/anyone_using_spatial_navigation_in_emacs/)

Mostly it just looks like a really neat way of navigating code: 
![gif of the navigation in action](https://user-images.githubusercontent.com/22285063/94889555-96dba480-044a-11eb-9268-fae6063f51eb.gif)

## Installing 
This package has been submitted to melpa and is waiting for approval. 
In the meantime, the package can be installed using `straight.el` if you'd like,
or another package manager that allows you to install from GitHub sources.

```lisp
(straight-use-package
 '(block-nav :type git :host github :repo "nixin72/block-nav.el")
 :config
 (progn
   (setf block-nav-move-skip-shallower t
        block-nav-center-after-scroll t)))
```

## Usage
There are 4 interactive commands that you can use with this package:
- `block-nav-next-block`
  Moves to the next line of code with the same level of indentation.
- `block-nav-previous-block`
  Moves to the previous line of code with the same level of indentation.
- `block-nav-next-indentation-level`
  Moves to the next line of code with a deeper indentation level.
- `block-nav-previous-indentation-level`
  Moves to the previous line of code with a shallower indentation level.

The functionality of each of these functions is shown in the git above.

## Configuring
Currently there are only two variable to add configuration:
- `block-nav-center-after-scroll`
  When non-nil, this will recenter the current line after calling any of the above 4 functions
- `block-nav-move-skip-shallower`
  When non-nil, any lines with shallower indentation will be skipped when using the 2 `move` functions above.

## Contributing 
If you'd like to contribute, please feel free to open a pull request. 

If you've found a bug, please create and issue and let me know what's going on!

Thank you, hope you've enjoyed this package.
