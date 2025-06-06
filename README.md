# oboe.el
A simple Emacs temporary buffer management framework.

## Installation

``` emacs-lisp
;; for emacs-30+
(use-package oboe
  :vc (:url "https://github.com/gynamics/oboe.el"))
```

## Usage

Emacs has *scratch* buffer for temporary elisp evaluation.
However, sometimes you may want to get a temporary buffer in
specific configuration to do something immediately.  That is not
something difficult to do but makes me tired.  Therefore oboe.el be.

The idea of oboe.el is just as simple as following steps:

1. prompt for configuration to load (you can skip the prompt)
2. create a buffer and load configuration
3. display it for use, with method specified by :display
4. manage buffers with a buffer menu

The idea looks like a trival version of org-capture, but trival
isn't that bad.  org integrated so many advanced features, which
also made it harder to learn to extend it.  You may choose to do
the same thing in the trival way.

At first there was a prototype in CL style, but I found that it was
actually not clearer with CL, so I just abandoned using cl-lib.

Currently we have commands: (if you have any good ideas, please tell me!)
- `oboe-new` creates a new temporary buffer.
- `oboe-recall` brings back a temporary buffer.
- `oboe-absorb` absorbs content from multiple buffers.
- `oboe-menu` filters out temporary buffers in a buffer menu.

I think we may do more in the future, for example, merge two buffers,
continue from an exist buffer like snapshots ... After all, temporary
buffers construct almost everything in high-level text edit control,
e. g. undo, redo, copy, concatenation, etc.

One example of using `oboe-absorb`:

``` emacs-lisp
  (use-package ibuffer
    :commands (ibuffer)
    :init
    (defun ibuffer-oboe-absorb ()
      "Absorb marked buffers into an OBOE temporary buffer."
      (interactive)
      (oboe-absorb (ibuffer-get-marked-buffers)))

    :bind
    (:map ibuffer-mode-map
          ("a" . ibuffer-oboe-absorb))
    )
```

This is useful when you want to create a temporary view of marked
buffers. `ibuffer` provides various filtering functions so it will be
very enjoyable to have this function added.


## Customization

This simple framework is designed to be tweaked easily. The basic
concept is that, a temporary buffer class is defined by a plist, e. g.

``` emacs-lisp
   `(elisp
     :major emacs-lisp-mode
     :init-content
     ,(concat
       ";; This buffer is a temporary buffer for elisp scripting by MEMO.\n"
       ";; you can save it somewhere by pressing C-x C-w.\n"))
```

You can add a new class `scratch` by:

``` emacs-lisp
    (add-to-list 'oboe-config-alist
                 '(scratch
                   :major emacs-lisp-mode
                   :assoc-file (concat (user-emacs-directory)
                                       "scratch-persistent"))
                 ))
    ;; create that associated file if it does not exist yet
    (make-empty-file (concat (user-emacs-directory)
                             "scratch-persistent"))

```

This adds a class named `scratch` to `oboe-config-alist`, which is
associated to a specific file. When a buffer of this type created, it
will be associated to file `~/.config/emacs/scratch-persistent`.

`oboe-config-alist` is a custom variable which can be customized. You
can alter its initial value with `custom-set-variables`.

I have implemented these keys:

- `:create` : A function to create the buffer.  Its default value is
`oboe-default-create-method`.  Normally you don't need to tweak it.

- `:display` : A function to display given buffer.  Its default value
is `oboe-default-display-method`.

- `:assoc-file` : A path as a string.  This allows us to associate
temporary buffers to files to solve some path env problems when
loading a project-wide major-mode.  It can also be used for creating
persistent storage.
  - If it is a regular file, all buffers created will be associated to
this file.  This file will never be truncated.
  - If it is a directory, a unique temporary file will be created in
that directory for each new oboe buffer to be associated to.  These
files may be deleted when associated buffers are killed.  You can
control this behavior via toggle `oboe-delete-temp-file-on-kill`.
  - If it is nil (default) or anything else, do not associate
temporary buffer to file.

- `:major` : Major mode to be loaded in the buffer.  Actually, it
doesn't need to be a major-mode at all, just a function to be called
once.

- `:minor-list` : A list of minor-modes to be loaded in the buffer.
The order of minor-mode calls is determined by `mapc`.

- `:init-content` : A string to be inserted into the buffer.

- `:revive` : A function to find and display a buried buffer with
given config.  This function works on a specific buffer config
rather than a specific buffer, so you can choose which buffer to
revive, even concat all existing buffers.  Its default value is
`oboe-default-revive-method`.

Some items can be used by `oboe-loaders`, which is a list of
functions, each function is a loader that will be called to initialize
the temporary buffer created. Loaders can have arguments, these
arguments are fetched from corresponding config with keys. This is
just a brute way to implement generic functions.

``` text
    (f :key1 :key2 ... :keyN)
    + config
    => (funcall 'f (plist-get :key1 config)
                   (plist-get :key2 config)
                   ...
                   (plist-get :keyN config))
```

Be aware that `plist-get` may return `nil`, `oboe-use` won't check it
for your function, so your function should be able to check them.

Actually there was a prototype in CL style. But I just found that,
that one was not simpler, so I just abandoned CL and chose plists.
