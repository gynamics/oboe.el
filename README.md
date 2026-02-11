# oboe.el

A simple Emacs temporary buffer management framework.

## About
Emacs has *scratch* buffer for temporary elisp scripting.  However,
sometimes you may want to get a temporary buffer with some other
specific configuration to do something immediately.  That is not
something difficult to do but usually diversed in various packages and
sometimes there is no such support, which can be really annoying.
Therefore oboe.el be.

The idea of oboe.el is just as simple as following steps:

1. prompt for a buffer configuration to load
2. create a buffer and load selected configuration
3. display it according to configuration

The idea looks like a trival version of org-capture, but they have
different target.  oboe is designed as a temporary buffer management
framework.  It will track created oboe buffers in queues, assigning
each new buffer with a unique ID.  You can manage all these buffers in
a menu.  It also provides a plist-based framework for creating
customized configurations.

The core design for the implementation of oboe is to map temporary
buffers (with the same configuration) to natural numbers so that they
can be identified and sorted.  We maintain a integer counter for each
buffer configuration to encode buffer names with natural numbers, the
names of temporary buffers never repeats during emacs process life.

## Installation

``` emacs-lisp
;; for emacs-30+
(use-package oboe
  :vc (:url "https://github.com/gynamics/oboe.el"))
```

## Usage
Currently we have these commands:
- `oboe-new` creates a new temporary buffer.
- `oboe-recall` brings back a temporary buffer.
- `oboe-absorb` absorbs content from multiple buffers.
- `oboe-menu` filters out temporary buffers in a buffer menu.
- `oboe-pipe` creates a new temporary buffer then apply a command on it.
- `oboe-blow` absorbs selected region and send it to an oboe pipe.

I think we may do more in the future, for example, merge two buffers,
continue from an exist buffer like snapshots ...  After all, temporary
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
        ("a" .  ibuffer-oboe-absorb))
  )
```

This is useful when you want to create a temporary view of marked
buffers.  `ibuffer` provides various filtering functions so it will be
very enjoyable to have this function added.


## Customization

This simple framework is designed to be tweaked easily.  The basic
concept is that, a temporary buffer class is defined by a plist, e. g.

``` emacs-lisp
`(:name elisp
  :major emacs-lisp-mode
  :init-content
  ,(concat
    ";; This buffer is a temporary buffer for elisp scripting by MEMO.\n"
    ";; you can save it somewhere by pressing C-x C-w.\n"))
```

You can add a new class `scratch` by:

``` emacs-lisp
(add-to-list 'oboe-config-alist
             `(scratch
               :major emacs-lisp-mode
               :assoc-file ,(concat (user-emacs-directory) "scratch-persistent"))
             ))

;; `:assoc-file' makes a weak association
;; create that associated file first if it does not exist yet
(make-empty-file (concat (user-emacs-directory) "scratch-persistent"))
```

This adds a class named `scratch` to `oboe-config-alist`, which is
associated to a specific file.  When a buffer of this type created, it
will be associated to file `~/.config/emacs/scratch-persistent`.

`oboe-config-alist` is a custom variable which can be customized.  You
can alter its initial value with `custom-set-variables`.  It use the
value of `:name` as key for association, so you don't need to specify
`:name` for items added to `oboe-config-alist`, but put its value at
the front of this item.

I have implemented these keys:

- `:create` : A function to create the buffer.  Its default value is
  `oboe-default-create-method`.  Normally you don't need to tweak it.

- `:display` : A function to display given buffer.  Its default value
  is `oboe-default-display-method`.  You may tweak it to adapt to you
  window management scheme, e. g. `popwin:popup-buffer`.

- `:assoc-file` : A path as a string.  This allows us to associate
  temporary buffers to files to solve some path env problems when
  loading a project-wide major-mode.  It can also be used for creating
  persistent storage.

  - If it is a regular file, all buffers created will be associated to
    this file.  This file will never be truncated.

  - If it is a directory, a unique temporary file will be created in
    that directory for each new oboe buffer to be associated to.
    These files may be deleted when associated buffers are killed.
    You can control this behavior via toggle
    `oboe-delete-temp-file-on-kill`.

  - If it is nil (default) or anything else, do not associate
    temporary buffer to file.

- `:major` : Major mode to be loaded in the buffer.  Actually, it
  doesn't need to be a major-mode at all, just a function to be called
  once when the buffer is just created.

- `:minor-list` : A list of minor-modes to be loaded in the buffer.
  The order of minor-mode calls is determined by `mapc`.

- `:init-content` : A string to be inserted into the buffer.  It can
  also be a function that returns a string.

- `:return` : A function to extract a value from current buffer to be
  provided for other usages.  For example, a `oboe-pipe`.

  ``` emacs-lisp
  ;; how `oboe-pipe' works
  (apply (read-command)            ;; => 1. select cmd ===> 4. call cmd
         (funcall (plist-get config :return)          ;; => 3. convert format 
                  (get-buffer *temporary buffer<X>*)  ;; => 2. edit tmp buffer
                  ))
  ```
  
  You can think this method as the `return` monad for lifting the
  input `x` to `(f x)` so that we can `(apply #'cmd (f x))`.

- `:revive` : A function to find and display a buried buffer with
  given config.  This function works on a specific buffer config
  rather than a specific buffer, so you can choose which buffer to
  revive, even concat all existing buffers.  Its default value is
  `oboe-default-revive-method`.

The values in a configuration are used by `oboe-loaders`, which is a
list of functions, each function is a loader that will be called to
initialize the temporary buffer created.  Loaders can have arguments,
these arguments are fetched from corresponding config with keys.  This
is just a brute way to implement generic functions.  Compared to
`defgeneric` it is just a humble and unsafe implementation, but simple
and stupid.

``` text
(f :key1 :key2 ...  :keyN)
+ config
=> (funcall 'f (plist-get :key1 config)
               (plist-get :key2 config)
               ...
               (plist-get :keyN config))
```

Be aware that `plist-get` may return `nil`, `oboe-use` won't check it
for your function, so your function should be able to check them.

Actually there was a prototype in CL style.  But I just found that,
that one was not simpler, so I just abandoned CL and chose plists.

## Hints

`oboe.el` can be more powerful than you can imagine, because it is a
framework that helps constructing visible buffer-based workflow.  It
is a way to think rather than a method to implement.

- Register buffer: Since buffer is the unit of data in Emacs, you can
  save any configurations into an oboe buffer by customizing `:major`
  method, since it is just a callback function that does not have to
  be a true major mode.  You can also recover the stored configuration
  by customizing `:revive` method.  This trick is just like using
  registers, but you can edit them explicitly in buffers.

- Virtual buffer: You can create a configuration that does not create
  any actual buffer by tweaking `:create` method, you may use this
  trick to recover a buffer configuration on an existing buffer.

- Temporary configuration: If you want to use `oboe.el` as a library
  in your code, you may encounter cases that you want to create an
  oboe buffer with `oboe-new` but you don't want to register that
  configuration to `oboe-config-alist`.  In this case, you can simply
  provide `oboe-new` with a full plist with `:name` as configuration,
  e. g.
  
  ```emacs-lisp
  (oboe-new '(:name myconfig :major my-initialization-func))
  ```
  
  `myconfig` here can still be used for buffer filtering in
  `oboe-menu`, but it will not appear in completion.

- EPC: You can build a buffer-based Edit-Print Chain with `oboe-pipe`,
  the bellend function may be like this:

  ```emacs-lisp
  (add-to-list 'oboe-config-alist
               '(my-epc-head :major my-edit-mode))

  (defun my-oboe-chain (buf)
    (interactive "Bbuffer: ")
    (oboe-pipe
     #'my-oboe-chain
     `(:name my-epc-tail
       :major ,(lambda () (my-init-with buf) (my-edit-mode)))))
  ```

  Here, `my-init-with` will initialize the new oboe buffer with
  information from `buf`.  You can simply type 
  `M-x oboe-pipe RET my-oboe-chain RET my-epc-head RET` to start.

  When the pipe returns, it will create a new oboe pipe which
  initializes new oboe buffer with previous oboe buffer.  This trick
  is just the same as undo chain, but each snapshot are stored
  separately, and can be sorted by buffer name or other
  buffer-independet information.  `oboe.el` is performant, you can
  build chains with thousands of buffers (as long as you have enough
  memory).

- Side pipe: You don't have to apply commands with argument, for
  example, you may set the `:return` method of your oboe buffer to
  `(lambda (buf) nil)`, which always return `nil`, then you can apply
  a command that does not need an argument as the bellend.  The
  bellend command is always runned in the context buffer.

- Oboe in oboe: You can even write oboe configuration in an oboe
  buffer, then create an oboe buffer according to the configuration
  with `oboe-blow` or `oboe-pipe`, the bellend function is simple:
  
  ```emacs-lisp
  (defun oboe-new-read (buf)
    (interactive "Bbuffer: ")
    (oboe-new (read buf)))
  ```
  
  You can simply type `M-x oboe-pipe RET oboe-new-read RET elisp RET`,
  then you write a plist which is a valid oboe configuration, then you
  press `C-c C-c` for commiting, a new oboe buffer will be created.
