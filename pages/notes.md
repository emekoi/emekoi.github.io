---
title: Notes
naked-main: true
---

Short-form writings.

::: {.note date="2024-08-09"}
# `thingatpt.el`

Emacs's `thingatpt.el` provides an API for defining text objects known as 'things', and querying the buffer for them. I want to add my own 'things' but the documentation isn't that clear (they say to do one thing, but do another internally).
However, after reading most of the functions `thingatpt.el` defines, things become a lot clearer. In short, there are a few requirements to define a 'thing':

1. You _must_ define `'forward-op` to use `forward-thing`. This is the only operation you have to define since all other operations can be emulated with just `forward-thing`. So for the rest of the operations we will talk about, it is assumed `'forward-op` is either `nil` or are more efficient implementation exists.

   ``` emacs-lisp
   (put 'wordv2 'forward-op
     (lambda (&optional count)
       (interactive "p^")
       (setq count (or count 1))
       (forward-word count)))
   ```

2. You _may_ define both `'beginning-op` and `'end-op`, or just `'bounds-of-thing-at-point` to use `bounds-of-thing-at-point`. If `'forward-op` is not defined, you _must_ define either one to use `bounds-of-thing-at-point.`

   ``` emacs-lisp
   (put 'bufferv2 'end-op
     (lambda () (goto-char (point-max))))
   (put 'bufferv2 'beginning-op
     (lambda () (goto-char (point-min))))

   (put 'bufferv2 'bounds-of-thing-at-point
     (lambda () (cons (point-min) (point-max))))
   ```

3. You _may_ define `'thing-at-point` directly to use `thing-at-point`.

   ``` emacs-lisp
   (put 'filenamev2 'thing-at-point
     (lambda (&optional _lax _bounds)
        (when-let ((filename (thing-at-point 'filename)))
          (setq filename (expand-file-name filename))
          (and (file-exists-p filename) filename))))
   ```
:::
