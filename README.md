# About
Contains functions for complex org-capture templates

## insert-todo
````
insert-todo (keyword &optional demote)
````

In my .emacs:
````
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJECT(p)" "NEXT(n)" "WAIT(w@/!)" "DELEGATED(l@/!)" "MAYBE(m)" "|" "DONE(d)" "CANCELED(c@)" "FAILED(f@)")))
````

In a template instead of typing
````
** PROJECT Hard project
*** NEXT Turn on the computer
````
you can type
````
%(insert-todo 2)Hard project
%(insert-todo 3 1)Turn on the computer
````

