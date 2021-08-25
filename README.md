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

## whether-add-org-capture-task
````
whether-add-org-capture-task (question abbreviation)
````
Type QUESTION that has answer yes or no. If yes then insert org-capture task by its ABBREVIATION.

First evaluate this expression (move cursor to the end of the function and type C-x C-e), then answer the question. If you answer no, then the task will be gone, else the org-capture task will replace the task. Example of calling this function:
````
%(insert-todo 3 1)(whether-add-org-capture-task \"The program is not launched\" \"gtaulapp\")
````
