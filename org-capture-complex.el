(defun insert-todo (keyword-number-in-sequence &optional demote)
  "Insert todo instead of hard-typing '** TODO'

KEYWORD-NUMBER-IN-SEQUENCE is the number of org-todo-keyword in org-todo-keywords:
````
(setq org-todo-keywords
      '((sequence \"TODO(t)\" \"PROJECT(p)\" \"NEXT(n)\" \"WAIT(w@/!)\" \"DELEGATED(l@/!)\" \"MAYBE(m)\" \"|\" \"DONE(d)\" \"CANCELED(c@)\" \"FAILED(f@)\")))
````

DEMOTE is how far the current task will demote (> 0) or promote (< 0) from the previous task.

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
"
  (with-temp-buffer
    (org-mode)
    (org-insert-todo-heading keyword-number-in-sequence)
    (org-todo keyword-number-in-sequence)
    (when demote
      (while (> demote 0)
	(org-do-demote)
	(setq demote (1- demote)))
      (while (< demote 0)
	(org-do-promote)
	(setq demote (1+ demote))))
    (buffer-substring-no-properties (point-min)(point-max))))

(defun todo (keyword demote)
  "Return any org-todo heading"
  (org-insert-todo-heading keyword)
;      (org-todo keyword)
    (when demote
      (while (> demote 0)
	(org-do-demote)
	(setq demote (1- demote)))
      (while (< demote 0)
	(org-do-promote)
	(setq demote (1+ demote)))))

(defun kill-condition ()
  "Kill condition for inserting task"
  (move-beginning-of-line nil)
  (kill-line 1))

(defun insert-task (task keyword demote)
  "Insert task"
  (todo keyword demote)
  (insert task))

(defun whether-insert-function (question true-function &optional false-function)
  "Whether insert function"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (call-interactively function)
    (if false-function
	(call-interactively false-function)))
    (kill-condition)
    (kill-line nil))

(defun whether-add-org-capture-task (question abbreviation &optional demote)
  "Whether add org-capture task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-org-capture-task abbreviation)
    (kill-condition)))

(defun whether-insert-org-capture-task (question true-abbreviation &optional false-abbreviation)
  "Add TRUE-ABBREVIATION or FALSE-ABBREVIATION org-capture task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-org-capture-task true-abbreviation)
    (if false-abbreviation
	(insert-org-capture-task false-abbreviation)
      (kill-condition))))

(defun whether-add-next-task (question task &optional demote)
  "Whether add next task"
  (kill-condition)
  (when (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
    (insert-task task 3 demote)
    (insert "\n")))

(defun whether-insert-tasks (question true-tasks &optional false-tasks keyword demote)
  "Insert TRUE-TASKS if answer to QUESTION is y, or FALSE-TASKS if answer is n"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-tasks true-tasks keyword demote)
    (if false-tasks
	(insert-tasks false-tasks keyword demote)
      (kill-condition))))

(defun insert-tasks-by-autocondition (condition true-tasks false-tasks)
  "Whether add tasks"
  (if condition
      (insert-tasks true-tasks 1 0)
    (if false-tasks
	(insert-tasks false-tasks 1 0))))

(defun insert-tasks (tasks keyword demote)
  "Insert TASKS"
  (kill-condition)
  (while tasks
    (insert-task (car tasks) keyword demote)
    (setq demote 0)
    (setq tasks (cdr tasks))))

(defun insert-org-capture-task (abbreviation)
  "Insert org-capture task ABBREVIATION."
  (kill-condition)
  (org-capture 0 abbreviation))
  
(provide 'org-capture-complex)
