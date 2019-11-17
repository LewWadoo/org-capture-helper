(defun insert-todo (keyword &optional demote)
  "Insert todo"
  (with-temp-buffer
    (org-mode)
    (org-insert-todo-heading keyword)
    (org-todo keyword)
    (if demote
	(if (= 1 demote)
	    (org-do-demote)
	  (if (= -1 demote)
	      (org-do-promote))))
    (buffer-substring-no-properties (point-min)(point-max))))

(defun todo (keyword &optional demote)
  "Return any org-todo heading"
  (org-insert-heading-respect-content)
      (org-todo keyword)
  (if demote
      (if (= 1 demote)
	  (org-do-demote)
	(if (= -1 demote)
	    (org-do-promote)))))
  
(defun kill-condition ()
  "Kill condition for inserting task"
  (move-beginning-of-line nil)
  (kill-line nil))

(defun insert-task (task keyword &optional demote)
  "Insert task"
  (todo keyword demote)
  (insert task))

(defun whether-insert-task (question insert-task-function)
  "Whether add next task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (call-interactively insert-task-function)
    (kill-condition)
    (kill-line nil)))

(defun whether-add-org-capture-task (question abbreviation &optional demote)
  "Whether add org-capture task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert (concat "(org-capture 0 " "\"" abbreviation "\")"))
    (kill-condition)
    (kill-line nil)))

(defun whether-add-next-task (question task &optional demote)
  "Whether add next task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-task task 3 demote)
    (kill-condition)
    (kill-line nil)))

(defun whether-insert-tasks (question true-tasks &optional false-tasks)
  "Whether add tasks"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-task true-tasks)
    (if false-tasks
	(insert-task false-tasks)
      (kill-condition))))

(defun insert-tasks (tasks)
  "Insert TASKS"
  (kill-condition)
  (while tasks
    (insert-task (car tasks) 1)
    (setq tasks (cdr tasks))))
  
(provide 'org-capture-complex)
