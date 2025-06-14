

(defun make-file (file-name directory)
  "Make file FILE-NAME in directory DIRECTORY."
  (let ((file-name-full (expand-file-name file-name directory)))
    (unless (file-exists-p file-name-full)
      (make-directory directory t)
      (find-file file-name-full))))

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

;; deprecated: use whether-insert-org-capture-task instead
;; (defun whether-add-org-capture-task (question abbreviation)
;;   "Type QUESTION that has answer yes or no. If yes then insert org-capture task by its ABBREVIATION."
;;   (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
;;       (insert-org-capture-task abbreviation)
;;     (kill-condition)))

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
  (org-cycle)
  (kill-line 1))

(defun insert-task (task &optional keyword demote)
  "Insert TASK."
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

(defun whether-insert-org-capture-task (question abbreviation &rest args)
  "Ask QUESTION that requires a yes or no answer. If the answer is yes, insert an org-capture task using ABBREVIATION with any additional ARGS. If the answer is no, do nothing.

This function can be called with any number of additional arguments that will be passed to `insert-org-capture-task`."
  (if (y-or-n-p question)
       (apply 'insert-org-capture-task abbreviation args)
    (kill-condition)))

(defun whether-add-next-task (question task &optional demote)
  "Whether add next task"
  (kill-condition)
  (when (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
    (insert-task task 3 demote)
    (insert "\n")))

(defun whether-insert-tasks-3 (question tasks-1 tasks-2 tasks-3 &optional keyword demote)
  "Insert TASKS-1 if answer to QUESTION is 1, TASKS-2 if answer to QUESTION is 2, or TASKS-3 if answer is 3"
(let ((answer (read-char-choice (concat question " (1/2/3)?") '(?1 ?2 ?3))))
  (if (equal ?1 answer)
      (insert-tasks tasks-1 keyword demote)
    (if (equal ?2 answer)
	(insert-tasks tasks-1 keyword demote)
      (if (equal ?3 answer)
	  (insert-tasks tasks-3 keyword demote)
	(kill-condition))))))

(defun whether-insert-tasks (question true-tasks &optional false-tasks keyword demote)
  "Insert TRUE-TASKS if answer to QUESTION is y, or FALSE-TASKS if answer is n"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-tasks true-tasks keyword demote)
    (if false-tasks
	(insert-tasks false-tasks keyword demote)
      (kill-condition))))

;; (defun insert-tasks-by-autocondition (condition true-tasks false-tasks)
;;   "Whether add tasks"
;;   (if condition
;;       (insert-tasks true-tasks 1 0)
;;     (if false-tasks
;; 	(insert-tasks false-tasks 1 0))))

(defun insert-tasks (tasks keyword demote)
  "Insert TASKS"
  (kill-condition)
  (while tasks
    (insert-task (car tasks) keyword demote)
    (setq demote 0)
    (setq tasks (cdr tasks))))

(defun insert-org-capture-task (abbreviation &rest args)
  "Insert org-capture task ABBREVIATION with extra arguments ARGS.
ARGS is a list of cons cells where each cell contains (KEY . VALUE)."
  (kill-condition)
  
  ;; Store the variables for later use directly
  (dolist (pair args)
    (let ((key (car pair))       ;; The key, e.g., 'branch-name-for-this-story
          (value (cdr pair)))     ;; The associated value
      (set key value)))          ;; Set the variable using the key directly
  
  ;; Call org-capture with the abbreviation key; only 2 args are supported
  (org-capture 0 abbreviation))

(provide 'org-capture-helper)
