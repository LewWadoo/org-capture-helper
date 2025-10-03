;;; org-capture-helper.el --- Helper functions for org-capture templates -*- lexical-binding: t; -*-

;; Version: 2.0.0

;;; Commentary:
;; This package provides helper functions for org-capture templates,
;; making it easier to create complex and conditional templates with
;; proper TODO states, indentation, and interactive prompts.
;;
;; Main functions:
;; - `insert-todo`: Insert a TODO item with specified keyword and indentation
;; - `whether-insert-tasks`: Conditionally insert tasks based on yes/no question
;; - `insert-org-capture-task`: Insert an org-capture task with custom arguments
;; - `whether-insert-org-capture-task`: Conditionally insert an org-capture task

;; - `make-file`: Create a file in a directory
;;
;; Version 2.0.0 Changes:
;; - Removed unused functions: `whether-insert-function` and `whether-add-org-capture-task`
;; - Removed redundant function: `whether-add-next-task` (replaced with `whether-insert-tasks`)
;; - Added helper function: `apply-heading-level-change` to reduce code duplication
;; - Streamlined API based on real usage patterns

;;; Code:

(require 'org)
(require 'org-capture)

(defun make-file (file-name directory)
  "Make file FILE-NAME in directory DIRECTORY.
Create directory if it doesn't exist yet."
  (let ((file-name-full (expand-file-name file-name directory)))
    (unless (file-exists-p file-name-full)
      (make-directory directory t)
      (find-file file-name-full))))

(defun insert-todo (keyword-number-in-sequence &optional demote)
  "Insert todo heading with specified keyword and indentation.

KEYWORD-NUMBER-IN-SEQUENCE is the number of org-todo-keyword in org-todo-keywords:
````
(setq org-todo-keywords
      '((sequence \"PROJECT(p)\" \"TODO(t)\" \"NEXT(n)\" \"WAIT(w@/!)\" 
         \"DELEGATED(l@/!)\" \"MAYBE(m)\" \"|\" \"DONE(d)\" \"CANCELED(c@)\" \"FAILED(f@)\")))
````

DEMOTE is how far the current task will demote (> 0) or promote (< 0) from the previous task.

In a template instead of typing
````
** PROJECT Hard project
*** NEXT Turn on the computer
````
you can type
````
%(insert-todo 1)Hard project
%(insert-todo 3 1)Turn on the computer
````
"
  (with-temp-buffer
    (org-mode)
    (org-insert-todo-heading keyword-number-in-sequence)
    (org-todo keyword-number-in-sequence)
    (apply-heading-level-change demote)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun kill-condition ()
  "Kill condition for inserting task.
Removes the current line after cycling visibility."
  (move-beginning-of-line nil)
  (org-cycle)
  (kill-line 1))

(defun apply-heading-level-change (demote)
  "Apply DEMOTE level changes to current heading.
DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (when demote
    (cond
     ;; Demote the heading
     ((> demote 0)
      (dotimes (_ demote) (org-do-demote)))
     ;; Promote the heading
     ((< demote 0)
      (dotimes (_ (abs demote)) (org-do-promote))))))

(defun todo (keyword demote)
  "Create an org-todo heading with KEYWORD index and DEMOTE levels.
KEYWORD is the index of the todo keyword to use.
DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (org-insert-todo-heading keyword)
  (apply-heading-level-change demote))

(defun insert-task (task &optional keyword demote)
  "Insert TASK as an org heading.
Optional KEYWORD specifies the todo keyword index to use.
Optional DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (todo keyword demote)
  (insert task))



(defun whether-insert-org-capture-task (question abbreviation &rest args)
  "Conditionally insert an org-capture task based on yes/no response.
Ask QUESTION that requires a yes or no answer.
If the answer is yes, insert an org-capture task using ABBREVIATION with any additional ARGS.
If the answer is no, do nothing."
  (if (y-or-n-p question)
      (apply #'insert-org-capture-task abbreviation args)
    (kill-condition)))



(defun whether-insert-tasks-3 (question tasks-1 tasks-2 tasks-3 &optional keyword demote)
  "Insert tasks based on a 3-choice response.
Ask QUESTION with 3 possible answers (1, 2, or 3).
Insert TASKS-1, TASKS-2, or TASKS-3 depending on the answer.
Optional KEYWORD specifies the todo keyword index to use.
Optional DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (let ((answer (read-char-choice (concat question " (1/2/3)? ") '(?1 ?2 ?3))))
    (cond
     ((eq answer ?1) (insert-tasks tasks-1 keyword demote))
     ((eq answer ?2) (insert-tasks tasks-2 keyword demote))
     ((eq answer ?3) (insert-tasks tasks-3 keyword demote))
     (t (kill-condition)))))

(defun whether-insert-tasks (question true-tasks &optional false-tasks keyword demote)
  "Conditionally insert tasks based on yes/no response.
Ask QUESTION and insert TRUE-TASKS if answer is yes, 
or FALSE-TASKS if answer is no and FALSE-TASKS is provided.
Optional KEYWORD specifies the todo keyword index to use.
Optional DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (if (y-or-n-p question)
      (insert-tasks true-tasks keyword demote)
    (if false-tasks
        (insert-tasks false-tasks keyword demote)
      (kill-condition))))

(defun insert-tasks (tasks keyword demote)
  "Insert multiple TASKS with specified KEYWORD and DEMOTE level.
TASKS is a list of task descriptions.
KEYWORD specifies the todo keyword index to use.
DEMOTE specifies how many levels to demote (positive) or promote (negative)."
  (kill-condition)
  (let ((initial-demote demote))
    (dolist (task tasks)
      (insert-task task keyword demote)
      ;; Only apply the demote to the first task
      (setq demote 0))))

(defun insert-org-capture-task (abbreviation &rest args)
  "Insert org-capture task using ABBREVIATION with extra arguments ARGS.
ARGS is a list of cons cells where each cell contains (KEY . VALUE)."
  (kill-condition)
  
  ;; Set variables from arguments for use in the template
  (dolist (pair args)
    (when (consp pair)
      (let ((key (car pair))
            (value (cdr pair)))
        (set key value))))
  
  ;; Call org-capture with the abbreviation key
  (condition-case err
      (org-capture 0 abbreviation)
    (error
     (message "Error inserting org capture task with key '%s': %s" 
              abbreviation (error-message-string err)))))

(defun org-capture-helper--in-capture-context-p ()
  "Return non-nil if called within an org-capture template context.
This is a utility function to determine if we should perform
certain operations that only make sense during org-capture."
  (or (bound-and-true-p org-capture-mode)
      (and (boundp 'org-capture-current-plist)
           org-capture-current-plist)))

(defun org-capture-helper-auto-insert-tasks (condition-fn true-tasks &optional false-tasks keyword demote)
  "Automatically insert tasks based on the result of evaluating CONDITION-FN.
CONDITION-FN should be a function that returns a boolean value.
If CONDITION-FN returns nil, insert TRUE-TASKS.
If CONDITION-FN returns non-nil and FALSE-TASKS is provided, insert FALSE-TASKS instead.
TRUE-TASKS and FALSE-TASKS should be lists of strings, each representing a task to insert.
This is similar to `whether-insert-tasks` but automatically decides based on
the function's return value instead of asking the user."
  ;; Kill the current line in capture context (like kill-condition)
  ;; (when (org-capture-helper--in-capture-context-p)
  ;;   (kill-condition))
  
  ;; Evaluate condition and return appropriate tasks
  (let ((result (and (functionp condition-fn)
                    (funcall condition-fn))))
    (if result
      (insert-tasks true-tasks keyword demote)
      (if false-tasks
        (insert-tasks false-tasks keyword demote)
	(kill-condition)))))

(provide 'org-capture-helper)
;;; org-capture-helper.el ends here
