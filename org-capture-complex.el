;; obsolete functions -->
(defun insert-task-approve-email ()
  "Approve email"
  (interactive)
  (kill-condition)
  (setq org-inhibit-logging t)
  (todo 2)
  (insert "Подтвердить электронную почту")
  (todo 3 1)
  (insert "Зайти в аккаунт lewwadoo@gmail.com")
  (todo 1)
  (insert "Открыть непрочитанное письмо с сайта, требующего подтверждение почты")
  (todo 1)
  (insert "Перейти по ссылке из письма для подтверждения почты"))

(defun privacy-policy-task ()
  "Политика конфиденциальности приложения"
  (move-beginning-of-line nil)
;  (kill-line nil)
  (kill-line nil)
  (insert (concat (todo 3) "")))

(defun insert-task-terms-of-service ()
  "Insert tasks to deal with Terms of Service"
  (interactive)
  (kill-condition)
  (setq org-inhibit-logging t)
  (todo 3)
  (insert "Просмотреть \"Условия использования приложения\"")
  (todo 1)
  (insert "Согласиться с \"Условиями использования приложения\""))

(defun insert-task-term-of-blocking-account ()
  "Insert task if term of blocking account exists"
  (kill-condition)
  (setq org-inhibit-logging t)
  (todo 3)
  (insert "Скопировать пункт соглашения, в котором описываются условия блокирования аккаунта")
  (todo 1)
  (insert "Выставить время истечения действия аккаунта на минимальный срок начала блокирования"))

;; <-- obsolete functions




(defun insert-todo (keyword &optional demote)
  "Insert todo"
  (with-temp-buffer
    (org-mode)
    (org-insert-todo-heading keyword)
    (org-todo keyword)
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
  (kill-line nil))

(defun insert-task (task keyword demote)
  "Insert task"
  (todo keyword demote)
  (insert task))

(defun whether-insert-function (question true-function false-function)
  "Whether insert function"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (call-interactively function)
    (if false-function
	(call-interactively false-function)))
    (kill-condition)
    (kill-line nil))

(defun whether-add-org-capture-task (question abbreviation demote)
  "Whether add org-capture task"
  (when (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
    (insert (concat "(org-capture 0 " "\"" abbreviation "\")")))
  (kill-condition)
  (kill-line nil))

(defun whether-insert-org-capture-task (question true-abbreviation false-abbreviation)
  "Add TRUE-ABBREVIATION or FALSE-ABBREVIATION org-capture task"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert (concat "(org-capture 0 " "\"" true-abbreviation "\")"))
    (if false-abbreviation
	(insert (concat "(org-capture 0 " "\"" false-abbreviation "\")"))))
    (kill-condition))

(defun whether-add-next-task (question task demote)
  "Whether add next task"
  (kill-condition)
  (kill-line nil)
  (when (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
    (insert-task task 3 demote)
    (insert "\n")))

(defun whether-insert-tasks (question true-tasks false-tasks)
  "Whether add tasks"
  (if (equal ?y (read-char-choice (concat question " (y/n)?") '(?y ?n)))
      (insert-tasks true-tasks)
    (if false-tasks
	(insert-tasks false-tasks)
      (kill-condition))))

(defun insert-tasks-by-autocondition (condition true-tasks false-tasks)
  "Whether add tasks"
  (if condition
      (insert-tasks true-tasks)
    (if false-tasks
	(insert-tasks false-tasks)
      (kill-condition))))

(defun insert-tasks (tasks)
  "Insert TASKS"
  (kill-condition)
  (while tasks
    (insert-task (car tasks) 1 0)
    (setq tasks (cdr tasks))))

(defun insert-org-capture-task (abbreviation)
  "Insert org-capture task ABBREVIATION."
  (kill-condition)
  (org-capture 0 abbreviation))
  
(provide 'org-capture-complex)
