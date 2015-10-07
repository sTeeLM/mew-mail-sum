;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mail-sum.el
;; Module:  主模块
;; Author:  sTeeL <steel.mental@gmail.com>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(require 'mew)
(require 'mew-mail-sum-msg)
(require 'mew-mail-sum-proto)
(require 'mew-mail-sum-imap)
(require 'tabulated-list)

(defcustom mew-mail-sum-refresh-interval 1
  "*列表刷新间隔（秒）."
  :group 'mew-env
  :type 'integer)

;; 在*Messages*中显示详细信息
(defvar mew-mail-sum-verbose nil)


(defun mew-mail-sum-status-update ()
  (mew-mail-sum-proto-call-all 'status-update)
  )

(defun mew-mail-sum-init ()
  (mew-mail-sum-proto-call-all 'init)
  )

(defun mew-mail-sum-quit()
  (let ((buffer (get-buffer mew-mail-sum-buffer-name)))
    (mew-mail-sum-proto-call-all 'quit)
    ;; kill all buffers
    (when buffer
      (kill-buffer buffer))))

(defun mew-mail-sum-summary-ls-no-scan()
  (let (case proto fld)
    (mew-set '(case proto) (mew-summary-case-proto))
    (setq fld (mew-case:folder-folder (buffer-name)))
    (when (mew-summary-exclusive-p)
      (mew-mail-sum-proto-call (mew-mail-sum-proto-test-proto proto) 'ls-no-scan case fld))))
  
;; 几个hook
(add-hook 'mew-status-update-hook 'mew-mail-sum-status-update)
(add-hook 'mew-init-hook 'mew-mail-sum-init)
(add-hook 'mew-quit-hook 'mew-mail-sum-quit)
(add-hook 'mew-summary-ls-no-scan-hook 'mew-mail-sum-summary-ls-no-scan)


;; 使用自定义的biff函数
(defun mew-mail-sum-biff-bark (n)
  (if (= n 0)
      (setq mew-biff-string "")
        (setq mew-biff-string (format "  [Mail(%d)]"  n))))

;; 获取当前邮箱新邮件数
(defun mew-mail-sum-retrieve-current-mail-count ()
    (interactive)
    (let (case proto fld)
      (mew-set '(case proto) (mew-summary-case-proto))
      (setq fld (buffer-name))
      (when (mew-summary-exclusive-p)
        (cond
         ((mew-folder-popp (mew-case:folder-folder fld))
          (mew-pop-retrieve case 'biff fld))
         ((mew-folder-imapp (mew-case:folder-folder fld))
          (mew-imap-retrieve case 'biff fld))
         ))))

(setq mew-biff-function 'mew-mail-sum-biff-bark)

;; 按i键获得当前邮箱新邮件数
(define-key mew-summary-mode-map "i"    'mew-mail-sum-retrieve-current-mail-count)

;; 按G键展示邮箱列表
(define-key mew-summary-mode-map "G"    'mew-mail-sum-list-buffers)


;; mew mail summary mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mew-mail-sum-buffer-name "*Mailbox List*")

(defvar mew-mail-sum-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map))

(define-key mew-mail-sum-mode-map "v" 'mew-mail-sum-select)
(define-key mew-mail-sum-mode-map (kbd "RET") 'mew-mail-sum-select)
(define-key mew-mail-sum-mode-map "g" 'mew-mail-sum-go)
(define-key mew-mail-sum-mode-map "G" 'mew-mail-sum-go)
(define-key mew-mail-sum-mode-map "u" 'mew-mail-sum-update)
(define-key mew-mail-sum-mode-map "U" 'mew-mail-sum-update-all)
(define-key mew-mail-sum-mode-map "r" 'mew-mail-sum-refresh)
(define-key mew-mail-sum-mode-map "f" 'mew-mail-sum-filter-zero-na)
(define-key mew-mail-sum-mode-map "Q" 'mew-summary-quit)

(define-derived-mode mew-mail-sum-mode tabulated-list-mode "Mew Mail List"
  (add-hook 'tabulated-list-revert-hook 'mew-mail-sum-buffers-revert nil t)
  )

(defun mew-mail-sum-filter-zero-na ()
  (interactive)
  (setq mew-mail-sum-filter-zero-na (not mew-mail-sum-filter-zero-na))
  (mew-mail-sum-refresh)
  )

(defun mew-mail-sum-refresh ()
  (interactive)
  (mew-mail-sum-buffers-refresh)
  (tabulated-list-print t)  
  )

(defun mew-mail-sum-update-all ()
  (interactive)
  (mew-mail-sum-proto-call-all 'update-mbox-count nil nil nil t)
  )

(defun mew-mail-sum-update ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case (car id))
         (mbox (cadr id))
         proto)
    (if mbox
        (progn
          (setq proto (substring mbox 0 1))
          (mew-mail-sum-proto-call
           (mew-mail-sum-proto-test-proto proto)
           'update-mbox-count case mbox t)))))
  

(defun mew-mail-sum-go ()
  (interactive)
  (call-interactively 'mew-mail-sum-list-buffers)
  )

(defun mew-mail-sum-select ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
         case:folder)
    (if id
        (progn 
        (setq case:folder (concat (car id) ":" (cadr id)))
        (mew-summary-visit-folder case:folder t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 私有函数


(defvar mew-mail-sum-generate-entries-proto nil)
(defvar mew-mail-sum-generate-entries-case-prefix nil)
(defvar mew-mail-sum-generate-entries-mbox-prefix nil)
(defvar mew-mail-sum-filter-zero-na nil)
(defvar mew-mail-sum-refresh-timer nil)


(defun mew-mail-sum-refresh-func ()
  (let ((buffer (get-buffer mew-mail-sum-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (mew-mail-sum-refresh))
      (progn
        (cancel-timer mew-mail-sum-refresh-timer)
        (setq mew-mail-sum-refresh-timer nil)))))


(defun mew-sum-kick-refresh-timer ()
  (if (null mew-mail-sum-refresh-timer)
      (setq mew-mail-sum-refresh-timer
            (run-with-idle-timer
             mew-mail-sum-refresh-interval t 'mew-mail-sum-refresh-func))))

(defun mew-mail-sum-generate-entries ()
  (let ((proto mew-mail-sum-generate-entries-proto)
        (case-prefix mew-mail-sum-generate-entries-case-prefix)
        (mbox-prefix mew-mail-sum-generate-entries-mbox-prefix)
        (no-zero-na mew-mail-sum-filter-zero-na)
        entries)
    (if (null proto)
        (setq entries (mew-mail-sum-proto-call-all
                       'generate-entries
                       'append
                       case-prefix mbox-prefix no-zero-na))
      (setq entries (mew-mail-sum-proto-call
                     (mew-mail-sum-proto-test-proto proto)
                     'generate-entries
                     case-prefix mbox-prefix no-zero-na)))
    entries))


(defun mew-mail-sum-buffers-refresh ()
   (setq tabulated-list-format
	  (vector '("U" 1 t :pad-right 0)
                  '("P" 2 t :pad-right 0)
		  '("LAST-UPDATE" 20 t :pad-right 0 :left-align t)
                  '("MAIL" 10 mew-mail-sum-sort-mail :right-align t)
                  '("CASE" 8 t)
		  '("MBOX" 8 t)))
   (setq tabulated-list-use-header-line t)
   (setq tabulated-list-entries 'mew-mail-sum-generate-entries)
   (tabulated-list-init-header)
  )


(defun mew-mail-sum-sort-mail (el1 el2)
  (let* ((val1 (cadr el1))
         (val2 (cadr el2))
         (mail1 (string-to-number (aref val1 3)))
         (mail2 (string-to-number (aref val2 3)))
         )
    (<= mail1 mail2)))

(defun mew-mail-sum-input-folder ()
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((mew-input-complete-function 'mew-complete-folder)
         (mew-circular-complete-function 'mew-circular-complete-case:)
         (init (mew-mail-sum-merge-proto-case-folder))
         ;; mew-inherit-case must be nil
         (ret (read-from-minibuffer (format "Folder name: ")
                                    init mew-input-folder-map nil
                                    'mew-input-folder-hist)))
    ret))


(defun mew-mail-sum-merge-proto-case-folder ()
  (let ((proto mew-mail-sum-generate-entries-proto)
        (case-prefix mew-mail-sum-generate-entries-case-prefix)
        (mbox-prefix mew-mail-sum-generate-entries-mbox-prefix))
    (concat
     (or case-prefix "default") ":" (or proto "") (or mbox-prefix "")))
  )

;; nil      -> nil nil nil
;; ""       -> nil nil nil
;; ":"      -> nil nil nil
;; "xx"     -> xx nil nil
;; "%xx"    -> nil % xx
;; "xx:"    -> xx nil nil
;; "xx:%"   -> xx % nil
;; "xx:%yy" -> xx % yy
;; ":%"     -> % nil
;; ":xx"    -> nil nil xx
;; ":%xx"   -> nil % xx 
(defun mew-mail-sum-parse-proto-case-folder (case-folder)
  (let
      (proto case folder split-ret prefix)
    (if (or (null case-folder) (string= case-folder "") (string= case-folder ":"))
        (list nil nil nil)
      (progn
        (setq split-ret (split-string case-folder "[ :]" t))
        (cond
         ((= 0 (length split-ret)) (list nil nil nil))
         ((= 1 (length split-ret))
          (progn
            (setq prefix (substring case-folder 0 1))
            (cond
             ((string=  prefix ":")
                (progn
                  (setq proto (substring (car split-ret) 0 1))
                  (setq folder (substring (car split-ret) 1 nil))
                  (cond
                   ((or (string= proto "%") (string= proto "$")
                        (string= proto "+") (string= proto "*")
                        (string= proto "-"))
                    (list nil proto folder))
                   (t (list nil nil (car split-ret))))))
             ((or (string= prefix "%") (string= prefix "$")
                  (string= prefix "+") (string= prefix "*")
                  (string= prefix "-"))
              (list nil prefix (substring case-folder 1 nil)))
             (t (list (car split-ret) nil nil)))))
         ((< 1 (length split-ret))
          (progn
            (setq proto (substring (cadr split-ret) 0 1))
            (setq folder (substring (cadr split-ret) 1 nil))
            (setq case (car split-ret))
            (cond
             ((or (string= proto "%") (string= proto "$")
                  (string= proto "+") (string= proto "*")
                  (string= proto "-")) (list case proto folder))
             (t (list case nil (cadr split-ret)))))))))))


(defun mew-mail-sum-list-buffers (&optional arg)
  (interactive "P")
  (mew-mail-sum-msg "mew-mail-sum-list-buffers called %s" (called-interactively-p))
  (let (case-folder)
    (if (called-interactively-p)
        (progn
          (setq case-folder (mew-mail-sum-input-folder))
          (mew-set '(mew-mail-sum-generate-entries-case-prefix
                     mew-mail-sum-generate-entries-proto
                     mew-mail-sum-generate-entries-mbox-prefix)
                   (mew-mail-sum-parse-proto-case-folder case-folder)))
      (progn
        (setq mew-mail-sum-generate-entries-proto nil)
        (setq mew-mail-sum-generate-entries-case-prefix nil)
        (setq mew-mail-sum-generate-entries-mbox-prefix nil)
        ))
    (switch-to-buffer (mew-mail-sum-list-buffers-noselect))
    (mew-sum-kick-refresh-timer)
    ))


(defun mew-mail-sum-list-buffers-noselect ()
  (let ((old-buffer (current-buffer))
	(buffer (get-buffer-create mew-mail-sum-buffer-name)))
    (with-current-buffer buffer
      (mew-mail-sum-mode)
      (mew-mail-sum-buffers-refresh)
      (tabulated-list-print))
    buffer))

(defun mew-mail-sum-buffers-revert ()
  (message "mew-mail-sum-buffers-revert called")
  )

(provide 'mew-mail-sum)
