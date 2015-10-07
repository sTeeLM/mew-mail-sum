;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mail-sum-msg.el
;; Module:  消息输出
;; Author:  sTeeL <steel.mental@gmail.com>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(defun mew-mail-sum-message-no-echo (msg)
  (if mew-mail-sum-verbose
      (with-current-buffer (get-buffer-create "*Messages*")
        (goto-char (point-max))
        (setq buffer-read-only nil)
        (insert msg)
        (setq buffer-read-only t))))


(defmacro mew-mail-sum-msg (fmt &rest args)
  `(let ((str (format ,fmt ,@args)))
    (mew-mail-sum-message-no-echo (format "[MMS] %s\n" str))))

(provide 'mew-mail-sum-msg)
