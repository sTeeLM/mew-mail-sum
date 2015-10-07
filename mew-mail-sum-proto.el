;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mail-sum-proto.el
;; Module:  协议扩展支持
;; Author:  sTeeL <steel.mental@gmail.com>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(require 'mew-mail-sum-msg)

(defvar mew-mail-sum-proto-alist nil)

(defun mew-mail-sum-regist-proto (proto
     init quit status-update ls-no-scan update-mbox-count generate-entries)
  (let ((entry (assoc proto mew-mail-sum-proto-alist)))
        (if (null entry)
            (setq mew-mail-sum-proto-alist
                  (cons
                   (cons
                    proto
                    (vector
                     init
                     quit
                     status-update
                     ls-no-scan
                     update-mbox-count
                     generate-entries)) mew-mail-sum-proto-alist)))))
  

(defun mew-mail-sum-proto-call (proto func &rest args)
  (mew-mail-sum-msg "mew-mail-sum-proto-call called %s %s %s" proto func args)
  (let* ((entry (assoc proto mew-mail-sum-proto-alist))
        (funcs (cdr entry)))
    (if (and proto entry funcs)
        (cond
         ((eq func 'init)(apply (aref funcs 0) args))
         ((eq func 'quit)(apply (aref funcs 1) args))
         ((eq func 'status-update)(apply (aref funcs 2) args))
         ((eq func 'ls-no-scan)(apply (aref funcs 3) args))
         ((eq func 'update-mbox-count)(apply (aref funcs 4) args))
         ((eq func 'generate-entries)(apply (aref funcs 5) args))
         )
      )))


(defun mew-mail-sum-proto-nop (a b)
  )

(defun mew-mail-sum-proto-call-all (func &optional reduce-ret &rest args)
  (mew-mail-sum-msg "mew-mail-sum-proto-call-all called %s %s %s" func reduce-ret args)
  (let (ret)
    (dolist (entry mew-mail-sum-proto-alist)
      (if (null reduce-ret)
          (setq reduce-ret 'mew-mail-sum-proto-nop))
      (setq ret
            (funcall
             reduce-ret
             (cond
              ((eq func 'init)(apply (aref (cdr entry) 0) args))
              ((eq func 'quit)(apply (aref (cdr entry) 1) args))
              ((eq func 'status-update) (apply (aref (cdr entry) 2) args))
              ((eq func 'ls-no-scan)(apply (aref (cdr entry) 3) args))
              ((eq func 'update-mbox-count) (apply (aref (cdr entry) 4) args))
              ((eq func 'generate-entries) (apply (aref (cdr entry) 5) args)))
             ret))
      )
    ret
    ))

(defun mew-mail-sum-proto-test-folder (fld)
  (cond
   ((mew-folder-imapp fld) 'imap)
   ((mew-folder-popp fld) 'pop)
   ((mew-folder-nntpp fld) 'nntp)
   ((mew-folder-localp fld) 'local)
  ))

(defun mew-mail-sum-proto-test-proto (proto)
  (cond
   ((string= "%" proto) 'imap)
   ((string= "$" protp) 'pop)
   ((string= "-" proto) 'nntp)
   ((string= "+") 'local)
  ))

(provide 'mew-mail-sum-proto)  
