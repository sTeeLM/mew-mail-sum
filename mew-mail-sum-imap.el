;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mail-sum-imap.el
;; Module:  IMAP协议支持
;; Author:  sTeeL <steel.mental@gmail.com>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(require 'mew)
(require 'mew-mail-sum-msg)
(require 'mew-mail-sum-proto)

;; imap checker程序名
(defcustom mew-mail-sum-imap-checker-proc "imapcheck"
  "*imap checker程序名."
  :group 'mew-env
  :type 'file)

;; 最多同时运行的异步后台imap checker数
(defcustom mew-mail-sum-imap-checker-max-count 10
  "*最多同时运行的异步后台imap checker数."
  :group 'mew-env
  :type 'integer)

;; imap checker 运行时间间隔（秒）
(defcustom mew-mail-sum-imap-checker-interval 10
  "*imap checker 运行时间间隔（秒）."
  :group 'mew-env
  :type 'integer)

;; 保存imap cache的间隔：每udpate n次
(defcustom mew-mail-sum-imap-save-interval 10
  "保存imap cache的间隔：每udpate n次"
  :group 'mew-env
  :type 'integer)

(defconst mew-mail-sum-imap-cache-file ".mew-sum-cache-imap")
(defconst mew-mail-sum-imap-process-name "mew-sum-imap-checker")
(defconst mew-mail-sum-imap-checker-prefix "mew-sum-imap-checker-")

;; 内部数据结构
;; MSGID|MESSAGES|RECENT|UIDNEXT|UIDVALIDITY|UNSEEN
(defvar mew-mail-sum-imap-mbox-alist nil)

;; checker进程列表
(defvar mew-mail-sum-imap-checker-list nil)

;; 已经update数目
(defvar mew-mail-sum-imap-update-count 0)

;; timer
(defvar mew-mail-sum-imap-timer nil)

;; 当前case index
(defvar mew-mail-sum-imap-current-case-index 0)

;; 当前mbox index
(defvar mew-mail-sum-imap-current-mbox-index 0)


;; timer 函数
(defun mew-mail-sum-imap-timer-func ()
  (mew-mail-sum-msg "%s" "mew-mail-sum-imap-timer-func called")
  (if mew-mail-sum-imap-mbox-alist
      (progn
        (let* ((case (nth-value mew-mail-sum-imap-current-case-index mew-mail-sum-imap-mbox-alist))
               (mboxes (cdr case)))

          (if (null mboxes) ; 如果mboxex是空列表
              (setq mew-mail-sum-imap-current-case-index ; 尝试下一个case
                    (% (+ 1 mew-mail-sum-imap-current-case-index)
                       (length mew-mail-sum-imap-mbox-alist)))

                                        ; 否则更新当前case mbox
            (let* ((mbox (nth-value mew-mail-sum-imap-current-mbox-index mboxes)))
              (progn ; 更新
                (mew-mail-sum-msg
                 "mew-mail-sum-imap-timer-func will update mbox '%s:%s'" (car case) (car mbox))
                (mew-mail-sum-imap-update-mbox-count (car case) (car mbox)))
                                        ; 指向下一个mbox
              (setq mew-mail-sum-imap-current-mbox-index
                    (+ 1 mew-mail-sum-imap-current-mbox-index))
                                        ; 如果到了mboxes末尾，尝试下一个case
              (if (>= mew-mail-sum-imap-current-mbox-index (length mboxes))
                  (progn
                    (setq mew-mail-sum-imap-current-case-index
                          (% (+ 1 mew-mail-sum-imap-current-case-index)
                             (length mew-mail-sum-imap-mbox-alist)))
                    (setq mew-mail-sum-imap-current-mbox-index 0))
                )
              )
            )
                                        ; 更新mew-mail-sum-imap-update-count计数，并且保存cache
          (setq mew-mail-sum-imap-update-count  (+ 1 mew-mail-sum-imap-update-count))
          (if (equal 0 (% mew-mail-sum-imap-update-count mew-mail-sum-imap-save-interval))
              (mew-mail-sum-imap-save-mbox-alist))
          )
        )
    )
  )


;; 设置Timer，各种初始化
(defun mew-mail-sum-imap-init ()
  (mew-mail-sum-msg "mew-mail-sum-imap-init called")
  (setq mew-mail-sum-imap-timer
        (run-with-idle-timer
         mew-mail-sum-imap-checker-interval t 'mew-mail-sum-imap-timer-func))
  nil)

;; 删除timer，杀死所有checker进程
(defun mew-mail-sum-imap-quit ()
  (mew-mail-sum-msg "mew-mail-sum-imap-quit called")
  (mew-mail-sum-imap-save-mbox-alist)
  (when mew-mail-sum-imap-timer
    (progn
      (cancel-timer mew-mail-sum-imap-timer)
      (setq mew-mail-sum-imap-timer nil)))
  (dolist (proc-name mew-mail-sum-imap-checker-list)
    (delete-process proc-name))
  (setq mew-mail-sum-imap-checker-list nil)
  (setq mew-mail-sum-imap-mbox-alist nil)
  (setq mew-mail-sum-imap-current-case-index 0)
  (setq mew-mail-sum-imap-current-mbox-index 0)
  nil)

;; 获取msgid
(defun mew-mail-sum-imap-load-msgid (case mbox)
  (mew-mail-sum-msg "mew-mail-sum-imap-load-msgid called %s %s" case mbox)    
  (let* ((case:mbox (concat case ":" mbox))
         (msgid-file (mew-expand-file case:mbox mew-imap-msgid-file))
         (msgid 0))
    (condition-case err
        (if (file-exists-p msgid-file)
            (setq msgid (string-to-number (mew-lisp-load msgid-file)))
          (progn
            (mew-mail-sum-msg "load msgid file %s error : not exist" msgid-file)
            0))
      (error (progn (mew-mail-sum-msg
                     "load msgid file %s for %s %s error %s"
                     msgid-file case mbox (error-message-string err) ) 0))))
  )


;; 重新构建mailbox列表
(defun mew-mail-sum-imap-rebuild-mbox-alist ()
  (mew-mail-sum-msg "mew-mail-sum-imap-rebuild-mbox-alist called")
  (let (case alist mbox-alist ret-alist) ;; 重新构建列表
    (dolist (case mew-config-cases)
      (if (eq (mew-mailbox-type case) 'imap)
          (progn
            (setq alist (mew-imap-folder-alist case))
            (setq mbox-alist nil)
            (dolist (mbox alist)
              (let* ((mbox-name (car mbox))
                     (time-stamp (cons "time-stamp" nil))
                     (data (cons "data" (list (mew-mail-sum-imap-load-msgid case mbox-name) 0 0 0 0 0)))
                     )
                (setq mbox-alist
                      (cons
                       (cons mbox-name (list time-stamp data))
                       mbox-alist
                       )
                      )
                ))
            (setq ret-alist (cons (cons case mbox-alist) ret-alist))
            )
        )
      )
    ret-alist
    )
  )


;; 保存mailbox列表
(defun mew-mail-sum-imap-save-mbox-alist ()
  (mew-mail-sum-msg "mew-mail-sum-imap-save-mbox-alist called")
  (condition-case err
      (if mew-mail-sum-imap-mbox-alist
          (mew-lisp-save
           (concat mew-mail-path "/"
                   mew-mail-sum-imap-cache-file)
           mew-mail-sum-imap-mbox-alist 'nobackup 'unlimit))
    (error (progn (mew-mail-sum-msg
                   "save mbox alist error %s"
                   (error-message-string err))
                  (setq mew-mail-sum-imap-mbox-alist nil)))))


;; 装载已有mailbox列表
(defun mew-mail-sum-imap-load-mbox-alist ()
  (mew-mail-sum-msg "mew-mail-sum-imap-load-mbox-alist called")
  (condition-case err
      (setq mew-mail-sum-imap-mbox-alist
            (mew-lisp-load
             (concat mew-mail-path "/"
                     mew-mail-sum-imap-cache-file)))
    (error (progn (mew-mail-sum-msg
                   "load mbox alist error %s"
                   (error-message-string err))
                  (setq mew-mail-sum-imap-mbox-alist nil)
                  ))))

;; 以alist为模版，将老mew-mail-sum-imap-mbox-alist数据合并mailbox列表中，并更新mew-mail-sum-imap-mbox-alist
(defun mew-mail-sum-imap-merge-mbox-alist (alist)
  (mew-mail-sum-msg "mew-mail-sum-imap-merge-mbox-alist called")
  (dolist (case-to alist)
    (dolist (mbox-to (cdr case-to))
      (let* ((val-to (cdr mbox-to))
             (mbox-name-to (car mbox-to))
             (time-stamp-to (nth-value 0 val-to))
             (data-to (nth-value 1 val-to))
             (mbox-from (assoc mbox-name-to (assoc (car case-to) mew-mail-sum-imap-mbox-alist)))
             )
        (if mbox-from
            (let* ((val-from (cdr mbox-from))
                   (time-stamp-from (nth-value 0 val-from))
                   (data-from (nth-value 1 val-from)))
              (mew-mail-sum-msg "merge %s:%s" (car case-to) (car mbox-to))
              (setf (cdr time-stamp-to) (cdr time-stamp-from))
              (setf (cdr data-to) (cdr data-from))
              )))))
    (setq mew-mail-sum-imap-mbox-alist alist))


;; 启动mew时会调用：mew-init-p ＝nil
;; Z 时会调用：mew-init-p ＝t  
(defun mew-mail-sum-imap-status-update ()
  (let (tmp-alist)
    (mew-mail-sum-imap-cache-passwd)
    (if (not mew-init-p)
        ; mew启动时,只需要装载cache
        (progn 
          (mew-mail-sum-msg "mew-mail-sum-imap-status-update called at init")
          (mew-mail-sum-imap-load-mbox-alist)
          (if (not mew-mail-sum-imap-mbox-alist)
              (setq mew-mail-sum-imap-mbox-alist
                    (mew-mail-sum-imap-rebuild-mbox-alist)))
          (mew-mail-sum-imap-save-mbox-alist))
      ; Z 后，配置可能有更新需要merge配置
      (progn
        (mew-mail-sum-msg "mew-mail-sum-imap-status-update called after init")        
        (setq tmp-alist (mew-mail-sum-imap-rebuild-mbox-alist))
        (mew-mail-sum-imap-merge-mbox-alist tmp-alist)
        (mew-mail-sum-imap-save-mbox-alist)
        )
      )))

(defun mew-mail-sum-dump-mbox-list ()
  (let (case mbox)
    (dolist (case mew-mail-sum-imap-mbox-alist)
      (message "case %s:" (car case))
      (dolist (mbox (cdr case))
        (let* ((val (cdr mbox))
               (time-stamp (nth-value 0 val))
               (data (nth-value 1 val))
               )
          (message "   %s --> %s:%s %s:(%d %d %d %d %d %d)"
                   (car mbox)
                   (car time-stamp)
                   (if (cdr time-stamp)
                       (format-time-string "%Y-%m-%d %H:%M:%S" (cdr time-stamp))
                     "N/A")
                   (car data)
                   (nth-value 0 (cdr data))
                   (nth-value 1 (cdr data))
                   (nth-value 2 (cdr data))
                   (nth-value 3 (cdr data))
                   (nth-value 4 (cdr data))
                   (nth-value 5 (cdr data))
                   ))))))

;; 缓存password，如果已经缓存，则不操作
(defun mew-mail-sum-imap-cache-passwd-by-case (case)
  (let* ((prompt (format "IMAP password (%s): " (mew-imap-passtag2 case)))
         (pass (mew-input-passwd prompt (mew-imap-passtag2 case))))
    pass))

;; 缓存所有passwd
(defun mew-mail-sum-imap-cache-passwd ()
  (setq mew-mail-sum-imap-passwd-alist nil)
  (dolist (case mew-config-cases)
    (if (eq (mew-mailbox-type case) 'imap)
        (mew-mail-sum-imap-cache-passwd-by-case case))
    )
  )


;; 获取缓存的password，如果缓存没有，提示用户输入
(defun mew-mail-sum-imap-get-cached-passwd (case)
  (let (passwd)
    (setq passwd (mew-passwd-get-passwd (mew-imap-passtag2 case)))
    (if (null passwd)
        (setq passwd (mew-mail-sum-imap-cache-passwd-by-case case)))
    passwd))

;; 生成imap checker进程名
(defun mew-mail-sum-imap-encode-process-name (case mbox)
  (concat "*" mew-mail-sum-imap-process-name "/" case ":" mbox "*"))

;; 非常简单的信号量
(defun mew-mail-sum-imap-checker-process-inc (proc-name &optional override-max)
  (mew-mail-sum-msg "mew-mail-sum-imap-checker-process-inc %s" proc-name)
  (if (or override-max (< (length mew-mail-sum-imap-checker-list)
                          mew-mail-sum-imap-checker-max-count))
      (if (not (member proc-name mew-mail-sum-imap-checker-list))
          (push proc-name mew-mail-sum-imap-checker-list)
        nil)
    nil)
  )

;; 非常简单的信号量
(defun mew-mail-sum-imap-checker-process-dec (proc-name)
  (mew-mail-sum-msg "mew-mail-sum-imap-checker-process-dec %s" proc-name)
  (and (member proc-name mew-mail-sum-imap-checker-list)
       (setq mew-mail-sum-imap-checker-list
             (remove proc-name mew-mail-sum-imap-checker-list))))

;; 设置进程属性值
(defun mew-mail-sum-imap-checker-get (proc key)
  (let ((r-key (concat mew-mail-sum-imap-checker-prefix key)))
    (process-get proc (intern r-key))))

;; 获得进程属性值
(defun mew-mail-sum-imap-checker-set (proc key val)
  (let ((r-key (concat mew-mail-sum-imap-checker-prefix key)))
    (process-put proc (intern r-key) val)))

;; 更新一个imap邮箱的data数据，更新timestamp
;; MSGID|MESSAGES|RECENT|UIDNEXT|UIDVALIDITY|UNSEEN
(defun mew-mail-sum-imap-update-and-rotate (case mbox value)
  (mew-mail-sum-msg "update %s:%s -> %s" case mbox value)
  (let* (
         (mboxes (assoc case mew-mail-sum-imap-mbox-alist))
         (mbox (assoc mbox mboxes))
         (mbox-name (car mbox))
         (val (cdr mbox))
         (time-stamp (nth-value 0 val))
         (data (nth-value 1 val)))
    (setf (cdr time-stamp) (current-time))
    (setf (cdr data) (append (list (mew-mail-sum-imap-load-msgid case mbox-name)) value))
    )
  )

(defun mew-mail-sum-imap-parse-checker-output (output)
  (mew-mail-sum-msg "mew-mail-sum-imap-parse-checker-output called %s" output)
  (let ((val (split-string output "[ ]" t) ))
    (setq messages (string-to-number (nth-value 0 val)))
    (setq recent (string-to-number (nth-value 1 val)))
    (setq uidnext (string-to-number (nth-value 2 val)))
    (setq uidvalidity (string-to-number (nth-value 3 val)))
    (setq unseen (string-to-number (nth-value 4 val)))
    (list messages recent uidnext uidvalidity unseen)))

;; checker进程退出
(defun mew-mail-sum-imap-update-mbox-sentinel (proc event)
  (mew-mail-sum-msg "[sentinel %s] return %s" proc event)
  (mew-mail-sum-imap-checker-process-dec (process-name proc))
  (let ((case (mew-mail-sum-imap-checker-get proc "case"))
        (mbox (mew-mail-sum-imap-checker-get proc "mbox"))
        (output (mew-mail-sum-imap-checker-get proc "output"))
        (proc-status (process-status proc))
        (proc-exit-code (process-exit-status proc)))
    (mew-mail-sum-msg "[sentinel %s] '%s:%s' output '%s'" proc case mbox output)
    (cond
     ((or (eq proc-status 'run) (eq proc-status 'stop))
      (progn
        (mew-mail-sum-msg "[sentinel %s] still running or stoped, kill it" proc)
        (kill-process proc)))
     ((eq proc-status 'signal)
      (progn
        (mew-mail-sum-msg "[sentinel %s] killed by signal %d" proc proc-exit-code)))
     ((eq proc-status 'exit)
      (progn
        (mew-mail-sum-msg "[sentinel %s] exit with status %d" proc proc-exit-code)
        (if (= 0 proc-exit-code)
            (let* ((msgid (mew-mail-sum-imap-load-msgid case mbox))
                   (val (mew-mail-sum-imap-parse-checker-output output)) )
              (mew-mail-sum-imap-update-and-rotate case mbox val))))))))

;; 获取checker进程的输出
(defun mew-mail-sum-imap-update-mbox-filter (proc string)
  (mew-mail-sum-msg "[filter %s] return %s" proc string)
  (let ((outval  (mew-mail-sum-imap-checker-get proc "output")))
    (setq outval (concat outval string))
    (mew-mail-sum-imap-checker-set proc "output" outval)))

;;
(defun mew-mail-sum-imap-update-mbox-count (case mbox &optional override-max)
  (if (and case mbox)
      (mew-mail-sum-imap-update-mbox case mbox override-max)
    (let (case mbox)
      (dolist (case mew-mail-sum-imap-mbox-alist)
        (dolist (mbox (cdr case))
          (mew-mail-sum-imap-update-mbox (car case) (car mbox) override-max))))))

;; 启动一个checker进程
(defun mew-mail-sum-imap-update-mbox (case mbox &optional override-max)
  (mew-mail-sum-msg "mew-mail-sum-imap-update-mbox-count called %s %s" case mbox override-max)
  (let* ((process-connection-type nil)
         (proc-path mew-mail-sum-imap-checker-proc)
         (server (mew-imap-server case))
         (port (mew-imap-port case))
         (sslp (mew-imap-ssl case))
         (sslport (mew-imap-ssl-port case))
         (user (mew-imap-user case))
         (passwd (mew-mail-sum-imap-get-cached-passwd case))
         (mailbox (mew-imap-utf-7-encode-string
                   (mew-imap-bnm-to-mailbox mbox)))
         (proc-name (mew-mail-sum-imap-encode-process-name case mbox))
         (process))
    (if (numberp port)
        (setq port (number-to-string port)))
    (if (numberp sslport)
        (setq sslport (number-to-string sslport)))
    (if (null passwd)
        (mew-mail-sum-msg "passwd is nil")
      (if (not (mew-mail-sum-imap-checker-process-inc proc-name override-max))
          (mew-mail-sum-msg "max number of checker process reached...")
        (progn
          (setq process (condition-case err
                            (start-process
                             proc-name
                             nil
                             proc-path
                             "-s"
                             server
                             "-p"
                             (if sslp sslport port)
                             "-u"
                             user
                             "-w"
                             passwd
                             "-m"
                             mailbox
                             "-S"
                             (if sslp "yes" "no")
                             )
                          (error (progn
                                   (mew-mail-sum-msg "can not start imap checker: %s"
                                                     (error-message-string err))
                                   (mew-mail-sum-imap-checker-process-dec proc-name)
                                   nil))))
          (if process
              (progn
                (set-process-sentinel process 'mew-mail-sum-imap-update-mbox-sentinel)
                (set-process-filter process 'mew-mail-sum-imap-update-mbox-filter)
                (mew-mail-sum-imap-checker-set process "case" case)
                (mew-mail-sum-imap-checker-set process "mbox" mbox)
                (process-send-string process "START\n"))))))))

;; 如果切换summary，则自动加入一个更新请求
(defun mew-mail-sum-imap-ls-no-scan (case mbox)
  (if (null case)
      (setq case "default"))
  (mew-mail-sum-msg "mew-mail-sum-imap-ls-no-scan called %s %s" case mbox)
  (mew-mail-sum-imap-update-mbox-count case mbox t)
  )

(defun mew-mail-sum-imap-get-flag (case mbox)
  (let ((proc-name (mew-mail-sum-imap-encode-process-name case mbox)))
        (if (member proc-name mew-mail-sum-imap-checker-list)            
        "*" "")))

(defun mew-mail-sum-imap-get-mail-count (data)
  (let ((msgid (nth-value 0 data))
        (messages (nth-value 1 data))
        (uidnext (nth-value 3 data))
        ret)
    ;; messages是0 或者msgid >= messages
    (if (or (equal 0 messages) (>= msgid messages))
        (setq ret 0) ;; 显示为0
      (progn
        (if (equal 0 msgid) ;; 如果msgid == 0
            (setq ret messages) ;; 显示messages
          (setq ret (- (- uidnext msgid) 1)))
        )
      )
    ret
    )
  )

;; 获取mbox列表
(defun mew-mail-sum-imap-generate-entries (case-prefix mbox-prefix no-zero-na)
  (mew-mail-sum-msg "mew-mail-sum-imap-generate-entries called case-prefix:'%s' mbox-prefix:'%s'" case-prefix mbox-prefix)
  (if (not (null mbox-prefix))
      (setq mbox-prefix (concat "%" mbox-prefix)))
  (let (entries skip)
    (dolist (case mew-mail-sum-imap-mbox-alist)
      (if (or (null case-prefix) (string-prefix-p case-prefix (car case)))
          (dolist (mbox (cdr case))
            (if (or (null mbox-prefix) (string-prefix-p mbox-prefix (car mbox)))
                (let* ((case-name (car case))
                       (mbox-name (car mbox))
                       (case-name (car case))
                       (val (cdr mbox))
                       (time-stamp (nth-value 0 val))
                       (data (nth-value 1 val))
                       (mail-str
                        (if (cdr time-stamp)
                            (format "%d" (mew-mail-sum-imap-get-mail-count (cdr data)))
                          "???"))
                       (time-stamp-str
                        (if (cdr time-stamp)
                            (format-time-string "%Y%m%d %H:%M:%S" (cdr time-stamp)) "N/A"))
                       )
                  
                  (setq skip (and no-zero-na (or (string= time-stamp-str "N/A") (string= mail-str "0") (string= mail-str "???")))) 

                  (if (not skip)
                      (push 
                       (list
                        (list case-name  mbox-name) ; key
                        (vector                                 ; value
                         (mew-mail-sum-imap-get-flag case-name mbox-name) ; U
                         "%"                                              ; P
                         time-stamp-str ; LAST-UPDATE
                         mail-str ; MAIL
                         case-name                                        ; CASE
                         mbox-name                                        ; MBOX
                         ))
                       entries))

                  )))))entries))

(mew-mail-sum-regist-proto 'imap
                           'mew-mail-sum-imap-init
                           'mew-mail-sum-imap-quit
                           'mew-mail-sum-imap-status-update
                           'mew-mail-sum-imap-ls-no-scan
                           'mew-mail-sum-imap-update-mbox-count
                           'mew-mail-sum-imap-generate-entries)
(provide 'mew-mail-sum-imap)
