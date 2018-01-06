;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'log4e)
(require 'tco)

(put 'ss-exit
     'error-conditions
     '(ss-errors ss-exit))

(put 'ss-exit 'error-message "ss-rpc error:")



(log4e:deflogger "ss" "%t [%l] %m" "%H:%M:%S")
(ss--log-enable-logging)
(ss--log-enable-messaging)
(ss--log-set-level 'info)


(defvar ss:read-timeout 12
  "time (in seconds) of waiting command answer")

(defvar ss:connection-timeout 15
  "time (in seconds) of waiting server response on starting")


(defun ss:send-list! (proc lst)
  (ss--log-debug "{%s} sending %s" (process-name proc) (prin1-to-string lst))
  (process-send-string proc (prin1-to-string lst)))



(defun ss:read (proc)
  (accept-process-output proc ss:read-timeout)

  (with-current-buffer (process-buffer proc)
    (goto-char 1)
    (ss--log-debug "{%s} received \"%s\"" (process-name proc) (buffer-string))

    (prog1
        (read (current-buffer))
      (erase-buffer))))



(defun ss:handle-error! (proc error-message)
  (ss--log-error "%s %s" (process-name proc) error-message)
  (signal 'ss-exit (list error-message)))


(defun-tco ss:handle-r-re-c-cv (proc x)
  (cond
   ((eq (car x) 'return) (cadr x))
   (t (case (car x)
        (return-error (ss:handle-error! proc (cl-second x)))
        (call (ss:handle-call! proc (cl-second x) (cl-third x))
              (ss:handle-r-re-c-cv proc (ss:read proc)))
        (call-void (ss:handle-call-void! proc (cl-second x) (cl-third x))
                   (ss:handle-r-re-c-cv proc (ss:read proc)))
        (otherwise (error "expected return | return-error | call | call-void message, received %s" x))))))


(defun-tco ss:handle-rv-re-c-cv (proc x)
  (case (car x)
    (return-void nil)
    (return-error (ss:handle-error! proc (cl-second x)))
    (call (ss:handle-call! proc (cl-second x) (cl-third x))
          (ss:handle-rv-re-c-cv proc (ss:read proc)))
    (call-void (ss:handle-call-void! proc (cl-second x) (cl-third x))
               (ss:handle-rv-re-c-cv proc (ss:read proc)))
    (otherwise (error "expected return-void | return-error | call | call-void message, received %s" x))))


(defun-tco ss:handle-e-re-c-cv! (proc x)
  (case (car x)
    (exit nil)
    (return-error (ss:handle-error! proc (cl-second x)))
    (call (ss:handle-call! proc (cl-second x) (cl-third x))
          (ss:handle-e-re-c-cv! proc (ss:read proc)))
    (call-void (ss:handle-call-void! proc (cl-second x) (cl-third x))
               (ss:handle-e-re-c-cv! proc (ss:read proc)))
    (otherwise (error "expected exit | return-error | call | call-void message, received %s" x))))

(defun ss:send-application-error! (proc method err)
  (let ((error-text (format "{%s} client procedure application error:\nprocedure: %s\nerror: %s"
                            (process-name proc)
                            method
                            (error-message-string err))))
    (ss--log-error error-text)
    (ss:send-list! proc (list 'return-error error-text))
    (signal 'ss-exit (list error-text))))




(defun ss:handle-call! (proc method args)
  (condition-case err
      (ss:send-list! proc (list 'return (apply method args)))
    (error (ss:send-application-error! proc method err))))


(defun ss:handle-call-void! (proc method args)
  (condition-case err
      (progn (apply method args)
             (ss:send-list! proc '(return-void)))
    (error (ss:send-application-error! proc method err))))



(defvar ss:server-list nil)

(defadvice save-buffers-kill-terminal (before ss:proc-hook
                                           (&optional arg))
  "Deinitialize ss-rpc servers"
  (mapc #'ss:terminate! ss:server-list)
  (sleep-for 0.5))

(ad-activate #'save-buffers-kill-terminal)




(cl-defstruct ss:rpc-server
  name
  (connection 'uninitialized)
  (subproc 'uninitialized))



(defun ss:try-connect! (server start-time)
  (let ((subproc (ss:rpc-server-subproc server)))
    (cond
     ((> (- (float-time) start-time) ss:connection-timeout)
      (ss--log-error "server did not responded for timeout period")
      (cancel-timer (process-get subproc 'timer))
      (delete-process subproc))

     (t
      (let ((subproc-buffer (process-buffer subproc)))
        (when (> (buffer-size subproc-buffer) 0)
          (with-current-buffer subproc-buffer
            (ss--log-debug "received output from subproc: %S\ntry connect now" (buffer-string))
            (cl-letf (((point) 1))
              (let ((port (read subproc-buffer)))
                (ss--log-debug "port: %s" port)
                (setf (ss:rpc-server-connection server)
                      (ss:connect (ss:rpc-server-name server) port))
                (cancel-timer (process-get subproc 'timer))
                (ss--log-info "{%s} connection complete" (ss:rpc-server-name server)))))))))))




(defun ss:connect (name port)
  (make-network-process :name (format "%s-conn" name)
                        :service port
                        :buffer (generate-new-buffer (format " *ss [%s] connection*" name))
                        :host 'local))


(defun ss:start-server-from-port (name port)
  (make-ss:rpc-server :name name :connection (ss:connect name port)))


(defun ss:start-server (name command)
  "start server named NAME running the shell command COMMAND"
  (ss--log-info "{%s} initializing server..." name)
  (condition-case err
      (let* ((proc (start-process-shell-command (format "%s-subproc" name)
                                                (generate-new-buffer (format " *ss:%s subproc*" name))
                                                command))
             (server (make-ss:rpc-server :name name :subproc proc)))
        (ss--log-info "{%s} connecting to server...")
        (process-put proc 'timer (run-at-time nil 0.1 #'ss:try-connect! server (float-time)))
        (push server ss:server-list)
        server)

    (error (ss--log-fatal "%s %s" name (error-message-string err))
           (signal (car err) (cdr err)))))



(defun ss:send-client-error! (proc err)
  (let ((error-text (format "{%s} client ss error: %s"
                            (process-name proc)
                            (error-message-string err))))
    (ss--log-fatal error-text)
    (ss:send-list! proc (list 'return-error error-text))))


(defun ss:push-failed-call (proc err method args)
  (ss--log-error "{%s} CLIENT->SERVER CALL STACK: %s %s" (process-name proc) method args)
  (signal 'ss-exit (cdr err)))



(defun ss:call (server name &rest args)
  "call remote procedure
SERVER is the server struct returned by `ss:start-server'
NAME is the remote procedure symbol
ARGS are the arguments for passing to that procedure
return result of procedure"
  (let ((conn (ss:rpc-server-connection server)))
    (condition-case err
        (progn
          (ss:send-list! conn (list 'call name args))
          (ss:handle-r-re-c-cv conn (ss:read conn)))
      (error (ss:send-client-error! conn err)
             (ss:push-failed-call conn err name args))
      (ss-exit (ss:push-failed-call conn err name args)))))


(defun ss:call! (server name &rest args)
  "call remote procedure for side effects
SERVER is the server struct returned by `ss:start-server'
NAME is the remote procedure symbol
ARGS are the arguments for passing to that procedure
return nil"
  (let ((conn (ss:rpc-server-connection server)))
    (condition-case err
        (progn
          (ss:send-list! conn (list 'call-void name args))
          (ss:handle-rv-re-c-cv conn (ss:read conn)))
      (error (ss:send-client-error! conn err)
             (ss:push-failed-call conn err name args))
      (ss-exit (ss:push-failed-call conn err name args)))))


(defun ss:terminate! (server)
  "terminate ss-rpc server process SERVER (returned by `ss:start-server') with executing deinitialization hooks, if any"
  (let ((conn (ss:rpc-server-connection server)))
    (when (eq (process-status conn) 'open)
      (condition-case err
              (progn
                (ss:send-list! conn '(terminate))
                (ss:handle-e-re-c-cv! conn (ss:read conn)))
        (error (ss:send-client-error! conn err))))))



(provide 'ss-rpc-client)
