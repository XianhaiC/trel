; (ql:quickload :cl-charms)
; (in-package :cl-test)
;
; (defmacro once-only ((&rest names) &body body)
  ; (let ((gensyms (loop for n in names collect (gensym))))
    ; `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       ; `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ; ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ; ,@body)))))
;
; (defun start-color ()
  ; (when (eql (charms/ll:has-colors) charms/ll:FALSE)
    ; (error "Your terminal does not support color."))
  ; (let ((ret-code (charms/ll:start-color)))
    ; (if (= ret-code 0)
      ; T
      ; (error "start-color error ~s." ret-code))))
;
; (eval-when (:load-toplevel :compile-toplevel :execute)
  ; (defconstant +black+   charms/ll:COLOR_BLACK)
  ; (defconstant +red+     charms/ll:COLOR_RED)
  ; (defconstant +green+   charms/ll:COLOR_GREEN)
  ; (defconstant +yellow+  charms/ll:COLOR_YELLOW)
  ; (defconstant +blue+    charms/ll:COLOR_BLUE)
  ; (defconstant +magenta+ charms/ll:COLOR_MAGENTA)
  ; (defconstant +cyan+    charms/ll:COLOR_CYAN)
  ; (defconstant +white+   charms/ll:COLOR_WHITE))
;
; (defmacro define-color-pair ((name pair) foreground background)
  ; `(progn
     ; ;(start-color)
     ; (defparameter ,name (progn (charms/ll:init-pair ,pair ,foreground ,background)
                                ; (charms/ll:color-pair ,pair)))))
;
; (define-color-pair (+white/blue+ 1) +white+ +blue+)
; (define-color-pair (+black/red+ 2) +black+ +red+)
;
; (defun draw-window-background (window color-pair)
  ; (charms/ll:wbkgd (charms::window-pointer window) color-pair))
;
; (defmacro with-colors ((window color-pair) &body body)
  ; (let ((winptr (gensym)))
    ; (once-only (color-pair)
               ; `(let ((,winptr (charms::window-pointer ,window)))
                  ; (charms/ll:wattron ,winptr ,color-pair)
                  ; ,@body
                  ; (charms/ll:wattroff ,winptr ,color-pair)))))
;
; (defun pretty-hello-world ()
  ; (charms:with-curses ()
                      ; (charms:disable-echoing)
                      ; (charms:enable-raw-input)
                      ; ;(start-color)
                      ; (loop named hello-world
                            ; with window = (charms:make-window 50 15 10 10)
                            ; do (progn
                                 ; (charms:clear-window window)
                                 ; (draw-window-background window +white/blue+)
                                 ; (with-colors (window +white/blue+)
                                              ; (charms:write-string-at-point window "Hello world!" 0 0))
                                 ; (with-colors (window +black/red+)
                                              ; (charms:write-string-at-point window "Hello world!" 0 1))
                                 ; (charms:refresh-window window)
;
                                 ; ;; Process input
                                 ; (when (eql (charms:get-char window :ignore-error t) #\q)
                                   ; (return-from hello-world))
                                 ; (sleep 0.1)))))
;
; (defun main ()
  ; (pretty-hello-world))
;
; (main)


(ql:quickload :croatoan)

(in-package :croatoan)

(defun test1 ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
               (clear scr)
               (move scr 2 0)
               (format scr "Type chars. Type q to quit.~%~%")
               (refresh scr)
               (setf (color-pair scr) '(:yellow :red)
                     (attributes scr) '(:bold))
               (event-case (scr event)
                           (#\q (return-from event-case))
                           (otherwise (princ event scr)
                                      (refresh scr)))))
(test1)
