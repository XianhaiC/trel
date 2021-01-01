(ql:quickload :uiop)
(ql:quickload :croatoan)
(ql:quickload :vom)

(vom:config t :debug)

(with-open-file (stream "log.txt" :direction :output :if-exists :supersede)
  (defparameter vom:*log-stream* stream)
  (croatoan:with-screen (scr :input-echoing nil :input-blocking nil :enable-colors t)
    (croatoan:add-string scr "Press 'enter' to open vim"
                         :position '(0 0))
    (croatoan:refresh scr)
    (loop
      (let ((event (croatoan:get-event scr)))

        (croatoan:add-string scr "Press 'enter' to open vim"
                             :position '(0 0))
        (croatoan:refresh scr)
        (if event
            (case event
              (#\Newline (progn
                           (vom:debug "Entering vim")
                           (ncurses:endwin)
                           (uiop:wait-process (uiop:launch-program (list "vim" "test.txt") :output :interactive :error-output :interactive :input :interactive))
                           (vom:debug "Exiting vim")
                           (ncurses:initscr)))
              (#\q (progn
                     (vom:debug "Exiting program")
                     (return))))
            (progn
              (vom:debug "Sleeping")
              (sleep .0166)))))))
