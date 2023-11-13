(in-package :lem)

(lem/line-numbers:toggle-line-numbers)
(setf *scroll-recenter-p* nil)

(define-command suspend-editor () ()
  (charms/ll:endwin)
  (sb-posix:kill    (sb-posix:getpid) sb-unix:sigtstp))
(define-key *global-keymap* "C-z" 'suspend-editor)

;; Lisp mode
(lem-lisp-mode:toggle-paren-coloring)
