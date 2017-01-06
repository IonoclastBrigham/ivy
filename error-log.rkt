#lang racket/base
; error-log.rkt
(require racket/class
         racket/gui/base
         "files.rkt")
(provide err-port log-frame)

(define old-err (current-error-port))

(define err-port
  (make-output-port
   'error-port-custom
   always-evt
   (λ (s start end non-block? breakable?)
     (define str (bytes->string/utf-8 s #\?))
     (fprintf old-err str)
     (send log-text insert str)
     (- end start))
   void))

(define log-frame
  (new frame%
       [label "Ivy - Error Log"]
       [width 600]
       [height 500]))
(void (send log-frame set-icon (read-bitmap logo)))

(define log-text (new text% [auto-wrap #f]))

(define log-ecanvas
  (new editor-canvas%
       [parent log-frame]
       [label "Error Log"]
       [editor log-text]
       [style '(auto-vscroll auto-hscroll no-focus)]))

(define button-hpanel
  (new horizontal-panel%
       [parent log-frame]
       [alignment '(right center)]
       [stretchable-height #f]))

(define close-button
  (new button%
       [parent button-hpanel]
       [label "Close"]
       [callback (λ (button evt)
                   (send log-frame show #f))]))

(define copy-button
  (new button%
       [parent button-hpanel]
       [label "Copy to Clipboard"]
       [callback (λ (button evt)
                   ; select all text
                   (send log-text copy #f (send evt get-time-stamp) 0 'end))]))