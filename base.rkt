#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require file/sha1
         gif-image
         pict
         racket/bool
         racket/class
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/gui/base
         racket/list
         racket/path
         racket/string
         riff
         rsvg
         (only-in srfi/13
                  string-contains-ci
                  string-null?)
         "db.rkt"
         "embed.rkt"
         "files.rkt")
(provide (all-defined-out)
         string-null?
         gif?
         gif-animated?)

; max length of a frame's label string
(define +label-max+ 198)

(define (path->symbol p)
  (string->symbol (path->string p)))

(define (symbol->path p)
  (string->path (symbol->string p)))

(define macosx?
  (eq? (system-type) 'macosx))

(define +root-path+
  (if (eq? (system-type) 'windows)
      (build-path "C:\\")
      (build-path "/")))
; path of the currently displayed image
(define image-path (make-parameter +root-path+))
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; the cached XMP metadata of the image
(define image-xmp (box empty))
(define xmp-threads (make-hash))
; directory containing the currently displayed image
(define image-dir (make-parameter (find-system-path 'home-dir)))
; the only extensions ivy will accept - ignores everything else
; shenanigans be here
(define +supported-downcase+ '(".bmp"
                               ".flaf"
                               ".flf"
                               ".flif"
                               ".gif"
                               ".jpe"
                               ".jpeg"
                               ".jpg"
                               ".png"
                               ".svg"
                               ".xbm"
                               ".xpm"))
(define +supported-extensions+ (append +supported-downcase+
                                       (map string-upcase +supported-downcase+)))
; gif/flif stuff
; listof pict?
(define image-lst-master empty)
(define image-lst empty)
; listof real?
(define image-lst-timings empty)
; number of times to loop a FLIF (0 = forever)
(define image-num-loops 0)
(define animation-thread (make-parameter #f))
; GIF cumulative animation
(define cumulative? (make-parameter #f))
(define exact-search? (make-parameter #f))

(define color-white (make-object color% "white"))
(define color-black (make-object color% "black"))
(define color-spring-green (make-object color% "spring green"))
(define color-gold (make-object color% "gold"))
(define color-red (make-object color% "red"))

; a supported file must have an extension
(define/contract (supported-file? img)
  (path-string? . -> . boolean?)
  (define ext (path-get-extension img))
  (and ext
       (member (bytes->string/utf-8 ext) +supported-extensions+)
       #t))

; find all supported images in dir,
; searched recursively
(define (dir-files [dir (image-dir)])
  (find-files supported-file? dir))

; all image files contained within dir,
; but not recursively searched
(define (path-files dir)
  (define dir-lst (directory-list dir #:build? #t))
  (filter supported-file? dir-lst))

; parameter listof path
; if pfs is empty, attempting to append a single image would
; make pfs just that image, rather than a list of length 1
(define pfs (make-parameter (list +root-path+)))

(define (string->taglist str)
  (cond [(string-null? str) empty]
        [else
         (define tags
           (filter
            (negate string-null?)
            (map string-trim
                 (string-split str ","))))
         (remove-duplicates (sort tags string<?))]))

(define/contract (tfield->list tf)
  ((is-a?/c text-field%). -> . list?)
  (define val (send tf get-value))
  (string->taglist val))

; get index of an item in the list
; numbering starts from 0
;
; may replace with index-of from Racket v6.7
(define/contract (get-index item lst)
  (any/c list? . -> . (or/c integer? false?))
  (define len (length lst))
  (define pos (member item lst))
  (if pos (- len (length pos)) #f))

(define (string-truncate str n)
  (if (<= (string-length str) n)
      str
      (substring str 0 n)))

; awww yeah... so oldskool...
(define (remove-children parent kids)
  (when (> (length kids) 0)
    (send parent delete-child (car kids))
    (remove-children parent (cdr kids))))

; just check out those tail recursions...
(define (add-children parent kids)
  (when (> (length kids) 0)
    (send parent add-child (car kids))
    (add-children parent (cdr kids))))

;; COPIED FROM opengl/main
;; Convert argb -> rgba
;; Modern wisdom is not to convert to rgba but rather use
;; GL_BGRA with GL_UNSIGNED_INT_8_8_8_8_REV. But that turns out not
;; to work on some implementations, even ones which advertise
;; OpenGL 1.2 support. Great.
(define (argb->rgba! pixels)
  (for ([i (in-range (/ (bytes-length pixels) 4))])
    (let* ([offset (* 4 i)]
           [alpha (bytes-ref pixels (+ 0 offset))]
           [red   (bytes-ref pixels (+ 1 offset))]
           [green (bytes-ref pixels (+ 2 offset))]
           [blue  (bytes-ref pixels (+ 3 offset))])
      (bytes-set! pixels (+ 0 offset) red)
      (bytes-set! pixels (+ 1 offset) green)
      (bytes-set! pixels (+ 2 offset) blue)
      (bytes-set! pixels (+ 3 offset) alpha))))
;; </COPIED>

(define (rgba->argb! pixels)
  (for ([i (in-range (/ (bytes-length pixels) 4))])
    (let* ([offset (* 4 i)]
           [red   (bytes-ref pixels (+ 0 offset))]
           [green (bytes-ref pixels (+ 1 offset))]
           [blue  (bytes-ref pixels (+ 2 offset))]
           [alpha (bytes-ref pixels (+ 3 offset))])
      (bytes-set! pixels (+ 0 offset) alpha)
      (bytes-set! pixels (+ 1 offset) red)
      (bytes-set! pixels (+ 2 offset) green)
      (bytes-set! pixels (+ 3 offset) blue))))

(define/contract (flif->list image #:animation? [animation? (want-animation?)])
  (->* ([or/c path-string? bytes?])
       (#:animation? boolean?)
       (listof (is-a?/c bitmap%)))
  ; create the decoder pointer
  (define dec-ptr (flif-create-decoder))
  ; decode either the file from the path or by its bytes
  (if (path-string? image)
      (flif-decoder-decode-file! dec-ptr image)
      (flif-decoder-decode-memory! dec-ptr image))
  ; only look at the first frame if we want a static image
  (define num (if animation? (flif-decoder-num-images dec-ptr) 1))
  (define lst
    (for/list ([i (in-range num)])
      (define img-ptr (flif-decoder-get-image dec-ptr i))
      (define width (flif-image-get-width img-ptr))
      (define height (flif-image-get-height img-ptr))
      ; make sure to decode with the proper depth
      (define reader (if (= (flif-image-get-depth img-ptr) 8)
                         flif-image-read-rgba8
                         flif-image-read-rgba16))
      (define pixels (reader img-ptr width height))
      (rgba->argb! pixels)
      (define bitmap (make-object bitmap% width height #f #t))
      (send bitmap set-argb-pixels 0 0 width height pixels)
      bitmap))
  (flif-destroy-decoder! dec-ptr)
  lst)

; objects that will be used extensively in transparency-grid
(define dgray-color (make-object color% 128 128 128))
(define lgray-color (make-object color% 204 204 204))
(define dgray-square (filled-rectangle 10 10 #:color dgray-color #:draw-border? #f))
(define lgray-square (filled-rectangle 10 10 #:color lgray-color #:draw-border? #f))

; creates a dark-gray/light-gray grid to place behind images
; so that if they are transparent, the grid will become visible.
(define/contract (transparency-grid img)
  ((or/c (is-a?/c bitmap%) pict?) . -> . pict?)
  (define x (if (pict? img) (pict-width img) (send img get-width)))
  (define y (if (pict? img) (pict-height img) (send img get-height)))
  ; 10 for a square's width
  (define x-times (inexact->exact (floor (/ x 10))))
  ; 20 for both square's height
  (define y-times (inexact->exact (floor (/ y 20))))
  (define remainder-x (modulo (floor x) 10))
  (define remainder-y (modulo (floor y) 20))
  ; remainder square for width
  (define rdsquare-x
    (if (> remainder-x 0)
        (filled-rectangle remainder-x 10 #:color dgray-color #:draw-border? #f)
        #f))
  (define rlsquare-x
    (if (> remainder-x 0)
        (filled-rectangle remainder-x 10 #:color lgray-color #:draw-border? #f)
        #f))
  ; remainder square for height
  (define rdsquare-y
    (if (> remainder-y 0)
        (filled-rectangle 10 (if (> remainder-y 10)
                                 (- remainder-y 10)
                                 remainder-y) #:color dgray-color #:draw-border? #f)
        #f))
  (define rlsquare-y
    (if (> remainder-y 0)
        (filled-rectangle 10 (if (> remainder-y 10)
                                 (- remainder-y 10)
                                 remainder-y) #:color lgray-color #:draw-border? #f)
        #f))
  ; remainder square for both width and height
  (define rdsquare-xy
    (cond
      [(> remainder-x 0)
       (if (> remainder-y 0)
           (if (> remainder-y 10)
               (filled-rectangle remainder-x
                                 (- remainder-y 10)
                                 #:color dgray-color
                                 #:draw-border? #f)
               (filled-rectangle remainder-x
                                 remainder-y
                                 #:color dgray-color
                                 #:draw-border? #f))
           #f)]
      [else #f]))
  (define rlsquare-xy
    (cond
      [(> remainder-x 0)
       (if (> remainder-y 0)
           (filled-rectangle remainder-x (if (> remainder-y 10)
                                             (- remainder-y 10)
                                             remainder-y) #:color lgray-color #:draw-border? #f)
           #f)]
      [else #f]))
  ; normal dgray-lgray line
  (define aline
    (let ([aline
           (for/list ([times (in-range x-times)]
                      [i (in-naturals)])
             (if (even? i)
                 dgray-square
                 lgray-square))])
      (if (> remainder-x 0)
          (if (even? x-times)
              (apply hc-append (flatten (append aline (if rdsquare-x rdsquare-x empty))))
              (apply hc-append (flatten (append aline (if rlsquare-x rlsquare-x empty)))))
          (apply hc-append aline))))
  ; normal lgray-dgray line
  (define bline
    (let ([bline
           (for/list ([times (in-range x-times)]
                      [i (in-naturals)])
             (if (even? i)
                 lgray-square
                 dgray-square))])
      (if (> remainder-x 0)
          (if (even? x-times)
              (apply hc-append (flatten (append bline (if rlsquare-x rlsquare-x empty))))
              (apply hc-append (flatten (append bline (if rdsquare-x rdsquare-x empty)))))
          (apply hc-append bline))))
  ; remainder-sized line for both x and y
  ; if there is no remainder for x or y, don't add them
  (define raline
    (let ([aline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rdsquare-y rdsquare-y empty)
                  (if rlsquare-y rlsquare-y empty))))]
          [bline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rlsquare-y rlsquare-y empty)
                  (if rdsquare-y rdsquare-y empty))))])
      (if (even? x-times)
          (apply hc-append (flatten (append aline (if rlsquare-xy rlsquare-xy empty))))
          (apply hc-append (flatten (append bline (if rdsquare-xy rdsquare-xy empty)))))))
  (define rbline
    (let ([aline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rdsquare-y rdsquare-y empty)
                  (if rlsquare-y rlsquare-y empty))))]
          [bline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rlsquare-y rlsquare-y empty)
                  (if rdsquare-y rdsquare-y empty))))])
      (if (even? x-times)
          (apply hc-append (flatten (append bline (if rlsquare-xy rlsquare-xy empty))))
          (apply hc-append (flatten (append aline (if rdsquare-xy rdsquare-xy empty)))))))
  ; put it all together
  (let ([base-grid (make-list y-times (vl-append aline bline))])
    (cond [(> remainder-x 0)
           (if (even? x-times)
               (apply vl-append (flatten (append base-grid (if (> remainder-y 0)
                                                               (if (> remainder-y 10)
                                                                   (list aline rbline)
                                                                   raline)
                                                               rbline))))
               (apply vl-append (flatten (append base-grid (if (> remainder-y 0)
                                                               (if (> remainder-y 10)
                                                                   (list aline raline)
                                                                   rbline)
                                                               raline)))))]
          [(> remainder-y 0)
           (if (> remainder-y 10)
               (apply vl-append
                      (append base-grid (list aline (if (even? x-times) rbline raline))))
               (apply vl-append
                      (append base-grid (list raline))))]
          [else (apply vl-append base-grid)])))

(define/contract (transparency-grid-append img)
  ((or/c (is-a?/c bitmap%) pict?) . -> . (is-a?/c bitmap%))
  (define x (if (pict? img) (pict-width img) (send img get-width)))
  (define pct (if (pict? img) img (bitmap img)))
  (define grid (transparency-grid img))
  ; generated grid size is imperfect
  (define offset (- x (pict-width grid)))
  (if (= offset 0)
      (pict->bitmap (hc-append (- x) grid pct))
      (pict->bitmap (hc-append (- offset x) grid pct))))

; janky!
(define ivy-canvas (make-parameter #f))
(define ivy-tag-tfield (make-parameter #f))
(define ivy-actions-rating (make-parameter #f))
(define status-bar-dimensions (make-parameter #f))
(define status-bar-error (make-parameter #f))
(define status-bar-position (make-parameter #f))
(define status-bar-size (make-parameter #f))
(define incoming-tags (make-parameter ""))
(define want-animation? (make-parameter #f))

(define/contract (animation-callback canvas dc lst)
  ((is-a?/c canvas%) (is-a?/c dc<%>) (listof (is-a?/c bitmap%)) . -> . void?)
  (define img-x (inexact->exact (round (send (first lst) get-width))))
  (define img-y (inexact->exact (round (send (first lst) get-height))))
  (define img-center-x (/ img-x 2))
  (define img-center-y (/ img-y 2))
  
  (define canvas-x (send canvas get-width))
  (define canvas-y (send canvas get-height))
  (define canvas-center-x (/ canvas-x 2))
  (define canvas-center-y (/ canvas-y 2))
  
  (define len (length lst))

  ; the left and top offsets for each frame in the gif,
  ; just in case the frames are of varying sizes
  (define left/top
    (cond
      [(gif? (image-path))
       (for/list ([bit-frame (gif-images (image-path))]
                  [master-frame (in-list image-lst-master)]
                  [gif-frame (in-list image-lst)])
         (define master-width (send master-frame get-width))
         (define master-height (send master-frame get-height))
         (define gif-width (send gif-frame get-width))
         (define gif-height (send gif-frame get-height))
         ; grab the frame's image descriptor
         ; starting with the graphics control extension
         (define matched (car (regexp-match-positions (byte-regexp (bytes #x21 #xf9)) bit-frame)))
         (define lengths (subbytes bit-frame (+ (car matched) 9) (+ (car matched) 13)))
         (define left (bytes (bytes-ref lengths 1) (bytes-ref lengths 0)))
         (define top (bytes (bytes-ref lengths 3) (bytes-ref lengths 2)))
         (list (* (string->number (bytes->hex-string left) 16) (/ gif-width master-width))
               (* (string->number (bytes->hex-string top) 16) (/ gif-height master-height))))]
      [else (make-list len (list 0 0))]))

  ; set the center location
  (define x-loc (- img-center-x))
  (define y-loc (- img-center-y))

  ; modify the scrollbars outside the animation loop
  (define hscroll (> img-x canvas-x))
  (define vscroll (> img-y canvas-y))
  (send canvas show-scrollbars hscroll vscroll)

  ; actual animation loop
  ; runs until animation-thread is killed
  (let loop ([img-frame (first lst)]
             [timing
              (if (empty? image-lst-timings)
                  1/20
                  (first image-lst-timings))]
             [offsets (first left/top)]
             [i 0]
             [times 0])
    ; remove any previous frames from the canvas
    (unless (cumulative?) (send dc clear))
    (send canvas recenter)
    (send dc draw-bitmap img-frame (+ x-loc (first offsets)) (+ y-loc (second offsets)))
    (sleep timing)
    (cond
      ; loop forever
      ; ran through every frame
      [(and (>= i len) (= image-num-loops 0))
       (loop (first lst) (if (empty? image-lst-timings)
                             1/20
                             (first image-lst-timings))
             (first left/top) 0 0)]
      ; loop forever
      ; still need to display the other frames
      [(and (not (>= i len)) (= image-num-loops 0))
       (loop (list-ref lst i)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (list-ref image-lst-timings i))
             (list-ref left/top i)
             (add1 i)
             0)]
      ; stop looping eventually
      ; ran through every frame, increment times
      [(and (>= i len) (not (= image-num-loops 0)))
       (loop (first lst)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (first image-lst-timings))
             (first left/top)
             0
             (add1 times))]
      ; stop looping eventually
      ; still need to display the other frames
      [(and (not (>= i len))
            (not (= image-num-loops 0)))
       (loop (list-ref lst i)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (list-ref image-lst-timings i))
             (list-ref left/top i)
             (add1 i)
             times)]
      ; reached the end of our allowed loops, do nothing
      [(and (>= i len) (not (= image-num-loops 0))) #t]
      [else #t])))

; procedure that loads the given image to the canvas
; takes care of updating the dimensions message and
; the position message
(define/contract (load-image img)
  (->* ([or/c path? (is-a?/c bitmap%) (listof (is-a?/c bitmap%))])
       void?)
  (define canvas (ivy-canvas))
  (define dc (send canvas get-dc))
  (define ivy-frame (send canvas get-parent))
  (define tag-tfield (ivy-tag-tfield))
  (define iar (ivy-actions-rating))
  (define sbd (status-bar-dimensions))
  (define sbe (status-bar-error))
  (define sbp (status-bar-position))

  (send dc set-scale 1.0 1.0)
  (send sbe set-label "")
  
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (image-dir base)
     (image-path img)
     (define img-str (path->string img))
     (cond
       ; load an animated gif
       [(and (want-animation?) (gif? img) (gif-animated? img))
        (define load-success (send image-bmp-master load-file img))
        ; set the new frame label
        (send ivy-frame set-label (string-truncate (path->string name) +label-max+))
        ; make a list of bitmaps
        (with-handlers
            ([exn:fail? (λ (e)
                          (eprintf "Error loading animated gif ~v: ~a\n"
                                   (path->string name)
                                   (exn-message e))
                          ; set the gifs to defaults
                          (set! image-lst-master empty)
                          (set! image-lst empty)
                          (set! image-lst-timings empty)
                          ; just load the static image instead
                          (load-image img)
                          (send sbe set-label
                                (format "Error loading file ~v"
                                        (string-truncate (path->string name) 30))))])
          (cumulative? (gif-cumulative? img))
          (define lst
            (for/list ([bits (gif-images img)])
              (define bmp-in-port (open-input-bytes bits))
              (define bmp (make-object bitmap% 50 50))
              (send bmp load-file bmp-in-port 'gif/alpha)
              (close-input-port bmp-in-port)
              bmp))
          (set! image-lst-master lst)
          (set! image-lst lst)
          (set! image-lst-timings (gif-timings img)))
        (define size (file-size (image-path)))
        (send sbd set-label
              (format "~a x ~a pixels  ~a"
                      (send image-bmp-master get-width)
                      (send image-bmp-master get-height)
                      (cond [(>= size (expt 2 20))
                             (format "~a MiB"
                                     (~r (exact->inexact (/ size (expt 2 20)))
                                         #:precision 1))]
                            [(>= size (expt 2 10))
                             (format "~a KiB"
                                     (~r (exact->inexact (/ size (expt 2 10)))
                                         #:precision 1))]
                            [else
                             (format "~a B" size)])))
        (send sbp set-label
              (format "~a / ~a"
                      (+ (get-index img (pfs)) 1)
                      (length (pfs))))]
       ; load animated flif
       [(and (want-animation?) (flif? img) (flif-animated? img))
        (cumulative? #f)
        (define dec-ptr (flif-create-decoder))
        ; regular decoding
        (flif-decoder-decode-file! dec-ptr img)
        (let ([img-ptr (flif-decoder-get-image dec-ptr 0)]
              [num-frames (flif-decoder-num-images dec-ptr)])
          (define timing-delay (flif-image-get-frame-delay img-ptr))
          (define lst (flif->list img))
          (set! image-bmp-master (first lst))
          (set! image-lst-master lst)
          (set! image-lst lst)
          (set! image-lst-timings (make-list num-frames (/ timing-delay 1000)))
          (set! image-num-loops (flif-decoder-num-loops dec-ptr))
          (flif-destroy-decoder! dec-ptr))
        ; set the new frame label
        (send ivy-frame set-label (string-truncate (path->string name) +label-max+))
        ; set the gui information
        (define size (file-size (image-path)))
        (define dimensions (flif-dimensions (image-path)))
        (send sbd set-label
              (format "~a x ~a pixels  ~a"
                      (first dimensions)
                      (second dimensions)
                      (cond [(>= size (expt 2 20))
                             (format "~a MiB"
                                     (~r (exact->inexact (/ size (expt 2 20)))
                                         #:precision 1))]
                            [(>= size (expt 2 10))
                             (format "~a KiB"
                                     (~r (exact->inexact (/ size (expt 2 10)))
                                         #:precision 1))]
                            [else
                             (format "~a B" size)])))
        (send sbp set-label
              (format "~a / ~a"
                      (+ (get-index img (pfs)) 1)
                      (length (pfs))))]
       ; else load the static image
       [else
        ; make sure the bitmap loaded correctly
        (define load-success
          (cond [(bytes=? (path-get-extension img) #".svg")
                 (and (set! image-bmp-master (load-svg-from-file img)) #t)]
                [(flif? img)
                 (cumulative? #f)
                 (define lst (flif->list img))
                 (set! image-bmp-master (first lst))
                 (load-image (first lst))]
                [else (send image-bmp-master load-file img 'unknown/alpha)]))
        (cond [load-success
               (send ivy-frame set-label (string-truncate (path->string name) +label-max+))
               (define size (file-size (image-path)))
               (cond
                 [(flif? (image-path))
                  (define dimensions (flif-dimensions (image-path)))
                  (send sbd set-label
                        (format "~a x ~a pixels  ~a"
                                (first dimensions)
                                (second dimensions)
                                (cond [(>= size (expt 2 20))
                                       (format "~a MiB"
                                               (~r (exact->inexact (/ size (expt 2 20)))
                                                   #:precision 1))]
                                      [(>= size (expt 2 10))
                                       (format "~a KiB"
                                               (~r (exact->inexact (/ size (expt 2 10)))
                                                   #:precision 1))]
                                      [else
                                       (format "~a B" size)])))]
                 [else
                  (send sbd set-label
                        (format "~a x ~a pixels  ~a"
                                (send image-bmp-master get-width)
                                (send image-bmp-master get-height)
                                (cond [(>= size (expt 2 20))
                                       (format "~a MiB"
                                               (~r (exact->inexact (/ size (expt 2 20)))
                                                   #:precision 1))]
                                      [(>= size (expt 2 10))
                                       (format "~a KiB"
                                               (~r (exact->inexact (/ size (expt 2 10)))
                                                   #:precision 1))]
                                      [else
                                       (format "~a B" size)])))])
               (send sbp set-label
                     (format "~a / ~a"
                             (+ (get-index img (pfs)) 1)
                             (length (pfs))))
               (set! image-lst empty)
               (set! image-lst-timings empty)]
              [else
               (eprintf "Error loading file ~v\n" img)
               (send sbe set-label
                     (format "Error loading file ~v"
                             (string-truncate (path->string name) 30)))])])
     
     ; pick what string to display for tags...
     (define-values [db-tags-lst db-tags]
       (cond [(db-has-key? 'images img-str)
              (define img-obj (make-data-object sqlc image% img-str))
              (define tags (send img-obj get-tags))
              (values tags (string-join tags ", "))]
             [else (values '() "")]))
     ; check to see if the image has embedded tags
     (define-values [embed-tags-lst embed-tags]
       (cond [(embed-support? img-str)
              (set-box! image-xmp (get-embed-xmp img-str))
              (define embed-lst (sort (get-embed-tags img-str) string<?))
              (if (empty? embed-lst)
                  (values '() "")
                  ; the embedded tags may come back unsorted
                  (values embed-lst (string-join embed-lst ", ")))]
             [else
              (set-box! image-xmp empty)
              (values '() "")]))
     ; now verify the tags match, and merge them/let the user know if necessary
     (incoming-tags
      (cond [(string=? db-tags embed-tags) db-tags]
            [else
             (send (ivy-tag-tfield) set-field-background color-red)
             (define mismatched
               (remove-duplicates (sort (append db-tags-lst embed-tags-lst) string<?)))
             (string-join mismatched ", ")]))

     ; load the image's rating, if any
     (define db-rating
       (if (db-has-key? 'ratings img-str)
           (let ([rating-obj (make-data-object sqlc rating% img-str)])
             (number->string (send rating-obj get-rating)))
           "0"))
     ; set the label of ivy-actions-rating to the rating of the
     ; image (if applicable)
     (define embed-rating
       (if (and (embed-support? img-str)
                (not (empty? (unbox image-xmp))))
           (xmp-rating (first (unbox image-xmp)))
           "0"))
     ; set the rating widget
     (define incoming-rating
       (if (string=? "0" embed-rating)
           db-rating
           embed-rating))
     (send iar set-string-selection (string-append incoming-rating " 🌟"))

     ; ...put them in the tfield
     (send tag-tfield set-value (incoming-tags))
     ; ensure the text-field displays the changes we just made
     (send tag-tfield refresh)]
    [(list? img)
     ; this is actually a list of frames
     (set! image-lst img)]
    [else
     ; we already have the image loaded
     (set! image-lst-master empty)
     (set! image-lst empty)
     (set! image-lst-timings empty)])
  
  (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
    (kill-thread (animation-thread)))

  (send canvas center-fit)

  (cond [(not (empty? image-lst))
         ; image-lst contains a list of picts, display the animation
         (define lst (map transparency-grid-append image-lst))
         
         (send canvas set-on-paint!
               (λ (canvas dc)
                 (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
                   (kill-thread (animation-thread)))
              
                 (send dc set-background "black")
              
                 (animation-thread
                  (thread
                   (λ ()
                     (animation-callback canvas dc lst))))))]
        [else
         ; image-lst contains a list of picts, display the animation
         (define bmp (transparency-grid-append (if (path? img) image-bmp-master img)))
         (define img-width (inexact->exact (round (send bmp get-width))))
         (define img-height (inexact->exact (round (send bmp get-height))))
         (define img-center-x (/ img-width 2))
         (define img-center-y (/ img-height 2))
         (define-values [canvas-width canvas-height]
           (send canvas get-virtual-size))
         (define canvas-center-x (/ canvas-width 2))
         (define canvas-center-y (/ canvas-height 2))
         
         (send canvas set-on-paint!
               (λ (canvas dc)
                 ; center the image on the canvas
                 (send canvas recenter)
                 (send dc draw-bitmap bmp (- img-center-x) (- img-center-y))))]))

; curried procedure to abstract loading an image in a collection
; mmm... curry (see https://www.imdb.com/name/nm0000347/?ref_=fn_al_nm_1)
(define ((load-image-in-collection direction))
  (unless (equal? (image-path) +root-path+)
    ; kill the animation thread, if applicable
    (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
      (kill-thread (animation-thread)))
    (send (ivy-tag-tfield) set-default-background)
    (define prev-index (get-index (image-path) (pfs)))
    (case direction
      [(previous)
       (when (and prev-index (> (length (pfs)) 1))
         (cond [(zero? prev-index)
                (define img (last (pfs))) ; this is a path
                (load-image img)]
               [else
                (define img (list-ref (pfs) (- prev-index 1)))
                (load-image img)]))]
      [(next)
       (when (and prev-index (> (length (pfs)) 1))
         (cond [(= prev-index (- (length (pfs)) 1))
                (define img (first (pfs))) ; this is a path
                (load-image img)]
               [else
                (define img (list-ref (pfs) (+ prev-index 1)))
                (load-image img)]))]
      [(rand) (load-image (list-ref (pfs) (- (random 1 (+ (length (pfs)) 1)) 1)))]
      [(home) (load-image (first (pfs)))]
      [(end) (load-image (last (pfs)))])))

; is this complicating things? I have no idea, but we should never
; underestimate the `cool' factor
(define load-previous-image (load-image-in-collection 'previous))
(define load-next-image (load-image-in-collection 'next))
(define load-rand-image (load-image-in-collection 'rand))
(define load-first-image (load-image-in-collection 'home))
(define load-last-image (load-image-in-collection 'end))

; takes a list lst and a width
; returns a list of lists of lengths no more than width
(define (grid-list lst width [accum empty])
  (if (> width (length lst))
      (filter (negate empty?) (append accum (list lst)))
      (grid-list (drop lst width) width (append accum (list (take lst width))))))

; implement common keyboard shortcuts
(define (init-editor-keymap km)
  
  (send km add-function "insert-clipboard"
        (lambda (editor kev)
          (send editor paste)))
  
  (send km add-function "insert-primary"
        (lambda (editor kev)
          (send editor paste-x-selection)))
  
  (send km add-function "cut"
        (lambda (editor kev)
          (send editor cut)))
  
  (send km add-function "copy"
        (lambda (editor kev)
          (send editor copy)))
  
  (send km add-function "select-all"
        (lambda (editor kev)
          (send editor move-position 'end)
          (send editor extend-position 0)))
  
  (send km add-function "delete-backward-char"
        (lambda (editor kev)
          (send editor delete)))
  
  (send km add-function "delete-forward-char"
        (lambda (editor kev)
          (define start (send editor get-start-position))
          (define end (send editor get-end-position))
          ; if we have something selected, only delete that part
          (send editor delete start (if (= start end)
                                        (+ end 1)
                                        end))))
  
  (send km add-function "backward-char"
        (lambda (editor kev)
          (send editor move-position 'left)))
  
  (send km add-function "backward-word"
        (lambda (editor kev)
          (send editor move-position 'left #f 'word)))
  
  (send km add-function "backward-kill-word"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'left #f 'word)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-word"
        (lambda (editor kev)
          (define from (send editor get-start-position))
          (send editor move-position 'right #f 'word)
          (define to (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-buffer"
        (lambda (editor kev)
          (send editor kill)))
  
  (send km add-function "backward-kill-buffer"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'home)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "mark-char-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'simple))))
  
  (send km add-function "mark-char"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'simple))))
  
  (send km add-function "mark-word-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'word))))
  
  (send km add-function "mark-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "mark-whole-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #f 'word)
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "forward-char"
        (lambda (editor kev)
          (send editor move-position 'right)))
  
  (send km add-function "forward-word"
        (lambda (editor kev)
          (send editor move-position 'right #f 'word)))
  
  (send km add-function "beginning-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'home)))
  
  (send km add-function "end-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'end)))
  
  ; from gui-lib/mred/private/editor.rkt
  (send km add-function "mouse-popup-menu"
        (lambda (edit event)
          (when (send event button-up?)
            (let ([a (send edit get-admin)])
              (when a
                (let ([m (make-object popup-menu%)])
                  (append-editor-operation-menu-items m)
                  ;; Remove shortcut indicators (because they might not be correct)
                  (for-each
                   (lambda (i)
                     (when (is-a? i selectable-menu-item<%>)
                       (send i set-shortcut #f)))
                   (send m get-items))
                  (let-values ([(x y) (send edit
                                            dc-location-to-editor-location
                                            (send event get-x)
                                            (send event get-y))])
                    (send a popup-menu m (+ x 5) (+ y 5)))))))))
  km)

(define (set-default-editor-bindings km)
  (cond [macosx?
         (send km map-function ":d:c" "copy")
         (send km map-function ":d:с" "copy") ;; russian cyrillic
         (send km map-function ":d:v" "insert-clipboard")
         (send km map-function ":d:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":d:x" "cut")
         (send km map-function ":d:ч" "cut") ;; russian cyrillic
         (send km map-function ":d:a" "select-all")
         (send km map-function ":d:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":d:backspace" "backward-kill-buffer")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":a:left" "backward-word")
         (send km map-function ":a:right" "forward-word")
         (send km map-function ":a:backspace" "backward-kill-word")
         (send km map-function ":a:delete" "forward-kill-word")
         (send km map-function ":c:k" "forward-kill-buffer")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":a:s:left" "mark-word-backward")
         (send km map-function ":a:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":d:left" "beginning-of-buffer")
         (send km map-function ":c:a" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":d:right" "end-of-buffer")
         (send km map-function ":c:e" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":leftbuttondouble" "mark-whole-word")]
        [else
         (send km map-function ":c:c" "copy")
         (send km map-function ":c:с" "copy") ;; russian cyrillic
         (send km map-function ":c:v" "insert-clipboard")
         (send km map-function ":c:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":c:x" "cut")
         (send km map-function ":c:ч" "cut") ;; russian cyrillic
         (send km map-function ":c:a" "select-all")
         (send km map-function ":c:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":c:left" "backward-word")
         (send km map-function ":c:right" "forward-word")
         (send km map-function ":c:backspace" "backward-kill-word")
         (send km map-function ":c:delete" "forward-kill-word")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":c:s:left" "mark-word-backward")
         (send km map-function ":c:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":middlebutton" "insert-primary")
         (send km map-function ":leftbuttondouble" "mark-whole-word")
         (send km map-function ":s:insert" "insert-primary")]))

(current-text-keymap-initializer
 (λ (keymap)
   (define km (init-editor-keymap keymap))
   (set-default-editor-bindings km)))
