(import * from gui)
(import * from layout)

(setf *font* "\"Droid Sans Mono\",\"Courier New\",\"Courier\",monospace")
(setf *fontsz* 18)
(setf *line* 20)

(defun font (ctx opts)
  (let ((font ""))
    (when opts.bold
      (incf font " bold"))
    (when opts.italic
      (incf font " italic"))
    (setf ctx.fillStyle (or opts.color "#000000"))
    (setf ctx.font ~"{(slice font 1)} {*fontsz*}px {*font*}")))

(defobject line
    (text
     start-signature
     end-signature
     (start-context #())
     (end-context #())
     (sections (list))))

(defobject section (from to style))

(defun signature (d)
  (+ "{"
     (join (map (lambda (k)
                  ~"{k}:{(json (aref d k))}")
                (sort (keys d)))
           ",")
     "}"))

(defun draw-line (text sections h w
                  ctx x y tx)
  (declare (ignorable h))
  (let ((xx 0))
    (dolist (s sections)
      (when (> s.from xx)
        (let ((part (slice text xx s.from)))
          (font ctx #())
          (ctx.fillText part (+ tx x) y)
          (incf x (ctx.measureText part).width)
          (setf xx s.from)))
      (when (< (+ tx x) w)
        (let ((part (slice text s.from s.to)))
          (font ctx s.style)
          (let ((pw (ctx.measureText part).width))
            (setf ctx.fillStyle (or s.style.color "#000000"))
            (ctx.fillText part (+ tx x) y)
            (when s.style.underline
              (setf ctx.fillStyle s.style.underline)
              (ctx.fillRect (+ tx x) (+ y (1- h)) pw 1))
            (incf x pw)
            (setf xx s.to)))))
    (when (< (+ tx x) w)
      (when (> (length text) xx)
        (let ((part (slice text xx)))
          (font ctx #())
          (ctx.fillText part (+ tx x) y)
          (incf x (ctx.measureText part).width))))))

(defun ifind (text lines row col)
  (let ((lt (if (= text (lowercase text)) #'lowercase (lambda (x) x))))
    (dolist (r (range row (length lines)))
      (let ((i (index text (funcall lt (slice (aref lines r).text col)))))
        (when (>= i 0)
          (return-from ifind (list r (+ i col)))))
      (setf col 0))))

(defvar nullmode
  #((parmatch (lambda (lines row col)
                (declare (ignorable lines row col))
                null))
    (compute-end-context (lambda (line)
                           (declare (ignorable line))
                           #()))
    (autoindent (lambda (lines row)
                  (declare (ignorable lines row))
                  0))))

(defun editor (name content &optional (mode nullmode))
  (macrolet ((mutate (redo undo)
               `(progn
                  (when (< (length undo) modified-level)
                    (setf modified-level -1))
                  (push (list (lambda () ,undo) (lambda () ,redo)) undo)
                  (setf redo (list))
                  (setf lastins undefined)
                  (funcall (second (last undo))))))
    (let** ((frame (set-style (create-element "div")
                              position "absolute"))
            (status (set-style (create-element "div")
                               whiteSpace "pre"
                               position "absolute"
                               backgroundColor "#888888"
                               fontFamily "\"Droid Sans mono\",\
                                           \"Courier New\",\
                                           \"Courier\",\
                                           \"monospace\""
                               color "#FFFFFF"))
            (screen (set-style (create-element "canvas")
                               position "absolute"))
            (lines (list))
            (cw null)
            (ch *line*)
            (top 0)
            (left 0)
            (row 0)
            (col 0)
            (s-row 0)
            (s-col 0)
            (ifind-mode false)
            (ifind-text "")
            (ifind-row 0)
            (ifind-col 0)
            (ifind-left 0)
            (ifind-top 0)
            (hinput (create-element "textarea"))
            (undo (list))
            (redo (list))
            (lastvalid -1)
            (lastins undefined)
            (focused false)
            (modified-level 0)
            (#'ensure (r)
                      (do ()
                          ((>= lastvalid r))
                        (let ((start-signature (if (= lastvalid -1)
                                                   "{}"
                                                   (aref lines lastvalid).end-signature))
                              (line (aref lines (1+ lastvalid))))
                          (unless (= start-signature line.start-signature)
                            (setf line.start-signature start-signature)
                            (setf line.start-context
                                  (if (= lastvalid -1)
                                      #()
                                      (aref lines lastvalid).end-context))
                            (setf line.end-context
                                  (mode.compute-end-context line))
                            (setf line.end-signature
                                  (signature line.end-context)))
                          (incf lastvalid))))
            (#'touch (r)
                     (when (<= r lastvalid)
                       (setf lastvalid (1- r)))
                     (setf (aref lines r).start-signature null))
            (#'modified ()
              (/= (length undo) modified-level))
            (#'clear-modified ()
              (setf modified-level (length undo))
              (update))
            (#'update ()
                      (setf screen.width screen.offsetWidth)
                      (setf screen.height screen.offsetHeight)
                      (let ((cr top)
                            (ctx (screen.getContext "2d"))
                            (info "")
                            (changed (if (modified) "*" "")))
                        (when (null? cw)
                          (font ctx #())
                          (setf cw (/ (ctx.measureText "XXXXXXXXXX").width 10)))
                        (setf ctx.fillStyle (if ifind-mode
                                                "#DDFFFF"
                                                "#FFFFFF"))
                        (ctx.fillRect 0 0 screen.width screen.height)
                        (setf ctx.textBaseline "top")
                        (do () ((or (>= cr (length lines))
                                    (>= (* (- cr top) ch) screen.offsetHeight))
                                  (if ifind-mode
                                      (setf info ~"Incremental search: \"{ifind-text}\"")
                                      (when (and (> col 0)
                                                 (find (aref lines row "text" (1- col))
                                                       "})]"))
                                        (let ((m (mode.parmatch lines row col)))
                                          (when m
                                            (let ((y0 (* (- (first m) top) *line*))
                                                  (x0 (* (- (second m) left) cw))
                                                  (y1 (* (- row top) *line*))
                                                  (x1 (* (- col left 1) cw)))
                                              (setf ctx.fillStyle "rgba(255,0,0,0.25)")
                                              (ctx.fillRect x0 y0 (+ cw 2) (+ *line* 2))
                                              (ctx.fillRect x1 y1 (+ cw 2) (+ *line* 2))
                                              (setf info (+ "Match: \""
                                                            (slice (aref lines (first m)).text
                                                                   (second m)
                                                                   (+ (second m) 20))
                                                            "\"")))))))
                                  (let ((coords (cond
                                                  ((and (= row s-row) (= col s-col))
                                                   (+ (1+ row) ":" col))
                                                  ((= row s-row)
                                                   (+ (1+ row) ":"
                                                      (min col s-col) "-"
                                                      (max col s-col)))
                                                  (true
                                                   (+ (1+ (min row s-row))
                                                      ":"
                                                      (if (< row s-row) col s-col)
                                                      "-"
                                                      (1+ (max row s-row))
                                                      ":"
                                                      (if (< row s-row) s-col col))))))
                                    (setf status.textContent
                                          ~" {name}{changed} \
                                            ({coords}) \
                                            {info}")))
                          (ensure cr)
                          (let ((line (aref lines cr))
                                (s0 (min row s-row))
                                (s1 (max row s-row)))
                            (when (<= s0 cr s1)
                              (let ((x0 0)
                                    (x1 0))
                                (cond
                                  ((= s0 s1)
                                   (setf x0 (min col s-col))
                                   (setf x1 (max col s-col)))
                                  ((= cr s0)
                                   (setf x0 (if (< row s-row) col s-col))
                                   (setf x1 (1+ (length line.text))))
                                  ((= cr s1)
                                   (setf x1 (if (< row s-row) s-col col)))
                                  (true
                                   (setf x1 (1+ (length line.text)))))
                                (when (> x1 (length line.text))
                                  (setf x1 (+ 1 left (floor (/ screen.offsetWidth cw)))))
                                (when (< x0 x1)
                                  (setf ctx.fillStyle "#FFFF00")
                                  (ctx.fillRect (* (- x0 left) cw)
                                                (* (- cr top) ch)
                                                (* (- x1 x0) cw)
                                                ch))))
                            (draw-line line.text line.sections
                                       ch screen.offsetWidth
                                       ctx 0 (* (- cr top) ch) (- (* cw left)))
                            (when (and focused (= cr row))
                              (setf ctx.fillStyle "#FF0000")
                              (ctx.fillRect (* cw (- col left)) (* ch (- cr top))
                                            2 *line*))
                            (incf cr)))))
            (#'fix ()
                   (let ((screen-lines (floor (/ screen.offsetHeight ch)))
                         (screen-cols (floor (/ screen.offsetWidth cw))))
                     (when (or (NaN? screen-lines) (< screen-lines 1))
                       (setf screen-lines 1))
                     (when (or (NaN? screen-cols) (< screen-cols 1))
                       (setf screen-cols 1))
                     (setf row (max 0 (min (1- (length lines)) row)))
                     (setf col (max 0 (min (length (aref lines row).text) col)))
                     (setf s-row (max 0 (min (1- (length lines)) s-row)))
                     (setf s-col (max 0 (min (length (aref lines s-row).text) s-col)))
                     (setf left (max 0 (- col screen-cols) (min left col)))
                     (setf top (max 0 (- row -1 screen-lines) (min row top (- (length lines) screen-lines)))))
                   (update))
            (#'selection ()
                         (let ((txt ""))
                           (if (= row s-row)
                               (setf txt (slice (aref lines row).text
                                                (min col s-col)
                                                (max col s-col)))
                               (let ((r0 (min row s-row))
                                     (r1 (max row s-row))
                                     (c0 (if (< row s-row) col s-col))
                                     (c1 (if (< row s-row) s-col col)))
                                 (setf txt (+ (slice (aref lines r0).text c0) "\n"))
                                 (dotimes (i (- r1 r0 1))
                                   (incf txt (+ (aref lines (+ r0 i 1)).text "\n")))
                                 (incf txt (slice (aref lines r1).text 0 c1))))
                           txt))
            (#'selection-to-hinput ()
                                   (let ((txt (selection)))
                                     (setf hinput.value txt)
                                     (hinput.setSelectionRange 0 (length txt))))
            (#'paste ()
                     (let ((pasted-lines (split (replace hinput.value "\r" "") "\n")))
                       (setf hinput.value "") ;; page rendering becomes slow otherwise (?!?)
                       (when (or (/= col s-col) (/= row s-row))
                         (delete-selection))
                       (let ((r0 row)
                             (c0 col)
                             (r1 null)
                             (c1 null))
                         (mutate
                          (let ((line (aref lines r0))
                                (r r0)
                                (c c0))
                            (if (= (length pasted-lines) 1)
                                (progn
                                  (setf line.text
                                        (+ (slice line.text 0 c)
                                           (aref pasted-lines 0)
                                           (slice line.text c)))
                                  (setf col (+ c (length (aref pasted-lines 0))))
                                  (setf row r)
                                  (touch r))
                                (let ((tail (slice line.text c)))
                                  (setf line.text
                                        (+ (slice line.text 0 c)
                                           (aref pasted-lines 0)))
                                  (touch r)
                                  (dotimes (i (- (length pasted-lines) 2))
                                    (let ((newline (new-line (aref pasted-lines (1+ i)))))
                                      (incf r)
                                      (insert lines r newline)))
                                  (let ((newline (new-line (+ (last pasted-lines) tail))))
                                    (incf r)
                                    (insert lines r newline)
                                    (setf row r)
                                    (setf col (length (last pasted-lines))))))
                            (setf s-col col)
                            (setf s-row row)
                            (setf r1 row)
                            (setf c1 col))
                          (let ((line (aref lines r0)))
                            (setf line.text
                                  (+ (slice line.text 0 c0)
                                     (slice (aref lines r1).text c1)))
                            (when (> r1 r0)
                              (splice lines (1+ r0) (- r1 r0)))
                            (touch r0)
                            (setf row r0)
                            (setf col c0)
                            (setf s-row r0)
                            (setf s-col c0)))))
                     (fix))
            (#'change-line (r c text)
                           (let ((oldtext (aref lines r).text)
                                 (oldr row)
                                 (oldc col)
                                 (oldsr s-row)
                                 (oldsc s-col)
                                 (l-undo (if (= lastins r)
                                             (first (pop undo)))))
                             (mutate
                              (progn
                                (setf (aref lines r).text text)
                                (setf row r)
                                (setf col c)
                                (setf s-row row)
                                (setf s-col col)
                                (touch r))
                              (progn
                                (setf (aref lines r).text oldtext)
                                (setf row oldr)
                                (setf col oldc)
                                (setf s-row oldsr)
                                (setf s-col oldsc)
                                (touch r)))
                             (when l-undo
                               (setf (first (last undo)) l-undo))
                             (setf lastins r)))
            (#'indent-selection ()
                                (dolist (r (range (min row s-row) (1+ (max row s-row))))
                                  (when (> r 0)
                                    (ensure (1- r)))
                                  (mode.autoindent lines r)
                                  (touch r))
                                (setf row (max row s-row))
                                (setf col (length (aref lines row).text))
                                (setf s-row row)
                                (setf s-col col)
                                (fix))
            (#'delete-selection ()
                                (if (= row s-row)
                                    (let ((text (aref lines row).text)
                                          (r row)
                                          (cc col)
                                          (sc s-col))
                                      (mutate
                                       (progn
                                         (setf (aref lines r).text
                                               (+ (slice text 0 (min cc sc))
                                                  (slice text (max cc sc))))
                                         (setf row r)
                                         (setf col (min cc sc))
                                         (setf s-col col)
                                         (touch r))
                                       (progn
                                         (setf (aref lines r).text text)
                                         (setf row r)
                                         (setf col cc)
                                         (setf s-col sc)
                                         (touch r))))
                                    (let ((changed-lines null)
                                          (org-r0 null)
                                          (rr row)
                                          (cc col)
                                          (sr s-row)
                                          (sc s-col)
                                          (r0 (min row s-row))
                                          (r1 (max row s-row))
                                          (c0 (if (< row s-row) col s-col))
                                          (c1 (if (< row s-row) s-col col)))
                                      (mutate
                                       (progn
                                         ;; Move lines to var for undo
                                         (setf changed-lines
                                               (slice lines (1+ r0) (1+ r1)))
                                         (setf org-r0 (aref lines r0).text)
                                         ;; Delete the lines
                                         (setf (aref lines r0).text
                                               (+ (slice (aref lines r0).text 0 c0)
                                                  (slice (aref lines r1).text c1)))
                                         (splice lines (1+ r0) (- r1 r0))
                                         (setf row r0)
                                         (setf col c0)
                                         (setf s-row row)
                                         (setf s-col col)
                                         (touch row))
                                       (progn
                                         ;; Recover
                                         (setf (aref lines r0).text org-r0)
                                         (setf lines (append (slice lines 0 (1+ r0))
                                                             changed-lines
                                                             (slice lines (1+ r0))))
                                         (setf changed-lines null)
                                         (setf row rr)
                                         (setf col cc)
                                         (setf s-row sr)
                                         (setf s-col sc)
                                         (dolist (r (range r0 (1+ r1)))
                                           (touch r)))))))
            (#'undo ()
                    (when (> (length undo) 0)
                      (setf lastins undefined)
                      (funcall (first (last undo)))
                      (push (pop undo) redo)))
            (#'redo ()
                    (when (> (length redo) 0)
                      (setf lastins undefined)
                      (funcall (second (last redo)))
                      (push (pop redo) undo))))

      (setf frame.lines (lambda () lines))
      (setf frame.selection #'selection)
      (setf frame.pos (lambda () (list row col)))
      (setf frame.set-pos (lambda (r c sr sc)
                            (setf row r)
                            (setf col c)
                            (setf s-row sr)
                            (setf s-col sc)
                            (fix)))
      (setf frame.name (lambda () name))
      (setf frame.rename (lambda (x)
                           (setf name x)
                           (update)))
      (setf frame.buffer (lambda ()
                           (join (map (get .text) lines) "\n")))
      (setf frame.modified #'modified)
      (setf frame.clear-modified #'clear-modified)
      (setf frame.update #'update)
      (setf frame.focus (lambda () (hinput.focus)))

      (set-style hinput
                 position "absolute"
                 px/left 0
                 px/top -100
                 px/width 1
                 px/height 1
                 outline "none"
                 border "none"
                 px/padding 0
                 px/margin 0)
      (append-child frame screen)
      (append-child frame status)
      (append-child frame hinput)
      (dolist (L (split content "\n"))
        (push (new-line L) lines))
      (setf frame."data-resize"
            (lambda (x0 y0 x1 y1)
              (set-style screen
                         px/left 0
                         px/top 0
                         px/width (round (- x1 x0))
                         px/height (round (- y1 y0 ch)))
              (set-style status
                         px/left 0
                         px/top (round (- y1 y0 ch))
                         px/width (round (- x1 x0))
                         px/height ch)
              (update)))
      (set-handler frame onmousewheel
        (let ((delta (floor (/ event.wheelDelta -60)))
              (screen-lines (floor (/ screen.offsetHeight ch))))
          (setf top (max 0 (min (+ top delta) (- (length lines) screen-lines)))))
        (update))
      (set-handler hinput onkeydown
        (let ((block true))
          (case event.which
            (9
               (indent-selection))
            (67
               (when event.ctrlKey
                 (selection-to-hinput)
                 ;; Clears input because page rendering (strangely)
                 ;; becomes a LOT slower otherwise
                 (set-timeout (lambda () (setf hinput.value "")) 50))
               (setf block false))
            (88
               (when event.ctrlKey
                 (selection-to-hinput)
                 (delete-selection)
                 (fix)
                 ;; Same trick as above
                 (set-timeout (lambda () (setf hinput.value "")) 50))
               (setf block false))
            (86
               (when event.ctrlKey
                 (setf hinput.value "")
                 (hinput.focus)
                 (set-timeout #'paste 10))
               (setf block false))
            (33
               (let ((delta (floor (/ screen.offsetHeight ch))))
                 (decf top delta)
                 (decf row delta)))
            (34
               (let ((delta (floor (/ screen.offsetHeight ch))))
                 (incf top delta)
                 (incf row delta)))
            (35
               (when event.ctrlKey
                 (setf row (1- (length lines))))
               (setf col (length (aref lines row).text)))
            (36
               (when event.ctrlKey
                 (setf row 0))
               (setf col 0))
            (37
               (if (> col 0)
                   (decf col)
                   (when (> row 0)
                     (decf row)
                     (setf col (length (aref lines row).text)))))
            (39
               (if (< col (length (aref lines row).text))
                   (incf col)
                   (when (< row (1- (length lines)))
                     (incf row)
                     (setf col 0))))
            (40
               (if (< row (1- (length lines)))
                   (progn
                     (incf row)
                     (when (> col (length (aref lines row).text))
                       (setf col (length (aref lines row).text))))
                   (setf col (length (aref lines row).text))))
            (38
               (if (> row 0)
                   (progn
                     (decf row)
                     (when (> col (length (aref lines row).text))
                       (setf col (length (aref lines row).text))))
                   (setf col 0)))
            (46
               (when (and (= row s-row) (= col s-col))
                 (if (< s-col (length (aref lines row).text))
                     (incf s-col)
                     (when (< s-row (1- (length lines)))
                       (incf s-row)
                       (setf s-col 0))))
               (delete-selection))
            (8
               (if ifind-mode
                   (progn
                     (when (> (length ifind-text) 0)
                       (setf ifind-text (slice ifind-text 0 -1))
                       (setf s-col (1- s-col)))
                     (event.preventDefault)
                     (event.stopPropagation)
                     (setf block false)
                     (fix))
                   (if (and (= row s-row) (= col s-col))
                       (if (> col 0)
                           (let ((text (aref lines row).text))
                             (change-line row (1- col)
                                          (+ (slice text 0 (1- col))
                                             (slice text col))))
                           (when (> row 0)
                             (let ((rr row)
                                   (sz (length (aref lines row).text)))
                               (mutate
                                (let ((line (aref lines rr))
                                      (prev-line (aref lines (1- rr))))
                                  (setf col (length prev-line.text))
                                  (incf prev-line.text line.text)
                                  (splice lines rr 1)
                                  (touch (1- rr))
                                  (setf row (1- rr))
                                  (setf s-col col)
                                  (setf s-row row))
                                (let* ((prev-line (aref lines (1- rr)))
                                       (line (new-line (slice prev-line.text (- sz)))))
                                  (setf prev-line.text (slice prev-line.text 0 (- sz)))
                                  (insert lines rr line)
                                  (setf row rr)
                                  (setf col 0)
                                  (setf s-col col)
                                  (setf s-row row)
                                  (touch (1- rr))
                                  (touch rr))))))
                       (delete-selection))))
            (13
               (if ifind-mode
                   (setf ifind-mode false)
                   (progn
                     (when (or (/= row s-row) (/= col s-col))
                       (delete-selection))
                     (let* ((r row)
                            (c col)
                            (text (aref lines row).text))
                       (mutate
                        (let ((line (aref lines r))
                              (newline (new-line (slice text c))))
                          (setf line.text (slice text 0 c))
                          (touch r)
                          (setf row (1+ r))
                          (insert lines row newline)
                          (setf col (or (mode.autoindent lines row) 0))
                          (touch (1+ r))
                          (setf s-row row)
                          (setf s-col col))
                        (let ((line (aref lines r)))
                          (setf line.text text)
                          (splice lines (1+ r) 1)
                          (touch r)
                          (setf row r)
                          (setf col c)
                          (setf s-row row)
                          (setf s-col col)))))))
            (27
               (when ifind-mode
                 (setf ifind-mode false)
                 (setf row ifind-row)
                 (setf col ifind-col)
                 (setf top ifind-top)
                 (setf left ifind-left)
                 (fix)))
            (83
               (when event.ctrlKey
                 (if ifind-mode
                     (let ((f (ifind ifind-text lines row (+ col (length ifind-text)))))
                       (when f
                         (setf row (first f))
                         (setf col (second f))
                         (setf s-row row)
                         (setf s-col (+ col (length ifind-text)))))
                     (progn
                       (setf ifind-mode true)
                       (setf ifind-text "")
                       (setf ifind-row row)
                       (setf ifind-col col)
                       (setf ifind-top top)
                       (setf ifind-left left)))
                 (event.stopPropagation)
                 (event.preventDefault)
                 (fix))
               (setf block false))
            (90
               (when event.ctrlKey
                 (undo)
                 (event.stopPropagation)
                 (event.preventDefault)
                 (fix))
               (setf block false))
            (89
               (when event.ctrlKey
                 (redo)
                 (event.stopPropagation)
                 (event.preventDefault)
                 (fix))
               (setf block false))
            (87
               (unless event.ctrlKey
                 (setf block false)))
            (otherwise
               (setf block false)))
          (when block
            (setf ifind-mode false)
            (event.preventDefault)
            (event.stopPropagation)
            (unless event.shiftKey
              (setf s-row row)
              (setf s-col col))
            (fix))))
      (set-handler hinput onfocus
        (setf focused true)
        (update))
      (set-handler hinput onblur
        (setf focused false)
        (update))
      (set-handler hinput onkeypress
        (event.preventDefault)
        (event.stopPropagation)
        (if ifind-mode
            (let* ((tx (+ ifind-text (char event.which)))
                   (f (ifind tx lines row col)))
              (when f
                (setf row (first f))
                (setf col (second f))
                (setf ifind-text tx)
                (setf s-row row)
                (setf s-col (+ col (length tx)))
                (fix)))
            (progn
              (when (or (/= row s-row) (/= col s-col))
                (delete-selection))
              (let ((text (aref lines row).text))
                (change-line row (1+ col)
                             (+ (slice text 0 col)
                                (char event.which)
                                (slice text col)))
                (fix)))))
      (set-handler screen onmousedown
        (hinput.focus)
        (labels ((pos (x y)
                   (let (((x0 y0) (element-pos screen)))
                     (decf x x0)
                     (decf y y0)
                     (if (and (< 0 x screen.clientWidth)
                              (< 0 y screen.clientHeight))
                         (let ((a (max 0 (min (1- (length lines)) (+ (floor (/ y ch)) top)))))
                           (list a (max 0 (min (floor (/ x cw)) (length (aref lines a).text)))))
                         (list null null)))))
          (let (((r c) (apply #'pos (event-pos event))))
            (unless (null? r)
              (event.preventDefault)
              (event.stopPropagation)
              (setf row r)
              (setf col c)
              (setf s-row r)
              (setf s-col c)
              (fix)
              (let* ((scroller-delta 0)
                     (scroller (set-interval (lambda ()
                                               (incf top scroller-delta)
                                               (incf row scroller-delta)
                                               (fix))
                                             20)))
                (tracking (lambda (x y)
                            (let (((r c) (pos x y)))
                              (if (null? r)
                                  (let (((sx sy) (element-pos screen))
                                        (sh screen.offsetHeight))
                                    (declare (ignorable sx))
                                    (when (< y sy)
                                      (setf scroller-delta (floor (/ (- y sy) ch))))
                                    (when (> y (+ sy sh))
                                      (setf scroller-delta (1+ (floor (/ (- y (+ sy sh)) ch))))))
                                  (progn
                                    (setf scroller-delta 0)
                                    (setf row r)
                                    (setf col c)
                                    (fix)))))
                          (lambda () (clear-interval scroller))))))))
      (set-timeout #'fix 10)
      (set-timeout (lambda () (hinput.focus)) 10)
      frame)))

(defun fullscreen-editor (name content &optional (mode nullmode))
  (let ((editor (editor name content mode))
        (frame (create-element "div")))
    (set-style frame
               position "absolute"
               px/left 8
               px/top 8
               px/bottom 8
               px/right 8)
    (set-style editor
               position "absolute"
               px/left 0
               px/top 0)
    (append-child frame editor)
    (append-child document.body frame)
    (let ((fw 0) (fh 0))
      (set-interval (lambda ()
                      (let ((w frame.offsetWidth)
                            (h frame.offsetHeight))
                        (when (or (/= w fw) (/= h fh))
                          (setf fw w)
                          (setf fh h)
                          (set-style editor
                                     px/left 0
                                     px/top 0
                                     px/width fw
                                     px/height fh)
                          (editor."data-resize" 0 0 fw fh))))
                    10))
    editor))

(export editor fullscreen-editor nullmode new-section)
