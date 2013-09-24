(import * from gui)
(import * from layout)

(defun drag-element (e containers)
  (let** ((container e.div.parentNode)
          (cix (index container containers))
          (bros e.div.parentNode.children)
          (w container.clientWidth)
          (h e.div.offsetHeight)
          (placeholder (set-style (create-element "div")
                                  border "dashed 1px #000"
                                  position "relative"
                                  px/padding 0
                                  px/margin -1
                                  px/width w
                                  px/height h))
          (wrapper (set-style (create-element "div")
                              position "absolute"
                              webkitTransform "rotate(-5deg)"
                              mozTransform "rotate(-5deg)"
                              transform "rotate(-5deg)"
                              px/width w
                              px/height h))
          (cx0 (map (lambda (cc) (first (element-pos cc))) containers))
          (cx1 (map (lambda (cc) (+ (first (element-pos cc)) cc.offsetWidth)) containers)))
    (nremove e container.parentNode.data-board.cards)
    (append-child container placeholder e.div)
    (append-child container wrapper)
    (append-child wrapper e.div)
    (set-style wrapper
               px/left placeholder.offsetLeft
               px/top placeholder.offsetTop)
    (tracking (lambda (x y)
                (let ((x0 container.parentNode.offsetLeft)
                      (y0 container.parentNode.offsetTop))
                  (set-style wrapper
                             px/left (- x x0 (/ w 2))
                             px/top (- y y0 (/ h 2)))
                  (do ()
                    ((or (null? placeholder.nextSibling)
                         (> (+ placeholder.offsetTop
                               placeholder.offsetHeight)
                            (- y y0))))
                    (append-child container placeholder placeholder.nextSibling.nextSibling))
                  (do ()
                    ((or (null? placeholder.previousSibling)
                         (< placeholder.offsetTop (- y y0))))
                    (append-child container placeholder placeholder.previousSibling))
                  (do ()
                    ((or (= 0 cix) (> x (aref cx1 (1- cix)))))
                    (decf cix)
                    (setf container (aref containers cix))
                    (append-child container placeholder)
                    (append-child container wrapper)
                    (let ((x0 container.parentNode.offsetLeft)
                          (y0 container.parentNode.offsetTop))
                      (set-style wrapper
                                 px/left (- x x0 (/ w 2))
                                 px/top (- y y0 (/ h 2)))))
                  (do ()
                    ((or (= (1- (length containers)) cix) (< x (aref cx0 (1+ cix)))))
                    (incf cix)
                    (setf container (aref containers cix))
                    (append-child container placeholder)
                    (append-child container wrapper))
                    (let ((x0 container.parentNode.offsetLeft)
                          (y0 container.parentNode.offsetTop))
                      (set-style wrapper
                                 px/left (- x x0 (/ w 2))
                                 px/top (- y y0 (/ h 2))))))
              (lambda ()
                (remove-child container wrapper)
                (insert container.parentNode.data-board.cards
                        (do ((i 0 (1+ i)))
                          ((or (>= i (length container.children))
                               (= (aref container.children i) placeholder))
                           i))
                        e)
                (append-child container e.div placeholder)
                (remove-child container placeholder)))))

(defobject board (div title cards))
(defobject card (div title
                     description))

(defvar boards (list))

(defun board (title)
  (let** ((area (set-style (create-element "div")
                           px/margin 8
                           px/padding 8
                           px/borderRadius 8
                           backgroundColor "#888"
                           color "#FFF"
                           px/font-size 16
                           fontWeight "bold"
                           fontFamily "sans-serif"))
          (container (create-element "div"))
          (b (make-board div: area
                         title: title
                         cards: (list)))
          (#'add-card (title)
            (let** ((card (set-style (create-element "div")
                                     cursor "move"
                                     px/padding 8
                                     px/margin 4
                                     px/borderRadius 8
                                     backgroundColor "#FFF"
                                     boxShadow "4px 4px 4px rgba(0,0,0,0.25)"
                                     color "#000"))
                    (c (make-card div: card
                                  title: title
                                  description: "")))
              (set-handler card onmousedown
                           (event.preventDefault)
                           (event.stopPropagation)
                           (drag-element c boards))
              (setf card.textContent title)
              (append-child container card)
              (push c b.cards)
              (setf card.data-card c)
              c)))
    (setf area.data-board b)
    (push container boards)
    (setf area.innerHTML title)
    (append-child area container)
    (append-child document.body area)
    (setf b.add-card #'add-card)
    b))

(defun main ()
  (let ((b1 (board "<center><h2>Test-board-1</h2><br>Second line<br>Third</center>"))
        (b2 (board "<center><h2>Test-board-2</h2></center>"))
        (b3 (board "<center><h2>Third</h2></center>")))
    (b1.add-card "This")
    (b1.add-card "is")
    (b1.add-card "a")
    (b2.add-card "test")
    (b2.add-card "of a")
    (b2.add-card "kanban")
    (b2.add-card "trello")
    (b2.add-card "like")
    (b2.add-card "widget")
    (set-style b1.div
               position "absolute"
               px/left 300
               px/top 200)
    (set-style b2.div
               position "absolute"
               px/left (+ 300 b1.div.offsetWidth 16)
               px/top 200)
    (set-style b3.div
               position "absolute"
               px/left (+ 300 b1.div.offsetWidth b2.div.offsetWidth 32)
               px/top 200)))

(main)