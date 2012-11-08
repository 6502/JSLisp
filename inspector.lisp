(import * from gui)
(import * from layout)

(defun inspector-node (text)
  (let ((cell (create-element "td"))
        (row (create-element "tr"))
        (table (create-element "table")))
    (append-child table row)
    (append-child row cell)
    (setf cell.colSpan 2)
    (setf cell.textContent "text")
    (setf cell.textContent text)
    (setf table.cellPadding 0)
    (setf table.cellSpacing 0)
    (set-style table
               borderCollapse "collapse"
               cursor "default")
    table))

(defun add-child (inspector label content)
  (let ((label-cell (if (undefined? label) null (create-element "td")))
        (content-cell (create-element "td"))
        (row (create-element "tr")))
    (if (undefined? label)
        (setf content-cell.colSpan 2)
        (progn
          (setf label-cell.vAlign "top")
          (set-style label-cell
                     px/padding 4
                     border "solid 1px #CCCCCC")
          (setf label-cell.textContent label)
          (append-child row label-cell)))
    (setf content-cell.vAlign "top")
    (set-style content-cell
               px/padding 4
               border "solid 1px #CCCCCC")
    (append-child content-cell content)
    (append-child row content-cell)
    (append-child inspector row)
    row))

(defun inspector (x)
  (declare (ignorable x))
  (inspector-node "Unknown object"))

(defmethod inspector (x) (string? x)
  (let ((n (inspector-node ~"string: {(json x)}")))
    (set-style n whiteSpace "normal")
    n))

(defmethod inspector (x) (number? x)
  (inspector-node ~"number: {(str-value x)}"))

(defmethod inspector (x) (list? x)
  (let ((n (inspector-node ~"list: {(length x)} elements"))
        (children (list)))
    (set-handler n onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (if (length children)
          (do () ((= 0 (length children)))
            (let ((c (pop children)))
              (remove-child c.parentNode c)))
          (enumerate (i child x)
            (push (add-child n ~"{i}" (inspector child)) children))))
    n))

(defmethod inspector (x) (null? x)
  (inspector-node "null"))

(defmethod inspector (x) (undefined? x)
  (inspector-node "undefined"))

(defmethod inspector (x) (NaN? x)
  (inspector-node "NaN"))

(defmethod inspector (x) (bool? x)
  (inspector-node "boolean: {(str-value x)}"))

(defmethod inspector (x) (symbol? x)
  (let ((n (inspector-node ~"symbol: {(symbol-full-name x)}"))
        (children (list)))
    (set-handler n onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (if (length children)
          (do () ((= 0 (length children)))
            (let ((c (pop children)))
              (remove-child c.parentNode c)))
          (progn
            (push (add-child n ~"macro" (inspector (symbol-macro x))) children)
            (push (add-child n ~"function" (inspector (symbol-function x))) children)
            (push (add-child n ~"value" (inspector (symbol-value x))) children))))
    n))

(defmethod inspector (x) (callable? x)
  (let ((n (inspector-node (if x.arglist
                               ~"(lambda {(str-value x.arglist)} ...)"
                               "#CODE")))
        (children (list)))
    (set-handler n onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (if (length children)
          (do () ((= 0 (length children)))
            (let ((c (pop children)))
              (remove-child c.parentNode c)))
          (when x.documentation
            (push (add-child n "documentation" (inspector x.documentation)) children))))
    n))

(defmethod inspector (x) (and x x."%class")
  (let ((n (inspector-node (+ "object " (first x."%class"))))
        (children (list)))
    (set-handler n onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (if (length children)
          (do () ((= 0 (length children)))
            (let ((c (pop children)))
              (remove-child c.parentNode c)))
          (progn
            (dolist (f (slice x."%class" 1))
              (push (add-child n ~"{f}" (inspector (aref x f))) children))
            (dolist (f (keys x))
              (unless (find f (slice x."%class" 1))
                (push (add-child n ~"({f})" (inspector (aref x f))) children))))))
    n))

(defmethod inspector (x) (object? x)
  (let ((n (inspector-node (+ "generic object")))
        (children (list)))
    (set-handler n onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (if (length children)
          (do () ((= 0 (length children)))
            (let ((c (pop children)))
              (remove-child c.parentNode c)))
          (dolist (f (keys x))
            (push (add-child n ~"{(json f)}" (inspector (aref x f))) children))))
    n))

(defun inspect (x)
  (let** ((w (window 0 0 200 100 title: "Inspector"))
          (div (add-widget w (create-element "div"))))
    (set-style div
               px/font 16
               fontFamily "monospace"
               fontWeight "bold"
               whiteSpace "pre"
               overflow "auto")
    (append-child div (inspector x))
    (set-layout w (V border: 8 (dom div)))
    (show-window w center: true)))