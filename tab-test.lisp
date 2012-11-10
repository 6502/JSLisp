(import * from gui)
(import * from layout)

(let** ((w (window 0 0 600 400 title: "Tab test"))
        (t (add-widget w (tabbed)))
        (ok (add-widget w (lbutton "OK" (hide-window w))))
        (cancel (add-widget w (lbutton "Cancel" (hide-window w)))))
  (setf w.client.style.backgroundColor "#EEEEEE")
  (setf w.frame.style.backgroundColor "#EEEEEE")
  (set-layout w (V border: 12 spacing: 12
                   (dom t)
                   size: 30
                   (H spacing: 16 :filler: size: 80 (dom ok) (dom cancel) :filler:)))

  (let** ((pg (set-style (create-element "div") position "absolute"))
          (first-name (append-child pg (input "First name")))
          (last-name (append-child pg (input "Last name")))
          (birth-date (append-child pg (date-input "Birth date")))
          (address (append-child pg (input "address")))
          (layout (V spacing: 8 border: 8
                     size: 40
                     (dom first-name)
                     (H spacing: 16 (dom last-name) size: 130 (dom birth-date))
                     (dom address))))
    (setf pg."data-resize" (lambda (x0 y0 x1 y1)
                             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (t.add "Infos" pg))

  (let** ((pg (set-style (create-element "div") position "absolute"))
          (home-phone (append-child pg (input "Home phone")))
          (office-phone (append-child pg (input "Office phone")))
          (mobile-phone (append-child pg (input "Mobile phone")))
          (personal-email (append-child pg (input "Personal email")))
          (work-email (append-child pg (input "Work email")))
          (website (append-child pg (input "Personal website")))
          (layout (V spacing: 8 border: 8
                     size: 40
                     (H spacing: 16 (dom home-phone) (dom office-phone) (dom mobile-phone))
                     (H spacing: 16 (dom personal-email) (dom work-email))
                     (H spacing: 16 (dom website)))))
    (setf pg."data-resize" (lambda (x0 y0 x1 y1)
                             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (t.add "Address" pg))

  (let** ((pg (set-style (create-element "div") position "absolute"))
          (notes (append-child pg (text-area "Notes")))
          (layout (V border: 8 (dom notes))))
    (setf pg."data-resize" (lambda (x0 y0 x1 y1)
                             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (t.add "Notes" pg))

  (let** ((pg (set-style (create-element "div") position "absolute"))
          (data (list (list "X001" 10)
                      (list "X002"  5)
                      (list "X003"  7)
                      (list "X004"  1)))
          (items #((X001 #((descr "Article 1") (uprice 100)))
                   (X002 #((descr "Article 2") (uprice 200)))
                   (X003 #((descr "Article 3") (uprice 300)))
                   (X004 #((descr "Article 4") (uprice 400)))
                   (X005 #((descr "Article 5") (uprice 500)))
                   (X006 #((descr "Article 6") (uprice 600)))
                   (X008 #((descr "Article 7") (uprice 800)))
                   (X009 #((descr "Article 8") (uprice 900)))))
          (#'mktab ()
                   (let ((tabdata (list (list "SKU" "Description" "Unit price" "Qty" "Amount" "")))
                         (total 0)
                         (tqty 0))
                     (enumerate (i (sku qty) data)
                       (let ((item (aref items sku)))
                         (incf total (* item.uprice qty))
                         (incf tqty qty)
                         (push (list sku item.descr item.uprice qty (* item.uprice qty)
                                     (append-child pg (lbutton "x"
                                                         (splice data i 1)
                                                         (update))))
                               tabdata)))
                     (push (list "" "TOTAL" "" tqty total "")
                           tabdata)
                     (append-child pg (table tabdata
                                             rows: (V spacing: 4
                                                      size: 40 null
                                                      size: 25 null)
                                             cols: (H spacing: 4
                                                      weight: 25 null
                                                      weight: 100 null
                                                      weight: 25 null
                                                      weight: 25 null
                                                      weight: 25 null
                                                      size: 25 null)))))
          (#'update () (setf cart (dom-replace cart (mktab))))
          (cart (mktab))
          (clear (append-child pg (lbutton "Clear" (setf data (list)) (update))))
          (layout (V border: 8 spacing: 8
                     (dom cart)
                     size: 25
                     (H spacing: 16
                        :filler: size: 120 (dom clear) :filler:))))
    (setf pg."data-resize" (lambda (x0 y0 x1 y1)
                             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (t.add "Cart" pg))

  (show-window w center: true))