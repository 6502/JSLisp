(import * from gui)
(import * from layout)
(import * from graphics)

(defun main ()
  (with-window (w (0 0 680 370 title: "Test window" close: false)
                  ((title (select "mr/ms" '("Mr." "Ms.")))
                   (first-name (input "first name"))
                   (last-name (input "last name"))
                   (birthdate (date-input "birthdate"))
                   (address (input "address"))
                   (preferred-color (css-color-input "preferred color"))
                   (email (input "e-mail"))
                   (phone (input "phone"))
                   (age (select "age" (append (range 1 100) "100+")))
                   (newsletter (checkbox "YES! I want to subscribe to the free newsletter"))
                   (spam (checkbox "YES! I want to feed my mailbox with unrelated spam"))
                   (freebies (group "Freebies"))
                   (yearly-charge (radio "charge" "$120 yearly"))
                   (monthly-charge (radio "charge" "$12 monthly"))
                   (single-charge (radio "charge" "$10 once (one week access)"))
                   (billing (group "Billing"))
                   (ok (button "OK" (lambda ()
                                      (display ~"name: {(text title)} {(text first-name)} {(text last-name)}")
                                      (display ~"birthdate: {(text birthdate)}")
                                      (display ~"address: {(text address)}")
                                      (display ~"preferred-color: {(text preferred-color)}")
                                      (display ~"email: {(text email)}")
                                      (display ~"phone: {(text phone)}")
                                      (display ~"age: {(text age)}")
                                      (display ~"newletter: {(checked newsletter)}")
                                      (display ~"spam: {(checked spam)}")
                                      (display ~"yearly-charge: {(checked yearly-charge)}")
                                      (display ~"monthly-charge: {(checked monthly-charge)}")
                                      (display ~"single-charge: {(checked single-charge)}")
                                      (hide-window w))))
                   (cancel (button "Cancel" (lambda () (hide-window w)))))
                  (V spacing: 8 border: 16
                     size: 40
                     (H weight: 15 (dom title)
                        weight: 50 (dom first-name)
                        weight: 50 (dom last-name)
                        weight: 30 (dom birthdate))
                     (H (dom address) weight: 25 (dom preferred-color))
                     (H (dom email) (dom phone) size: 70 (dom age))
                     size: 10 null
                     size: 80
                     (H (V spacing: 4
                           (dom freebies
                                (V border: 4
                                   size: 10 null
                                   size: 20
                                   (dom newsletter)
                                   (dom spam))))
                        weight: 60
                        (V spacing: 4
                           (dom billing
                                (V border: 4
                                   size: 5 null
                                   size: 16
                                   (dom yearly-charge)
                                   (dom monthly-charge)
                                   (dom single-charge)))))
                     :filler:
                     size: 30
                     (H :filler: size: 80 (dom ok) (dom cancel) :filler:)))
    (show-window w center: true)
    (setf (text title) "Mr.")
    (setf (text first-name) "Andrea")
    (setf (text last-name) "Griffini")
    (setf (text birthdate) "1966-07-07")
    (setf (text address) "Via somewhere, somecivic")
    (setf (text preferred-color) "#80C0C0")
    (preferred-color.update-style)
    (setf (text email) "agriffini@jslisp.org")
    (setf (text phone) "555 123 4567")
    (setf (text age) "45")
    (setf (checked newsletter) true)
    (setf (checked spam) true)
    (setf (checked yearly-charge) true)))

(main)
