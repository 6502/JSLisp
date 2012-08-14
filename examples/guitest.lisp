(import * from gui)
(import * from layout)

(defun main ()
  (with-window (w (50 50 680 350 title: "Test window" close: false)
                  ((title (select "mr/ms" '("Mr." "Ms.")))
                   (first-name (input "first name"))
                   (last-name (input "last name"))
                   (address (input "address"))
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
                                      (display ~"address: {(text address)}")
                                      (display ~"email: {(text email)}")
                                      (display ~"phone: {(text phone)}")
                                      (display ~"age: {(selection age)}")
                                      (display ~"newletter: {(checked newsletter)}")
                                      (display ~"spam: {(checked spam)}")
                                      (display ~"yearly-charge: {(checked yearly-charge)}")
                                      (display ~"monthly-charge: {(checked monthly-charge)}")
                                      (display ~"single-charge: {(checked single-charge)}")
                                      (hide-window w))))
                   (cancel (button "Cancel" (lambda () (hide-window w)))))
                  (V spacing: 8 border: 8
                     size: 35
                     (H weight: 15 (dom title)
                        weight: 50 (dom first-name)
                        weight: 50 (dom last-name))
                     (H (dom address))
                     (H (dom email) (dom phone) size: 70 (dom age))
                     size: 10 null
                     size: 80
                     (H spacing: 8
                        (V spacing: 4
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
    (show-window w)
    (setf (text title) "Mr.")
    (setf (text first-name) "Andrea")
    (setf (text last-name) "Griffini")
    (setf (text address) "Via somewhere, somecivic")
    (setf (text email) "agriffini@jslisp.org")
    (setf (text phone) "555 123 4567")
    (setf (selection age) "45")
    (setf (checked newsletter) true)
    (setf (checked spam) true)
    (setf (checked yearly-charge) true)))

(main)
