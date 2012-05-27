(import * from gui)

(defun main ()
  (with-window (w (100 100 780 350 :title "Test window" :close false)
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
                  (:V :spacing 8 :border 8
                      (:H :size 35
                          (:Hdiv title :weight 15)
                          (:Hdiv first-name :weight 50)
                          (:Hdiv last-name))
                      (:H :size 35 (:Hdiv address))
                      (:H :size 35 (:H (:Hdiv email) (:Hdiv phone)) (:Hdiv age :size 70))
                      (:V :size 10)
                      (:V :size 95
                          (:H (:Vdiv freebies :border 8 :spacing 4
                                     (:V :size 3)
                                     (:H :size 20 (:Hdiv newsletter))
                                     (:H :size 20 (:Hdiv spam)))
                              (:Vdiv billing :weight 60 :border 8 :spacing 4
                                     (:V :size 3)
                                     (:H :size 16 (:Hdiv yearly-charge))
                                     (:H :size 16 (:Hdiv monthly-charge))
                                     (:H :size 16 (:Hdiv single-charge)))))
                      (:H)
                      (:H :size 30
                          (:H)
                          (:Hdiv ok :size 80)
                          (:Hdiv cancel :size 80)
                          (:H))))
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
