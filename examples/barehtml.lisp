(import * from html)

(defun main ()
  (base-css "examples/base.css")
  (let** ((view (document.body.appendChild (create-element "div")))
          (#'login-view ()
            (setf view.innerHTML (template "examples/login.html"))
            (focus ##user)
            (on ##ok click
              (if (and (= ##user.value "agri")
                       (= ##password.value "pw"))
                  (main-view)
                  (progn
                    (focus ##user)
                    (show ##errmsg text: "Invalid user/password" delay: 2000)))))
          (#'main-view ()
            (setf view.innerHTML (template "examples/main.html"))
            (on ##wq mouseover
              (setf ##message.innerText "This is a white queen"))
            (on ##bn mouseover
              (setf ##message.innerText "This is a black knight"))
            (on ##logout click
              (login-view))))
    (login-view)))

(main)
