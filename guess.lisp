(defstruct doubt question if-yes if-no)

(defstruct answer message)

(defmacro setter (place)
  (let ((x (gensym)))
    `(lambda (,x) (setf ,place ,x))))

(defvar *db* (make-answer message: "A dog."))

(defun guess (&key (node *db*)
              (setter (lambda (x) (setf *db* x))))
  (if (doubt? node)
      (if (yesno (doubt-question node))
          (guess node: (doubt-if-yes node)
                 setter: (setter (doubt-if-yes node)))
          (guess node: (doubt-if-no node)
                 setter: (setter (doubt-if-no node))))
      (if (yesno ~"The answer is: {(answer-message node)}\nIs this correct?")
          (display "Yay!")
          (let* ((correct (prompt "What was it then?"))
                 (question (prompt (+ "Please type a yes/no question that allows to distinguish\n"
                                      (answer-message node) "\nfrom\n"
                                      correct)))
                 (correct-answ (yesno ~"And the answer for \"{correct}\" would be?"))
                 (new-answer (make-answer message: correct)))
            (funcall setter (make-doubt question: question
                                        if-yes: (if correct-answ new-answer node)
                                        if-no: (if correct-answ node new-answer)))))))
