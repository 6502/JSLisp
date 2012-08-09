(load (get-file "deploy.lisp"))
(setf *deploy-prefix* "<doctype HTML><html><body><script>\n")
(setf *deploy-suffix* "\n</script></body></html>")