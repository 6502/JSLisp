(load (get-file "deploy.lisp"))
(setf *deploy-prefix* "<!DOCTYPE html><html><head><meta http-equiv=\"Content-Type\" content = \"text/html; charset=utf-8\"></head><body><script>\n")
(setf *deploy-suffix* "\n</script></body></html>")