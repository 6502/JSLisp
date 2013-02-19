(import * from gui)
(import * from layout)

(defobject node (text children))

(defun node (text &rest args)
  (new-node text args))

(defun test-treeview ()
  (let** ((w (window 0 0 0.75 0.75 title: "TreeView"))
          (tree (node "Root"
                      (node "1"
                            (node "1.1")
                            (node "1.2"))
                      (node "2"
                            (node "2.1"))
                      (node "3"
                            (node "3.1")
                            (node "3.2")
                            (node "3.3")
                            (node "3.4"))))
          (tw (add-widget w (tree-view tree onclick: #'edit)))
          (add (add-widget w (button "Add" #'add)))
          (cut (add-widget w (button "Cut" #'cut)))
          (paste (add-widget w (button "Paste" #'paste)))
          (clipboard null)
          (#'edit (n)
            (let ((name (prompt "New node name?")))
              (when name
                (setf n.text name)
                (tw.rebuild))))
          (#'add ()
            (let ((name (prompt "Node name?")))
              (when name
                (baloon "Select insert point")
                (tw.select-place (lambda (parent index)
                                   (insert parent.children index (node name))
                                   (tw.rebuild))))))
          (#'cut ()
            (baloon "Select node")
            (tw.select-node (lambda (n)
                              (setf clipboard n)
                              (let** ((#'remove-from (x)
                                        (nremove n x.children)
                                        (dolist (c x.children)
                                          (remove-from c))))
                                (remove-from tree)
                                (tw.rebuild)))))
          (#'paste ()
            (when clipboard
              (baloon "Select insert point")
              (tw.select-place (lambda (parent index)
                                 (insert parent.children index clipboard)
                                 (setf clipboard null)
                                 (tw.rebuild))))))
    (set-layout w (V border: 8 spacing: 8
                     (dom tw)
                     size: 30
                     (H :filler:
                        size: 80
                        (dom add)
                        (dom cut)
                        (dom paste)
                        :filler:)))
    (show-window w center: true)))

(defun main ()
  (test-treeview))

(main)
