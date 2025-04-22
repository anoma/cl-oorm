(in-package :cl-rm.view)

(defview contents (object cl-rm.env::compilation-environment)
  (html-view
   :title "environment" :priority 5
   (html (fset:do-map (key val-map (environment object))
           (html
             (:details
              :open t
              (:summary (object-ref (resource->obj key)))
              (:table :class "inspector-table"
                      (:tr (:th "Key")
                           (:th "Value"))
                      (fset:do-map (key2 val2 val-map)
                        (html
                          (:tr :class "inspector-inspect"
                               (:td (object-ref key2))
                               (:td (object-ref val2))))))))))))
