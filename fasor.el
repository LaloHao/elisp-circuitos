;;; fasor.el --- Calculo de fasores
;;; Commentary:

;;; Code:
(load-file "@.el")
(require '@)

(defun grad2rad (grados)
  "Convierte GRADOS a radianes."
  (/ (* float-pi grados) 180))

(defun rad2grad (radianes)
  "Convierte RADIANES a grados."
  (/ (* 180 radianes) float-pi))

(defvar @fasor (@extend :_magnitud :_grados :_radianes
                      :_x :_y))

(def@ @fasor :magnitud (&optional magnitud)
  (when magnitud
    (progn (setf @:_magnitud magnitud)
           (setf @:_x (* magnitud (cos @:_radianes)))
           (setf @:_y (* magnitud (sin @:_radianes)))))
  @:_magnitud)

(def@ @fasor :radianes (&optional radianes)
  (when radianes
    (progn (setf @:_radianes radianes)
           (setf @:_grados (rad2grad radianes))
           (setf @:_x (* @:_magnitud (cos radianes)))
           (setf @:_y (* @:_magnitud (sin radianes)))))
  @:_radianes)

(def@ @fasor :grados (&optional grados)
  (when grados
    (progn (setf @:_grados grados)
           (setf @:_radianes (grad2rad grados))
           (setf @:_x (* @:_magnitud (cos @:_radianes)))
           (setf @:_y (* @:_magnitud (sin @:_radianes)))))
  @:_grados)

(def@ @fasor :x (&optional x)
  (when x
    (progn (setf @:_x x)
           (setf @:_magnitud (sqrt (+ (* @:_x @:_x) (* @:_y @:_y))))
           (setf @:_radianes (atan @:_y @:_x))
           (setf @:_grados (rad2grad @:_radianes))))
  @:_x)

(def@ @fasor :y (&optional y)
  (when y
    (progn (setf @:_y y)
           (setf @:_magnitud (sqrt (+ (* @:_x @:_x) (* @:_y @:_y))))
           (setf @:_radianes (atan @:_y @:_x))
           (setf @:_grados (rad2grad @:_radianes))))
  @:_y)

(setf fasor-temporal (@! @fasor :new))

(def@ @fasor :+ (otro-fasor)
    (@! fasor-temporal :x (+ @:_x (@ otro-fasor :_x)))
    (@! fasor-temporal :y (+ @:_y (@ otro-fasor :_y)))
    (@! @fasor :new (@! fasor-temporal :magnitud) (@! fasor-temporal :radianes)))

(def@ @fasor :- (otro-fasor)
    (@! fasor-temporal :x (- @:_x (@ otro-fasor :_x)))
    (@! fasor-temporal :y (- @:_y (@ otro-fasor :_y)))
    (@! @fasor :new (@! fasor-temporal :magnitud) (@! fasor-temporal :radianes)))

(def@ @fasor :* (otro-fasor)
  (@! fasor-temporal :magnitud (* @:_magnitud (@ otro-fasor :_magnitud)))
  (@! fasor-temporal :grados (+ @:_grados (@ otro-fasor :_grados)))
  (@! @fasor :new (@! fasor-temporal :magnitud) (@! fasor-temporal :radianes)))

(def@ @fasor :/ (otro-fasor)
  (@! fasor-temporal :magnitud (/ @:_magnitud (@ otro-fasor :_magnitud)))
  (@! fasor-temporal :grados (- @:_grados (@ otro-fasor :_grados)))
  (@! @fasor :new (@! fasor-temporal :magnitud) (@! fasor-temporal :radianes)))

(def@ @fasor :inverso ()
  (@! fasor-temporal :magnitud (/ 1.0 @:_magnitud))
  (@! fasor-temporal :grados (* -1 @:_grados))
  (@! @fasor :new (@! fasor-temporal :magnitud) (@! fasor-temporal :radianes)))

(def@ @fasor :init (&optional magnitud radianes)
  (@^:init)
  (setf @:_radianes (or radianes 0))
  (setf @:_grados (or (when radianes (rad2grad radianes)) 0))
  (setf @:_x 0)
  (setf @:_y 0)
  (if magnitud
      (@:magnitud magnitud)
    (setf @:_magnitud 0)))

;; Debugeo

;; (let ((f1 (@! @fasor :new 12 (grad2rad 0)))
;;       (f2 (@! @fasor :new 8 (grad2rad 45)))
;;       (f3 (@! @fasor :new 10 (grad2rad 30))))
;;   ;; (@! f3 :+ (@! f1 :+ f2))
;;   (@! (@! f3 :+ (@! f1 :+ f2)) :inverso)
;;   ;; (@! f1 :inverso)
;;   ;; (@! f2 :inverso)
;;   ;; (@! f3 :inverso)
;;   )

;; (let ((f1 (@! @fasor :new 25 (grad2rad 30)))
;;       (f2 (@! @fasor :new 25 (grad2rad 30))))
;;   (@! f1 :- f2))

;; (let ((f1 (@! @fasor :new 12.7 (grad2rad -30)))
;;       (f2 (@! @fasor :new 10 (grad2rad 30))))
;;   (@! f1 :* f2))

;; (let ((f1 (@! @fasor :new 127 (grad2rad 0)))
;;       (f2 (@! @fasor :new 12.7 (grad2rad -30))))
;;   (@! f1 :/ f2))

(provide 'fasor)
;;; fasor.el ends here
