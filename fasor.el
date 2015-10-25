;;; fasor.el --- Calculo de fasores
;;; Commentary:
;;; Hecho para realizar cálculos de fasores de manera mas fácil,
;;; la mayoría de los ejemplos son del libro Análisis de circuitos de Hayt 8ed,
;;; los demas son tareas o ejemplos inventados

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

(def@ @fasor :init (&optional magnitud radianes forma)
  (@^:init)
  (setf @:_x 0)
  (setf @:_y 0)
  (setf @:_magnitud 0)
  (setf @:_radianes 0)
  (setf @:_grados 0)
  (if (eq forma 'xy)
      (progn
        (@:x magnitud)
        (@:y radianes)
        )
    (progn
      (when radianes
        (@:radianes radianes))
      (when magnitud
        (@:magnitud magnitud))
      )
    ))

;; Untested
;; (defun fasor (&optional x y form)
;;   (@! @fasor :new x y form))

;; (defun ++ (&rest fasores)
;;   (when (>= (length fasores) 2)
;;     (@! :+ )
;;     ))


;; Debugeo
;; (@! @fasor :new 4 3 'xy)
;; (@! @fasor :new 5 0)

;; Calculo de actividad 2 (tarea)
;; (let* (
;;     (Vp (/ 220 (sqrt 3)))
;;     (za (@! @fasor :new 12 (grad2rad 0)))
;;     (zb (@! @fasor :new 8 (grad2rad 45)))
;;     (zc (@! @fasor :new 10 (grad2rad 30)))
;;     (van (@! @fasor :new Vp (grad2rad 0)))
;;     (vbn (@! @fasor :new Vp (grad2rad -120)))
;;     (vcn (@! @fasor :new Vp (grad2rad -240)))
;;     (ya (@! za :inverso))
;;     (yb (@! zb :inverso))
;;     (yc (@! zc :inverso))
;;     )
;;   (@! ya :+ (@! yb :+ yc))
;;   (let* (
;;       (t1 (@! ya :* van))
;;       (t2 (@! yb :* vbn))
;;       (t3 (@! yc :* vcn))
;;       (UP (@! (@! t1 :+ t2) :+ t3))
;;       (DOWN (@! ya :+ (@! yb :+ yc)))
;;       (von (@! UP :/ DOWN))
;;       )
;;     (let* (
;;         (vao (@! van :- von))
;;         (vbo (@! vbn :- von))
;;         (vco (@! vcn :- von))
;;         (ia (@! vao :/ za))
;;         (ib (@! vbo :/ zb))
;;         (ic (@! vco :/ zc))
;;         (iN (@! (@! ia :+ ib) :+ ic))
;;         (pa (* Vp (@! ia :magnitud) (cos (- (@! van :radianes) (@! ia :radianes)))))
;;         (pb (* Vp (@! ib :magnitud) (cos (- (@! vbn :radianes) (@! ib :radianes)))))
;;         (pc (* Vp (@! ic :magnitud) (cos (- (@! vcn :radianes) (@! ic :radianes)))))
;;         (pt (+ pa pb pc))
;;         (sa (@! (@! vao :* vao) :/ za))
;;         (sb (@! (@! vbo :* vbo) :/ zb))
;;         (sc (@! (@! vco :* vco) :/ zc))
;;         (st (@! (@! sa :+ sb) :/ sc))
;;         )
;;       (concat
;;        (format "Von: %s<%s\n" (@! von :magnitud) (@! von :grados))
;;        (format "Vao: %s<%s\n" (@! vao :magnitud) (@! vao :grados))
;;        (format "Vbo: %s<%s\n" (@! vbo :magnitud) (@! vao :grados))
;;        (format "Vco: %s<%s\n" (@! vco :magnitud) (@! vco :grados))
;;        (format "IlinA: %s<%s\n" (@! ia :magnitud) (@! ia :grados))
;;        (format "IlinB: %s<%s\n" (@! ib :magnitud) (@! ib :grados))
;;        (format "IlinC: %s<%s\n" (@! ic :magnitud) (@! ic :grados))
;;        (format "Ineutro: %s<%s\n" (@! iN :magnitud) (@! iN :grados))
;;        (format "Potencia promedio\n")
;;        (format "Pa: %s\n" pa)
;;        (format "Pb: %s\n" pb)
;;        (format "Pc: %s\n" pc)
;;        (format "Pt: %s\n" pt)
;;        (format "Potencia aparente\n")
;;        (format "Sa: %s<%s\n" (@! sa :magnitud) (@! sa :grados))
;;        (format "Sb: %s<%s\n" (@! sb :magnitud) (@! sb :grados))
;;        (format "Sc: %s<%s\n" (@! sc :magnitud) (@! sc :grados))
;;        (format "St: %s<%s\n" (@! st :magnitud) (@! st :grados))
;;        )
;;       )
;;     )
;;   )

;; Comprobación pag. 481
;; (let* (
;;     (Vp (/ 100 (sqrt 3)))
;;     (za (@! @fasor :new 0 -10 'xy))
;;     (zb (@! @fasor :new 0 10 'xy))
;;     (zc (@! @fasor :new 10 0 'xy))
;;     (van (@! @fasor :new Vp (grad2rad -30)))
;;     (vbn (@! @fasor :new Vp (grad2rad -150)))
;;     (vcn (@! @fasor :new Vp (grad2rad -270)))
;;     (ya (@! za :inverso))
;;     (yb (@! zb :inverso))
;;     (yc (@! zc :inverso))
;;     )
;;   (let* (
;;       (t1 (@! ya :* van))
;;       (t2 (@! yb :* vbn))
;;       (t3 (@! yc :* vcn))
;;       (num (@! (@! t1 :+ t2) :+ t3))
;;       (den (@! ya :+ (@! yb :+ yc)))
;;       (von (@! num :/ den))
;;       )
;;     (let* (
;;         (vao (@! van :- von))
;;         (vbo (@! vbn :- von))
;;         (vco (@! vcn :- von))
;;         (ia (@! vao :/ za))
;;         (ib (@! vbo :/ zb))
;;         (ic (@! vco :/ zc))
;;         (pa (* Vp (@! ia :magnitud) (cos (- (@! van :radianes) (@! ia :radianes)))))
;;         (pb (* Vp (@! ib :magnitud) (cos (- (@! vbn :radianes) (@! ib :radianes)))))
;;         (pc (* Vp (@! ic :magnitud) (cos (- (@! vcn :radianes) (@! ic :radianes)))))
;;         (pt (+ pa pb pc))
;;         )
;;       (concat
;;        (format "VnN: %s<%s\n" (@! von :magnitud) (@! von :grados))
;;        (format "IlinA: %s<%s\n" (@! ia :magnitud) (@! ia :grados))
;;        (format "IlinB: %s<%s\n" (@! ib :magnitud) (@! ib :grados))
;;        (format "IlinC: %s<%s\n" (@! ic :magnitud) (@! ic :grados))
;;        (format "Potencia activa/promedio\n")
;;        (format "Pa: %s\n" pa)
;;        (format "Pb: %s\n" pb)
;;        (format "Pc: %s\n" pc)
;;        (format "Pt: %s\n" pt)
;;        )
;;       )
;;     )
;;   )

;; Ejemplo 13.2 pag 517
;; 13.2, 13.3, 13.4
;; Ejercicio 5
;; (let (
;;       (v24 (@! @fasor :new -80 (grad2rad 120)))
;;       (v45 (@! @fasor :new 60 (grad2rad 75)))
;;       )
;;   (@! v24 :+ v45))


;; (let (
;;       (v12 (@! @fasor :new 9 (grad2rad 87)))
;;       (v23 (@! @fasor :new 8 (grad2rad 45)))
;;       )
;;   (@! v12 :+ v23))


;; (let ((v1 (@! @fasor :new 120 0))
;;       (v2 (@! @fasor :new 120 0))
;;       (z1 (@! @fasor :new 10 0))
;;       (z3 (@! @fasor :new))
;;       )
;;   (@! z3 :x 16)
;;   (@! z3 :y 12)
;;   ;; (@! (@! (@! v1 :/ z1) :- v1) :- v2) ;; I1
;;   ;; (@! (@! (@! v1 :/ z1) :+ (@! @fasor :new 228 (grad2rad 180))) :- (@! v2 :/ z1)) ;; I2
;;   (@! (@! (@! @fasor :new 228 float-pi) :+ (@! @fasor :new 228 float-pi)) :/ (@! (@! z3 :/ z1) :+ (@! @fasor :new 2 0)))
;;   )

;; (let* ((v12 (@! @fasor :new 100 0))
;;       (v24 (@! @fasor :new -80 (grad2rad 120)))
;;       (v45 (@! @fasor :new 60 (grad2rad 75)))
;;       (v25 (@! v24 :+ v45))
;;       (v53 (@! @fasor :new 120 (grad2rad -90))))
;;   (@! (@! v12 :+ v25) :+ v53))


;; (let ((f1 (@! @fasor :new 25 (grad2rad 30)))
;;       (f2 (@! @fasor :new 25 (grad2rad 30))))
;;   (@! f1 :- f2))

;; (let ((f1 (@! @fasor :new 12.7 (grad2rad -30)))
;;       (f2 (@! @fasor :new 10 (grad2rad 30))))
;;   (@! f1 :* f2))

;; (let ((f1 (@! @fasor :new 127 (grad2rad 0)))
;;       (f2 (@! @fasor :new 12.7 (grad2rad -30))))
;;   (@! f1 :/ f2))

;; (let* (
;;        (vab (@! @fasor :new 254 0))
;;        (vbc (@! @fasor :new 127 (grad2rad -120)))
;;        (vca (@! @fasor :new -127 (grad2rad -240)))
;;        (z (@! @fasor :new 12.7 (grad2rad 30)))
;;        (iab (@! vab :/ z))
;;        )
;;   (@! iab :* (@! @fasor :new (sqrt 3) (grad2rad -30)))
;;   )

;; (let* (
;;        (vab (@! @fasor :new 220 0))
;;        (z (@! @fasor :new 12.7 (grad2rad 30)))
;;        )
;;   (@! vab :/ z)
;;   )

;; (let (
;;       (v12 (@! @fasor :new 9 (grad2rad 30)))
;;       (v32 (@! @fasor :new 3 (grad2rad 130)))
;;       )
;;   (@! v12 :- v32))

;; (let (
;;       (v32 (@! @fasor :new 3 (grad2rad 120)))
;;       (v12 (@! @fasor :new 9 (grad2rad 30)))
;;       (v14 (@! @fasor :new 2 (grad2rad 10)))
;;       )
;;   (@! v32 :+ (@! v14 :- v12)))

;; (let (
;;       (v14 (@! @fasor :new 2 (grad2rad 10)))
;;       (v21 (@! @fasor :new -9 (grad2rad 30)))
;;       )
;;   (@! v21 :+ v14))


;; (let (
;;       (v14 (@! @fasor :new 9 -1 'xy))
;;       (v24 (@! @fasor :new 3 3 'xy))
;;       (v34 (@! @fasor :new 8 0 'xy))
;;       )
;;   (@! v14 :- v24)
;;   (@! v34 :- v24)
;;   (@! v14 :- v34)
;;   (++ v14 v34)
;;   )

;; (let (
;;       (z1 (@! @fasor :new 50 0))
;;       (z2 (@! @fasor :new 100 45 'xy))
;; ))

;; (let (
;;       (i1 (@! @fasor :new 3.41 1.09 'xy))
;;       (i2 (@! @fasor :new 2.13 0.68 'xy))
;;       (i3 (@! @fasor :new 1.21 1.09 'xy))
;;       (v1 (@! @fasor :new 110))
;;       (v2 (@! @fasor :new 110))
;;       (v3 (@! @fasor :new 220))
;;       ;; (vt (fasor 1 2 'xy))
;;       ;; (vtt (fasor 3 4 'xy))
;;       )
;;   (@! i1 :* v1)
;;   (@! i2 :* v2)
;;   (@! i3 :* v3)
;;   (@! (@! i2 :* v2):+ (@! (@! i3 :* v3) :+ (@! i1 :* v1)))
;;   ;; (++ vt vtt)
;;   )

;; (let (
;;       (i1 (@! @fasor :new 0.5850 -0.4518 'xy))
;;       (i2 (@! @fasor :new 0.0109 -0.4541 'xy))
;;       (i3 (@! @fasor :new 2.2656 -0.4452 'xy))
;;       (v1 (@! @fasor :new 110))
;;       (v2 (@! @fasor :new 110))
;;       (v3 (@! @fasor :new 220))
;;       (r (@! @fasor :new 1))
;;       ;; (vt (fasor 1 2 'xy))
;;       ;; (vtt (fasor 3 4 'xy))
;;       )
;;   (@! i1 :* v1)
;;   (@! i2 :* v2)
;;   (@! i3 :* v3)
;;   (@! (@! i2 :* v2):+ (@! (@! i3 :* v3) :+ (@! i1 :* v1)))
;;   ;; (++ vt vtt)
;;   (@! r :* (@! i1 :* i1))
;;   (@! r :* (@! i2 :* i2))
;;   )


;; (let ((z0 (@! @fasor :new 1 2 'xy))
;;     (a0 (@! @fasor :new (* 0.5 0.5) 0 'xy)))
;;   ;; (@! z0 :/ a0)
;;   (let* ((zin (@! z0 :/ a0))
;;       (z1 (@! @fasor :new 5 -6 'xy))
;;       (zl (@! z1 :+ zin))
;;       (a1 (@! @fasor :new (* 0.25 0.25) 0 'xy)))
;;     (@! zl :/ a1)
;;     (let ((z2 (@! zl :/ a1))
;;         (r (@! @fasor :new 12)))
;;       (@! z2 :+ r)
;;       (let ((z3 (@! z2 :+ r))
;;           (v (@! @fasor :new 10)))
;;         (@! v :/ z3)
;;         )
;;       )
;;     ;; (@! z1 :+ zin)
;;     ))

(provide 'fasor)
;;; fasor.el ends here
