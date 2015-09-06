;;; analisis.el --- Analisis de circuitos trifasicos
;;; Commentary:
;; Cuando tienes una fuente trifasica en Y conectada a cargas en Y
;; calcula los valores

;;; Code:
(load-file "fasor.el")
(require '@)
(require 'fasor)

(setf @analisis-Y-Y
      (@extend :_Vao :_Vbo :_Vco :_Von
               :_Van :_Vbn :_Vcn
               :_IlinA :_IlinB :_IlinC
               :_Za :_Zb :_Zc
               :_Vlin :_frecuencia :_secuencia))

(def@ @analisis-Y-Y :secuencia (&optional secuencia)
  (when secuencia
    (cond ((string= secuencia "CBA") (progn (setf @:_secuencia "CBA")
                                            (@! @:_Van :radianes 0)
                                            (@! @:_Vbn :radianes 2.09439510239)
                                            (@! @:_Vcn :radianes 4.18879020479)))
          ((string= secuencia "ABC") (progn (setf @:_secuencia "ABC")
                                            (@! @:_Van :radianes 0)
                                            (@! @:_Vbn :radianes -2.09439510239)
                                            (@! @:_Vcn :radianes -4.18879020479)))
          (t nil)))
  @:_secuencia)

(def@ @analisis-Y-Y :Van ()
  @:_Van)

(def@ @analisis-Y-Y :Vbn ()
  @:_Vbn)

(def@ @analisis-Y-Y :Vcn ()
  @:_Vcn)

(def@ @analisis-Y-Y :Vlin (&optional Vlin)
  (when Vlin
    (setf @:_Vlin Vlin)
    (let ((V (/ Vlin (sqrt 3))))
      (progn (@! @:_Van :magnitud V)
             (@! @:_Vbn :magnitud V)
             (@! @:_Vcn :magnitud V))))
  @:_Vlin)

(def@ @analisis-Y-Y :init ()
  (@^:init)
  (setf @:_frecuencia 1)
  (setf @:_Vlin 0)
  (setf @:_Van (@! @fasor :new))
  (setf @:_Vbn (@! @fasor :new))
  (setf @:_Vcn (@! @fasor :new))
  (@:secuencia "ABC"))

;; (setf ejemplo1 (@! @analisis-Y-Y :new))
;; (@! ejemplo1 :Van)
;; (@! ejemplo1 :Vbn)
;; (@! ejemplo1 :Vcn)
;; (@! ejemplo1 :secuencia "CBA")
;; (@! ejemplo1 :Vlin 220)
;; (@! ejemplo1 :Vlin)
;; (@! (@ ejemplo1 :_Van) :magnitud)

(provide 'analisis)
;;; analisis.el ends here
