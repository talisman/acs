; 1286 lisp routines
;

(defun c:r2f ( / )
  (att-feed
      (car (entsel "\nSelect source [Room] block"))
      (car (entsel "\nSelect destination [Finish] block"))
      (list (list "ROOM_NAME1"  "ROOM-NAME-1")(list "ROOM_NAME2"  "ROOM-NAME-2")(list "ROOM_NO."  "ROOM-ID"))
  )
)
;-----------------------------------------------------
(defun C:NEM ( / blks i)
  (princ "Select blocks to set as windows - no Ectrl or motor")
  (setq blks (ssget) i 0)
  (while (< i (sslength blks))
    (progn
        (att-set-value (ssname blks i) "E-CTRL" "NO")
        (att-set-value (ssname blks i) "MOTORIZED" "NO")
        (setq i (1+ i))
      )
    )
  )
;-----------------------------------------------------
(defun C:ECT ( / blks i)
    (princ "Select blocks to set as windows - no Ectrl or motor")
    (setq blks (ssget) i 0)
    (while (< i (sslength blks))
      (progn
          (att-set-value (ssname blks i) "E-CTRL" "YES")
          (setq i (1+ i))
        )
      )
    )
;-----------------------------------------------------
(defun c:export-doors ( / blks i csvfile e row )
  (setq blks (ssget "x" (list (cons 0 "INSERT")(cons 2 "MASGERUT2,NAGARUT2,ALUMINIUM2"))))
  (setq i 0)
  (setq csvfile (open (strcat (getvar "dwgprefix") (vl-filename-base (getvar "dwgname")) "doors.csv") "w"))
  (while (< i (sslength blks))
    (progn
      (setq e (ssname blks i))

      (setq row (list (strcat "''" (_dxf e 5) "''")))
      (setq row (cons (_dxf e 2) row))
      (setq row (cons (_dxf e 8) row))


      (foreach x (list "NUMBER" "ROOM" "Phase" "E-CTRL" "MOTORIZED")
        (setq row (cons (att-get-value e x) row ))
      )
      (setq row (apply 'strcat (mapcar '(lambda(X)(strcat x ",")) (reverse row))))
      (write-line row csvfile)
      (setq i (1+ i))
    )
  )
  (close csvfile)
)
;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;
;(defun c:qrcc () (qrc 683 290 1 4))
;(qrc 683 290 1 4)
;(qrc 683 290 2 8)

(defun qrc (width height cirsize spacing / row rows _matrix lowerLeft rowData drow ypos tmp)
  (setq LowerLeft (getpoint "\nSelect Lower Left corner"))
  (setq pt1 (polar lowerLeft (dtor 0) width))
  (setq pt2 (polar pt1 (dtor 90) height))
  (setq pt3 (polar pt2 (dtor 180) width))
  (command "pline" LowerLeft pt1 pt2 pt3 "close")

  (setq rows 71 row 0 _matrix '())
  (while (< row rows )
    (progn
      (setq hmany (intRand 0 60) i 0 rowData '())
      (while (< i hmany)
      (progn
        (setq tmp (intRand 0 169))
          (while (member tmp rowData)
              (setq tmp (intRand 0 169))
              )
          (setq rowData (cons tmp rowData) i(1+ i))
          )
        )
      (setq _matrix (cons rowData _matrix))
      (setq row (1+ row))
    )
  )
  (setq ypos (+ spacing  (cadr lowerleft)))
  (setq xpos (+ spacing (car lowerleft)))
  (foreach drow _matrix
    (progn
        (foreach x drow (command "circle" (list (+ xpos(* spacing x )) ypos) cirsize))
        (setq ypos (+ ypos spacing))
    )
  )
)

;;; based on Lee Macs' work
(defun num_rand ( / a c m )
    (setq m   4294967296.0
          a   1664525.0
          c   1013904223.0
          xn (rem (+ c (* a (cond (xn) ((getvar 'date))))) m)
    )
    (/ xn m)
)

(defun intRand ( a b )
    (+ (min a b) (fix (* (num_rand) (1+ (abs (- a b))))))
)
;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:CreateExcel(/ acwb caminho cel excel-app ofile o_fil path sht shts wb wb-collection x)
  (vl-load-com)
  (setq excel-app (vlax-create-object "excel.application"))
  (vlax-put-property excel-app 'Visible :vlax-true)
  (setq wb-collection (vlax-get excel-app "workbooks"))
  (setq wb (vlax-invoke-method wb-collection 'Add))
  (setq sht (vlax-invoke-method (vlax-get-property excel-app 'Worksheets) 'Add))
  (vlax-put-property sht 'Name "NewSheet")
  (setq acwb (vlax-get-property excel-app 'Activeworkbook))
  (setq shts (vlax-get-property acwb 'Sheets))
  (setq cel (vlax-get-property sht 'Range "A1"))
  (vlax-put cel 'Value "Rogerio_Brasil")
  (setq caminho (getvar "dwgprefix"))
  (setq ofile (strcat caminho "teste1.xls")); output file

  (vlax-invoke-method wb 'SaveAs ofile -4143 nil nil :vlax-false :vlax-false 1 2 )
  (vl-catch-all-apply 'vlax-invoke-method (list wb "Close" ))
  (vl-catch-all-apply 'vlax-invoke-method (list excel-app "Quit") )
  (mapcar
    (function (lambda (x)
      (vl-catch-all-apply (function (lambda()
        (progn
          (if (not (vlax-object-released-p x))
          (vlax-release-object x)
          )
          (setq x nil)
          )
          )))))
          (list cel sht shts wb wb-collection excel-app)
          )
)
