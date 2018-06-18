(defun c:FB1467 () (FloorBlocks1467)  / fldr blocks blockNames x b )
(defun FloorBlocks1467 ( / )
  (setvar "texteval" 1)
  (setq fldr  "C:/7777/ProjectsWork_Revit2017/1467/M145/FloorBlocks")
  (setq blocks (vla-get-blocks _doc))
  (setq blockNames '())
  (vlax-for b blocks
    (Setq blockNames (cons (vla-get-Name b) blockNames ))
    )
  (setq blockNames (vl-remove-if-not '(lambda (a)(snvalid a)) blocknames))
  (foreach x blockNames
    (command "wblock" (strcat fldr "/" x ".dwg") "=")
    )
)
;-----------------------------------------------------
(defun c:qb (/ fldr exlevels nextlevel nfile bpt elements)
  (setq fldr  "c:\\7777\\ProjectsWork_Revit2017\\1467\\")
  (setvar "filedia" 0)
  (setvar "texteval" 1)
  (setq exlevels (vl-directory-files fldr "LEVEL-PL-??.dwg" 0))
  (setq nextlevel (length exlevels ))
  (if (< nextlevel 10)
    (setq nextlevel (strcat "0" (itoa nextlevel)))
    (setq nextlevel (itoa nextlevel))
    )
  (setq nfile (strcat fldr "LEVEL-PL-" nextlevel))
  (setq bpt (getpoint "\nPick insertion point"))
  (princ "\nSelect elements to export")
  (setq elements (ssget))
  (command "ucs" "o" bpt)
  (command "ucs" "z" 90)
  (command "-wblock" nfile "" bpt elements "" "" )
  (command "ucs" "w")
  (setvar "filedia" 1)

)
;-----------------------------------------------------

;-----------------------------------------------------
  (defun c:cfc () (createFloorCountour))

  (defun createFloorCountour ( / fldr blks css)
      (setvar "texteval" 1)
      (setq fldr  "C:/7777/ProjectsWork_Revit2017/1467/M145/FloorBlocks")
      (repeat 200 (command "explode" "all" ))
      (setq css (ssget "x" (list (cons 8 "maatefet"))))
      (command
        "wblock"
        (strcat fldr "/wb_"  (vl-filename-base (getvar "dwgname")) "contour.Dwg")
        "" ; Enter
        "0,0" ; ipoint
        css
        ""
      )
  )

;-----------------------------------------------------
(defun c:e1467 ()
  (setvar "texteval" 1)
  (setq fldr  "c:\\7777\\ProjectsWork_Revit2017\\1467\\")
  (setq clines '())
  (setq lines (ssget "x" (list (cons 0 "LINE")(cons 8 "HD")))  i 0 )
  (while
    (< i (sslength lines))
      (setq clines
          (cons
              (list
                  (_dxf (ssname lines i) 10)
                  (_dxf (ssname lines i) 11)
                )
                clines
              )
              i (1+ i)
            )
  )
  (setq csvfile (open (strcat fldr "reflines.csv") "w"))
  (setq csvfile2 (open (strcat fldr "ptss.csv") "w"))
  (foreach line clines
    (progn
      (write-line (strcat
                    (rtos (nth 0 (car line)) 2 8 ) ","
                    (rtos (nth 1 (car line)) 2 8 ) ","
                    (rtos (nth 2 (car line)) 2 8 ) ","
                    (rtos (nth 0 (cadr line)) 2 8 ) ","
                    (rtos (nth 1 (cadr line)) 2 8 ) ","
                    (rtos (nth 2 (cadr line)) 2 8 )

                    ) csvfile)
      (write-line
        (strcat
            (rtos (nth 0 (car line)) 2 8 ) ","
            (rtos (nth 1 (car line)) 2 8 ) ","
            (rtos (nth 2 (car line)) 2 8 )
              )

        csvfile2 )
        (write-line
          (strcat
              (rtos (nth 0 (cadr line)) 2 8 ) ","
              (rtos (nth 1 (cadr line)) 2 8 ) ","
              (rtos (nth 2 (cadr line)) 2 8 )
                )

          csvfile2 )
        )
      )
  (close csvfile)
  (close csvfile2)
)
;-----------------------------------------------------
(defun c:m1467 ( / fldr exlevels x z count)
  (setvar "texteval" 1)
  (setq fldr  "c:\\7777\\ProjectsWork_Revit2017\\1467\\")
  (setq count 0)
  (setq exlevels (acad_strlsort(vl-directory-files fldr "LEVEL-PL-??.dwg" 0)))
  (foreach x exlevels
      (progn
        (setq z (* count 330))
        (command "-insert" (strcat fldr x)(list 0 0 z) 1 1 0)
        (setq count (+  count 1))
      )
    )
)
;-----------------------------------------------------
(defun c:ex-pts-1467 ( / ptsfile blocks circles pts i csvfile blk)
  (setvar "texteval" 1)
  (setq ptsfile (strcat (getvar "dwgprefix")(getvar "dwgname") ".CSV"))
  (setq blocks (ssget "x" (list (cons 0 "insert"))))
  ;(command "explode" blocks "")
  (setq blocks (sset->objects blocks))
  (foreach blk blocks (vla-explode blk))
  (setq circles (ssget "x" (list (cons 0 "circle") (cons 8 "ELV1"))))
  (setq pts '() i 0 )
  (while (< i (sslength circles))
    (setq pts (cons (_dxf (ssname circles i) 10) pts) i (1+ i))
  )
  (setq csvfile (open ptsfile "w"))
  (foreach x pts (write-line (strcat (rtos (car x) 2 8 ) "," (rtos (cadr x) 2 8)"," (rtos (caddr x ) 2 8) ) csvfile))
  (close csvfile)
)
;-----------------------------------------------------

(defun c:qwb ( /  num sset ipoint)
  (princ "\nSelect objs")
  (setq sset (ssget))
  (setq num (_dxf (car (entsel "\Nselect number of block")) 1))
  (setq ipoint (getpoint "\Npick insertion point"))
  (setvar "filedia" 0)
  (command "-wblock" (strcat (getvar "dwgprefix") "lvls\\lvl" num) "" ipoint sset "" "")
  (setvar "filedia" 1)
)
