;;___________________________
(setq _acad (vlax-get-acad-object))
(setq _doc (vla-get-activedocument _acad))
;;___________________________
(defun _dxf(ent code)(cdr(assoc code (entget ent))))
;;
(defun e->obj (e / eo)
  (if (=
        (type e)
        (type (entlast))
      )
      (setq eo (vlax-ename->vla-object e))
        (if (= (type e) (type (vlax-ename->vla-object (entlast ))))
          (setq eo e )
          )
        )
    eo
)
;-----------------------------------------------------
(defun-q sset->objects (sset /  i objs)
  (setq objs '())
  (setq i 0 )
  (if (and sset  (> (sslength sset) 0))
    (while (< i (sslength  sset))
        (setq objs
            (cons (e->obj (ssname sset i)) objs) i (1+ i))
      )
    )
    objs
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
(defun c:e1467 ()
  (setvar "texteval" 1)
  (setq fldr  "c:\\7777\\ProjectsWork_Revit2017\\1467\\")
  (setq clines '())
  (setq lines (ssget) i 0 )
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
  (setq csvfile (open (strcat folder "reflines.csv") "w"))
  (foreach line clines
    (write-line (strcat
                    (rtos (nth 0 (car line)) 2 8 ) ","
                    (rtos (nth 1 (car line)) 2 8 ) ","
                    (rtos (nth 2 (car line)) 2 8 ) ","
                    (rtos (nth 0 (cadr line)) 2 8 ) ","
                    (rtos (nth 1 (cadr line)) 2 8 ) ","
                    (rtos (nth 2 (cadr line)) 2 8 ) 

      ) csvfile)
  )
  (close csvfile)

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
;-----------------------------------------------------
(defun c:dbld ( / sset)
  (setq sset (ssget))
  (if (= (sslength sset) 2)
    (if (< (-  (_dxf (ssname sset 1 ) 42)(_dxf (ssname sset 0) 42)) 0.0001)
              (entdel (ssname sset 0))
              )
      )
)
;-----------------------------------------------------

;-----------------------------------------------------


;;
(defun c:L* ()
(command "-layer" "t" "*" "u" "*" "on" "*" "")
)
;-----------------------------------------------------
(defun c:l0 ()
  (command "-layer" "u" "0" "t" "0" "on" "0" "s" "0" "")
)
;-----------------------------------------------------
(defun c:xlh ()(xl+ "h"))
(defun c:xlv ()(xl+ "v"))
;-----------------------------------------------------
(defun xl+ (vh)
(command "xline" vh (getpoint))
)
;-----------------------------------------------------
(defun mri-layers ( / layers lnames)
  (setq layers (vla-get-layers _doc))
  (for vlax-for i (setq lnames (cons (vla-get-name i ) lnames)))
  (foreach x lanmes
    (if  (x wcmatch "*$P$*")
      (setq cnames (substr x 12))
    )
    )
    )
  ;(setq layers_to_change)
;-----------------------------------------------------
(defun c:qa ( / pline astr pt )
  (setq pline (e->obj  (car(entsel "\nSelect Polyline"))))
  (setq astr (rtos (* 0.0001 (vla-get-Area pline)) 2 1))
  (setq pt (getpoint))
  (command "-text"  pt 25 0.0 astr)
)
;-----------------------------------------------------
(defun-q c:sheetname()
  (command "-layout" "r" (car (layoutlist)) (vl-filename-base (getvar "dwgname")))
)
;-----------------------------------------------------
(defun c:bqa ( / pline astr blk )
  (setq pline (e->obj  (car(entsel "\nSelect Polyline"))))
  (setq astr (rtos (* 0.0001 (vla-get-Area pline)) 2 1))
)
;-----------------------------------------------------
(defun-q calcMid (ll ur)
  (polar ll (angle ll ur)(/ (distance ll ur) 2.0) )
)


(defun-q C:DA3 ( / box mpt detNum)
  (setq box (e->obj (car (Entsel "\NSelect A3 Detail Border"))))
  (vla-GetBoundingBox box 'll 'ur)
  (setq mPt (calcMid (vlax-safearray->list ll) (vlax-safearray->list ur)))
  (setq DetNum (getString "\Enter Detail number"))
  (DA3-CL DetNum)
  (command "mspace")
  (command "zoom" "c" mpt "")
  (command "zoom" "1xp")
  (command "pspace")
  (command "mview" "Lock" "on" "ALL" "")
  (command "-layout" "s" "model")
)

(defun-q DA3-CL (name /)
  (command "-layout" "C" "3687-P9-P" (strcat "3687-P9-P-" name))
  (command "-layout" "S" (strcat "3687-P9-P-" name))
  (command "mview" "0,0" "420,297")
)



(defun C:TabSort (/ cnt doc lay)
(vl-load-com)
  (setq cnt 1 doc (vla-get-activedocument (vlax-get-acad-object)) )
    (foreach lay (acad_strlsort (vl-remove "Model" (layoutlist)))
    (vla-put-taborder (vla-item (vla-get-layouts doc) lay) cnt)
    (setq cnt (1+ cnt))
    )
    (princ)
)

(defun-q get-xref-defs(/ blk xrefs)
  (vlax-for blk (vla-get-blocks _doc)
    (if (= :vlax-true (vla-get-isXref blk))
      (setq xrefs (cons (vla-get-name blk) xrefs))
      )
    )
    xrefs
  )

;|

(defun c:x3687 ()
  (setq xrefs)

)
|;


(defun p3687-path-fix (path)
  (vl-string-subst "T:\\icilov\\3687-ofer-7-10\\"  "T:\\icilov\\3687\\"  path)
)


(defun-q att-set-value (blockent tag value / att-obj-list )
	(setq att-obj-list  (vlax-safearray->list (vlax-variant-value (vla-getattributes (e->obj blockent)))))
		(foreach n att-obj-list
				(if (= tag (vlax-get-property n "TagString"))
						(vlax-put-property n "TextString" value)
						)
				)
)


(defun-q att-get-value (blockent tag  / att-obj-list value)
	(setq att-obj-list  (vlax-safearray->list (vlax-variant-value (vla-getattributes (e->obj blockent)))))
		(foreach n att-obj-list
				(if (= tag (vlax-get-property n "TagString"))
						(setq value (vlax-get-property n "TextString" ))
						)
				)
        value
)

(defun c:room-link ( / )
  (setq pline (e->obj (car (entsel "\nSelect Polyline"))))
  (setq RoomBlock(e->obj  (car (entsel "\nSelect Block"))))
  (att-set-value RoomBlock "PLINK" (vla-get-handle pline))
  (att-set-value RoomBlock "ROOM-AREA" (rtos (* 0.0001 (vla-get-area pline)) 2 1))

)
(defun c:rl ()(c:room-link))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RoomCalc (bent / pline)
  (setq pline (e->obj (handent (att-get-value bent "PLINK"))))
  (att-set-value bent "ROOM-AREA"  (rtos (* 0.0001 (vla-get-area pline)) 2 1 ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:RoomAuto ( / blks )
  (setq i 0 blks (ssget "x" (list (cons 0 "insert")(cons 2 "B-ROOMN"))))
  (while (< i (sslength blks))
    (progn
      (RoomCalc (e->obj (ssname blks i)))
      (setq i (1+ i))
      )
    )
)

(defun c:rau ()(c:RoomAuto))
