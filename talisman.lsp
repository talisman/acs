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
(defun c:qb ()
  (setq fldr  "c:\\users\\jonathant\\Documents\\1467\\")
  (setq exlevels (vl-directory-files fldr "LEVEL??.dwg" 0))
  (setq nextlevel (length exlevels ))
  (setq nfile (strcat fldr "LEVEL" (itoa nextlevel)))
  (setq bpt (getpoint "\nPick insertion point"))
  (princ "\nSelect elements to export")
  (setq elements (ssget "x"))
  (command "-wblock" nfile bpt elements)

)
;-----------------------------------------------------

(defun c:qwb ( /  num sset ipoint)
  (princ "\nSelect objects")
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
