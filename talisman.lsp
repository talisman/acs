;-----------------------------------------------------
(setq _acad (vlax-get-acad-object))
(setq _doc (vla-get-activedocument _acad))
(defun obDt (object)(vlax-dump-object  object t))
;-----------------------------------------------------
(defun _dxf(ent code)(cdr(assoc code (entget ent))))
;-----------------------------------------------------
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
; selection set to list of objects
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
;-----------------------------------------------------
(defun C:TabSort (/ cnt doc lay)
(vl-load-com)
  (setq cnt 1 doc (vla-get-activedocument (vlax-get-acad-object)) )
    (foreach lay (acad_strlsort (vl-remove "Model" (layoutlist)))
    (vla-put-taborder (vla-item (vla-get-layouts doc) lay) cnt)
    (setq cnt (1+ cnt))
    )
    (princ)
)
;-----------------------------------------------------
(defun-q get-xref-defs(/ blk xrefs)
  (vlax-for blk (vla-get-blocks _doc)
    (if (= :vlax-true (vla-get-isXref blk))
      (setq xrefs (cons (vla-get-name blk) xrefs))
      )
    )
    xrefs
  )
;-----------------------------------------------------
; Set attribute value
(defun-q att-set-value (blockent tag value / att-obj-list )
	(setq att-obj-list  (vlax-safearray->list (vlax-variant-value (vla-getattributes (e->obj blockent)))))
		(foreach n att-obj-list
				(if (= tag (vlax-get-property n "TagString"))
						(vlax-put-property n "TextString" value)
						)
			)
)
;-----------------------------------------------------
; Get attribute value
(defun-q att-get-value (blockent tag  / att-obj-list value)
	(setq att-obj-list  (vlax-safearray->list (vlax-variant-value (vla-getattributes (e->obj blockent)))))
		(foreach n att-obj-list
				(if (= tag (vlax-get-property n "TagString"))
						(setq value (vlax-get-property n "TextString" ))
						)
				)
        value
)
;-----------------------------------------------------
; Rooms polylines , areas and tags
          (defun c:room-link ( / )
            (setq pline (e->obj (car (entsel "\nSelect Polyline"))))
            (setq RoomBlock(e->obj  (car (entsel "\nSelect Block"))))
            (att-set-value RoomBlock "PLINK" (vla-get-handle pline))
            (att-set-value RoomBlock "ROOM-AREA" (rtos (* 0.0001 (vla-get-area pline)) 2 1))
          )
          ;-----------------------------------------------------
          (defun c:rl ()(c:room-link))
          ;-----------------------------------------------------
          (defun RoomCalc (bent / pline)
            (setq pline (e->obj (handent (att-get-value bent "PLINK"))))
            (att-set-value bent "ROOM-AREA"  (rtos (* 0.0001 (vla-get-area pline)) 2 1 ))
          )
          ;-----------------------------------------------------
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
;-----------------------------------------------------
