(defun c:xv (/ code d apdata dcl_re dclname en ent f gr i cnt eachCol total inf
             thiskey appKeyList allKeyList xDatas loop lst1 lst2 lw n name nent
             oldent pd pt ptlst ss str str1 txlst w ww x y zuma
            )  ;<扩展数据编辑>
  (defun *error* (inf)  ; 出错处理
    (setq inf (strcase inf t))
    (if (wcmatch inf "*break*,*cancel*,*exit*,*取消*,*中断*") 
      (deltx txlst)
    )
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  )
  (defun jspt (pt w ww)  ; pt相对坐标计算
    (list (+ (car pt) w) (+ (cadr pt) ww))
  )
  (defun dxf (ent n)  ; 取得图元内容
    (if (= (type ent) 'ename) 
      (setq ent (entget ent))
    )
    (cdr (assoc n ent))
  )
  (defun emod (ent i n)  ; 替换图元内容
    (subst 
      (cons i n)
      (assoc i ent)
      ent
    )
  )
  (defun deltx (txlst / en i x)  ; 删除显示的txlst='(ename0 ename1 ...)
    (foreach x txlst 
      (entdel x)
    )
    (setq w 0.0)
    (if lw 
      (progn 
        (setq en (entget lw)
              en (reent en (list '(0.0 0.0) '(0.0 0.0)))
        )
        (entmod (emod en 43 0.0))
      )
    )
    (if oldent 
      (redraw oldent 4)
    )
  )
  (defun wrxdata (name allKeyList / partXDList newXDList x)  ; 写入扩展数据
    (dedata name)
    (setq newXDList '())
    (foreach appKeyList allKeyList 
      (setq partXDList '())
      (foreach key appKeyList 
        (setq zuma   (get_attr key "label")
              value  (get_tile key)
              apdata nil
        )
        (cond 
          ((wcmatch zuma "程序名") (setq partXDList (cons value partXDList)))
          ((= "1000" zuma) (setq apdata value))
          ((= "1001" zuma) (setq apdata value))
          ((= "1002" zuma) (setq apdata value))
          ((= "1003" zuma) (setq apdata value))
          ((= "1004" zuma) (setq apdata value))
          ((= "1005" zuma) (setq apdata value))
          ((= "1010" zuma) (setq apdata (strtolst value)))
          ((= "1020" zuma) (setq apdata (strtolst value)))
          ((= "1030" zuma) (setq apdata (strtolst value)))
          ((= "1011" zuma) (setq apdata (strtolst value)))
          ((= "1021" zuma) (setq apdata (strtolst value)))
          ((= "1031" zuma) (setq apdata (strtolst value)))
          ((= "1012" zuma) (setq apdata (strtolst value)))
          ((= "1022" zuma) (setq apdata (strtolst value)))
          ((= "1032" zuma) (setq apdata (strtolst value)))
          ((= "1013" zuma) (setq apdata (strtolst value)))
          ((= "1023" zuma) (setq apdata (strtolst value)))
          ((= "1033" zuma) (setq apdata (strtolst value)))
          ((= "1040" zuma) (setq apdata (atof value)))
          ((= "1041" zuma) (setq apdata (atof value)))
          ((= "1042" zuma) (setq apdata (atof value)))
          ((= "1070" zuma) (setq apdata (atoi value)))
          ((= "1071" zuma) (setq apdata (atoi value)))
        )
        (if apdata (setq partXDList (append partXDList (list (cons (atoi zuma) apdata)))))
        ; (prin1 partXDList)
      )
      (setq newXDList (append newXDList (list partXDList)))
    )
    ; (prin1 newXDList)(princ "\n")
    ; (prin1 (entmod
    (entmod 
      (append 
        (entget name)
        (list (cons -3 newXDList))
      )
    )
    ; )
  )
  (defun dedata (name)  ; 删除扩展数据
    (entmod 
      (list (cons -1 name) 
            (cons -3 
                  (mapcar 
                    'list
                    (mapcar 
                      'car
                      (cdr (assoc -3 (entget name '("*"))))
                    )
                  )
            )
      )
    )
  )
  (defun lsttostr (lst / n str)  ; 表转字符串
    (setq str "")
    (foreach n lst 
      (setq str (strcat str (if (= (type n) 'STR) n (rtos n 2 3)) " "))
    )
    str
  )
  (defun strtolst (str / i lst str1)  ; 字符串转表
    (setq lst '()
          i   1
    )
    (while (/= str "") 
      (if (= (substr str i 1) " ") 
        (setq str1 (substr str 1 (1- i))
              lst  (cons (atof str1) lst)
              str  (substr str (1+ i))
              i    1
        )
        (setq i (1+ i))
      )
    )
    (reverse lst)
  )
  (defun reent (ent ptlst / i nent x)  ; 按点表顺序更新多段线顶点,无须更换的顶点用nil代替。by:langjs
    (setq i    -1
          nent '()
    )
    (foreach x ent 
      (setq nent (append 
                   nent
                   (list 
                     (if 
                       (and 
                         (= (car x) 10)
                         (/= 
                           (nth (setq i (1+ i)) 
                                ptlst
                           )
                           nil
                         )
                       )
                       (cons 10 (nth i ptlst))
                       x
                     )
                   )
                 )
      )
    )
  )
  (setvar "cmdecho" 0) ; 关闭命令响应
  (vl-load-com)
  ;(prompt "\n    鼠标移动查询信息，左键编辑，右键退出！")
  (setq loop  t
        i     0
        txlst '()
  )
  (entmake 
    (list '(0 . "LWPOLYLINE") 
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 62 186)
          (cons 90 2)
          (cons 10 '(0.0 0.0))
          (cons 10 '(0.0 0.0))
    )
  )
  (setq lw (entlast))
  (command "DRAWORDER" lw "" "B")
  (while loop 
    (setq gr   (grread t 4 2)
          code (car gr)
          pt   (cadr gr)
          pd   nil
    )
    (cond 
      ((= code 3) ; 鼠标左击
       ;(deltx txlst)
       (if ent 
         (progn 
           (if (setq xDatas (cdr (assoc -3 (entget ent '("*"))))) 
             (progn 
               (setq dclname (vl-filename-mktemp "yx.dcl")
                     f       (open dclname "w")
               )
               (write-line "yx1:dialog {" f)
               (write-line "label = 扩展数据 ; " f)
               (write-line ":row {" f)
               (write-line ":column {" f)
               (setq cnt   0
                     total 0
               )
               (foreach appDatas xDatas 
                 (foreach data appDatas 
                   (setq total (1+ total))
                 )
               )
               (if (> total 15) (progn (setq eachCol (1+ (/ total 15))) (setq eachCol (1+ (/ total eachCol)))) (setq eachCol 15))
               (setq allKeyList '())
               (foreach appDatas xDatas 
                 (setq appKeyList '())
                 (foreach data appDatas 
                   (setq cnt    (1+ cnt)
                         cntStr (itoa cnt)
                         rflag  nil
                   )
                   (cond 
                     ((= (type data) 'STR) (write-line (strcat "    :text { label =\"程序名\"" "; key = \"" cntStr "\"; value=\"" data "\";}") f) (setq rflag 1))
                     ((listp data)
                      (setq zuma    (car data)
                            zumaStr (itoa zuma)
                            value   (cdr data)
                      )
                      (cond 
                        ((= zuma 1000) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1001) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1002) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1003) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1004) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1005) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" value "\";}") f) (setq rflag 1))
                        ((= zuma 1010) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1020) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1030) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1011) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1021) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1031) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1012) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1022) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1032) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1013) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1023) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1033) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (lsttostr value) "\";}") f) (setq rflag 1))
                        ((= zuma 1040) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (rtos value 2 3) "\";}") f) (setq rflag 1))
                        ((= zuma 1041) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (rtos value 2 3) "\";}") f) (setq rflag 1))
                        ((= zuma 1042) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (rtos value 2 3) "\";}") f) (setq rflag 1))
                        ((= zuma 1070) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (rtos value 2 0) "\";}") f) (setq rflag 1))
                        ((= zuma 1071) (write-line (strcat "    :edit_box { label =\"" zumaStr "\"; key = \"" cntStr "\"; value=\"" (rtos value 2 0) "\";}") f) (setq rflag 1))
                        (T (princ (strcat "Unknown GruopCode" zumaStr)))
                      )
                     )
                   )
                   (if rflag (setq appKeyList (append appKeyList (list cntStr))))
                   (if (= (rem cnt eachCol) 0) (progn (write-line " }" f) (write-line ":column {" f)))
                 )
                 (setq allKeyList (append allKeyList (list appKeyList)))
               )
               (write-line " }" f)
               (write-line " }" f)
               (write-line "    :row { :button { key = \"e01\" ; label = \"确认\" ;  is_default = true ;   }" f)
               (write-line "           :button { key = \"e02\" ; label = \"取消\" ; is_cancel = true ; } } }" f)
               (close f)
               (setq dcl_re (load_dialog dclname))
               (new_dialog "yx1" dcl_re)
               (setq i 0)
               (action_tile "e01" "(wrxdata ent allKeyList)(done_dialog 1)")
               (start_dialog)
               (unload_dialog dcl_re)
               (vl-file-delete dclname)
             )
             (princ "\n没有扩展数据!")
           )
         )
       )
      )
      ((= code 5) ; 鼠标移动
       (redraw)
       (if 
         (and 
           (setq d (* (/ (getvar "viewsize") (cadr (getvar "screensize"))) (getvar "pickbox")))
           (setq ent (nentselp pt)
                 ent (if (and ent (= (type (last (last ent))) 'ename)) 
                       (last (last ent))
                       (car ent)
                     )
           )
           (setq xDatas (cdr (assoc -3 (entget ent '("*")))))
         )
         (progn 
           (setq cnt   0
                 total 0
           )
           (setq allLines '())
           (foreach appDatas xDatas 
             (setq lines '())
             (foreach data appDatas 
               (setq cnt    (1+ cnt)
                     cntStr (itoa cnt)
                     rflag  nil
               )
               (cond 
                 ((listp data)
                  (setq zuma    (car data)
                        zumaStr (itoa zuma)
                        value   (cdr data)
                  )
                  (cond 
                    ((= zuma 1000) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1001) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1002) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1003) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1004) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1005) (setq thisLine (strcat zumaStr "      " value)) (setq rflag 1))
                    ((= zuma 1010) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1020) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1030) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1011) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1021) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1031) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1012) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1022) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1032) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1013) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1023) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1033) (setq thisLine (strcat zumaStr "      " (lsttostr value))) (setq rflag 1))
                    ((= zuma 1040) (setq thisLine (strcat zumaStr "      " (rtos value 2 3))) (setq rflag 1))
                    ((= zuma 1041) (setq thisLine (strcat zumaStr "      " (rtos value 2 3))) (setq rflag 1))
                    ((= zuma 1042) (setq thisLine (strcat zumaStr "      " (rtos value 2 3))) (setq rflag 1))
                    ((= zuma 1070) (setq thisLine (strcat zumaStr "      " (rtos value 2 0))) (setq rflag 1))
                    ((= zuma 1071) (setq thisLine (strcat zumaStr "      " (rtos value 2 0))) (setq rflag 1))
                  )
                 )
                 ((= (type data) 'STR) (setq thisLine data) (setq rflag 1))
               )
               ;(if (= (rem ii iii) 0) (progn (write-line " }"f)(write-line ":column {"f)))
               (if rflag (setq lines (cons thisLine lines)))
             )
             (setq allLines (cons (reverse lines) allLines))
           )
           (setq i 0)
           (setq pt (jspt pt d (* -2 d))
                 i  0
                 w  0.0
           )
           (redraw)
           (redraw ent 3)
           (if (or (and ent (not oldent)) (not (equal oldent ent))) 
             (progn 
               (if oldent 
                 (redraw oldent 4)
               )
               (if txlst 
                 (progn 
                   (while (< 0 (length txlst)) 
                     (entdel (car txlst))
                     (setq txlst (cdr txlst))
                   )
                   (setq oldent nil)
                   (if lw 
                     (progn 
                       (setq en (entget lw)
                             en (reent en (list '(0.0 0.0) '(0.0 0.0)))
                       )
                       (entmod (emod en 43 0.0))
                     )
                   )
                 )
               )
               (foreach y (reverse allLines) 
                 (foreach x y 
                   (entmake 
                     (list '(0 . "TEXT") 
                           (cons 10 (jspt pt 0 (* -1.5 d i)))
                           (cons 62 6)
                           (cons 40 d)
                           (cons 1 x)
                           '(41 . 0.9)
                     )
                   )
                   (setq en (entlast))
                   ;(command "DRAWORDER" en "" "F")
                   (setq txlst (cons en txlst))
                   (if (> (car (cadr (textbox (entget en)))) w) 
                     (setq w (car (cadr (textbox (entget en)))))
                   )
                   (setq i (1+ i))
                 )
               )
             )
             (if txlst 
               (progn 
                 (foreach tx (reverse txlst) 
                   (setq en (entget tx)
                         en (emod en 10 (jspt pt 0 (* -1.5 d i)))
                         en (emod en 40 d)
                   )
                   (entmod en)
                   (if (> (car (cadr (textbox en))) w) 
                     (setq w (car (cadr (textbox en))))
                   )
                   (setq i (1+ i))
                 )
               )
             )
           )
           (setq oldent ent)
           (redraw oldent 4)
           (setq h  (* -0.75 d (length txlst))
                 en (entget lw)
                 en (reent en (list (jspt pt 0 (+ h (* 1.5 d))) (jspt pt (+ (* 0.3 d) w) (+ h (* 1.5 d)))))
                 en (emod en 43 (+ (* -2 h) (* 0.65 d)))
           )
           (entmod en)
           (redraw)
         )
         (if txlst 
           (progn 
             (while (< 0 (length txlst)) 
               (entdel (car txlst))
               (setq txlst (cdr txlst))
             )
             (setq oldent nil)
             (if lw 
               (progn 
                 (setq en (entget lw)
                       en (reent en (list '(0.0 0.0) '(0.0 0.0)))
                 )
                 (entmod (emod en 43 0.0))
               )
             )
           )
         )
       )
      )
      ((or  ; 鼠标右击
           (= code 11)
           (= code 25)
       )
       (deltx txlst)
       (setq loop nil)
      )
      (t)
    )
  )
  (princ)
)