(defun c:kz (/ code d data dcl_re dclname en ent f gr i ii iii iiii inf 
             key keylst keylst2 kzsj loop lst1 lst2 lw n name nent 
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
  (defun wrxdata (name lst / lst1 x)  ; 写入扩展数据
    (dedata name)
    (setq lst2 '())
    (foreach y lst 
      (setq lst1 '())
      (foreach x y 
        (cond 
          ((= 1000 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1001 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1002 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1003 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1004 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1005 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (get_tile x))))))
          ((= 1010 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1020 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1030 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1011 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1021 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1031 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1012 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1022 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1032 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1013 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1023 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1033 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (strtolst (get_tile x)))))))
          ((= 1040 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (atof (get_tile x)))))))
          ((= 1041 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (atof (get_tile x)))))))
          ((= 1042 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (atof (get_tile x)))))))
          ((= 1070 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (atoi (get_tile x)))))))
          ((= 1071 (setq zuma (atoi (get_attr x "label")))) (setq lst1 (append lst1 (list (cons zuma (atoi (get_tile x)))))))
          ((wcmatch (setq zuma (get_attr x "label")) "程序名") (setq lst1 (cons (get_tile x) lst1)))
        )
      )
      (setq lst2 (append lst2 (list lst1)))
    )
    ;(regapp "data1")
    (entmod 
      (append 
        (entget name)
        (list (cons -3 lst2))
      )
    )
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
           (if (setq kzsj (cdr (assoc -3 (entget ent '("*"))))) 
             (progn 
               (setq dclname (vl-filename-mktemp "yx.dcl")
                     f       (open dclname "w")
               )
               (write-line "yx1:dialog {" f)
               (write-line "label = 扩展数据 ; " f)
               (write-line ":row {" f)
               (write-line ":column {" f)
               (setq ii   0
                     iiii 0
               )
               (foreach y kzsj 
                 (foreach x y 
                   (setq iiii (1+ iiii))
                 )
               )
               (if (> iiii 15) (progn (setq iii (1+ (/ iiii 15))) (setq iii (1+ (/ iiii iii)))) (setq iii 15))
               (setq keylst2 '())
               (foreach y kzsj 
                 (setq keylst '())
                 (foreach x y 
                   (setq ii (1+ ii))
                   (cond 
                     ((listp x)
                      (cond 
                        ((= (car x) 1000) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1001) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1002) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1003) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1004) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1005) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (cdr x) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1010) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1020) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1030) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1011) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1021) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1031) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1012) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1022) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1032) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1013) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1023) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1033) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (lsttostr (cdr x)) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1040) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (rtos (cdr x) 2 3) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1041) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (rtos (cdr x) 2 3) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1042) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (rtos (cdr x) 2 3) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1070) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (rtos (cdr x) 2 0) "\";}") f) (setq keylst (append keylst (list key))))
                        ((= (car x) 1071) (write-line (strcat "    :edit_box { label =\"" (rtos (car x) 2 0) "\"; key = \"" (setq key (strcat (rtos (car x) 2 0) (itoa ii))) "\" ;value=\"" (rtos (cdr x) 2 0) "\";}") f) (setq keylst (append keylst (list key))))
                      )
                     )
                     ((= (type x) 'STR) (write-line (strcat "    :edit_box { label =\"程序名\"" "; key = \"" (setq key (strcat "key" (itoa ii))) "\" ;value=\"" x "\";}") f) (setq keylst (cons key keylst)))
                   )
                   (if (= (rem ii iii) 0) (progn (write-line " }" f) (write-line ":column {" f)))
                 )
                 (setq keylst2 (append keylst2 (list keylst)))
               )
               (write-line " }" f)
               (write-line " }" f)
               (write-line "    :row { :button { key = \"e01\" ; label = \"确认\" ;  is_default = true ;   }" f)
               (write-line "           :button { key = \"e02\" ; label = \"取消\" ; is_cancel = true ; } } }" f)
               (close f)
               (setq dcl_re (load_dialog dclname))
               (new_dialog "yx1" dcl_re)
               (setq i 0)
               (action_tile "e01" "(wrxdata ent  keylst2)(done_dialog 1)")
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
           (setq kzsj (cdr (assoc -3 (entget ent '("*")))))
         )
         (progn 
           (setq ii   0
                 iiii 0
           )
           (setq keylst2 '())
           (foreach y kzsj 
             (setq keylst '())
             (foreach x y 
               (setq ii (1+ ii))
               (cond 
                 ((listp x)
                  (cond 
                    ((= (car x) 1000) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1001) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1002) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1003) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1004) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1005) (setq key (strcat (rtos (car x) 2 0) "      " (cdr x))) (setq keylst (cons key keylst)))
                    ((= (car x) 1010) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1020) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1030) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1011) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1021) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1031) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1012) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1022) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1032) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1013) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1023) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1033) (setq key (strcat (rtos (car x) 2 0) "      " (lsttostr (cdr x)))) (setq keylst (cons key keylst)))
                    ((= (car x) 1040) (setq key (strcat (rtos (car x) 2 0) "      " (rtos (cdr x) 2 3))) (setq keylst (cons key keylst)))
                    ((= (car x) 1041) (setq key (strcat (rtos (car x) 2 0) "      " (rtos (cdr x) 2 3))) (setq keylst (cons key keylst)))
                    ((= (car x) 1042) (setq key (strcat (rtos (car x) 2 0) "      " (rtos (cdr x) 2 3))) (setq keylst (cons key keylst)))
                    ((= (car x) 1070) (setq key (strcat (rtos (car x) 2 0) "      " (rtos (cdr x) 2 0))) (setq keylst (cons key keylst)))
                    ((= (car x) 1071) (setq key (strcat (rtos (car x) 2 0) "      " (rtos (cdr x) 2 0))) (setq keylst (cons key keylst)))
                  )
                 )
                 ((= (type x) 'STR) (setq keylst (cons x keylst)))
               )
               ;(if (= (rem ii iii) 0) (progn (write-line " }"f)(write-line ":column {"f)))
             )
             (setq keylst2 (cons (reverse keylst) keylst2))
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
               (foreach y (reverse keylst2) 
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