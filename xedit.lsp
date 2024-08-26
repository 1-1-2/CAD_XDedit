(defun c:xvg (/ code d apData dclRe dclName en ent f gr i cnt eachCol total inf thisKey appKeyList allKeyList xDatas loop lst1 lst2 lwplBackground n name nEnt oldEnt pt ptList ss str str1 txtList maxWide ww x y xdTypeCode) 
  ; 编辑实体的扩展数据

  ;; 错误处理
  (defun *error* (inf) 
    (setq inf (strcase inf t))
    (if (wcmatch inf "*break*,*cancel*,*exit*,*取消*,*中断*") 
      (deleteTextObjects txtList)
    )
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  )

  ;; 辅助函数：计算相对坐标
  (defun calculateRelativePoint (pt w ww) 
    (list (+ (car pt) w) (+ (cadr pt) ww))
  )

  ;; 辅助函数：获取实体的指定DXF代码值
  (defun getDxfValue (ent n) 
    (if (= (type ent) 'ename) 
      (setq ent (entget ent))
    )
    (cdr (assoc n ent))
  )

  ;; 辅助函数：修改实体的指定DXF代码值
  (defun modifyEntityDxf (ent i n) 
    (subst 
      (cons i n)
      (assoc i ent)
      ent
    )
  )

  ;; 辅助函数：更新多段线顶点
  (defun updatePolyline (plEnt ptList / i) 
    (setq i -1) ; 初始化计数器
    (mapcar 
      (function 
        (lambda (x) 
          (if (= (car x) 10)  ; 仅处理组码 10
            (progn 
              (setq i (1+ i)) ; 增加索引
              (cons 10 (nth i ptList)) ; 替换为新的顶点坐标
            )
            x ; 其他组码保持不变
          )
        )
      )
      plEnt ; 传入的多段线实体的 DXF 数据列表
    )
  )

  ;; 辅助函数：删除显示的文本对象列表
  (defun deleteTextObjects (txtList / en i x) 
    (foreach x txtList 
      (entdel x)
    )
    (if lwplBackground 
      (progn 
        (setq en (entget lwplBackground)
              en (updatePolyline en (list '(0.0 0.0) '(0.0 0.0)))
        )
        (entmod (modifyEntityDxf en 43 0.0))
      )
    )
  )

  ;; 辅助函数：写入扩展数据
  ;; get_attr (AutoLISP/DCL): https://help.autodesk.com/view/OARX/2025/CHS/?guid=GUID-AB9100B0-F19E-4C29-B82A-28E8A607BF4E
  ;; get_tile (AutoLISP/DCL): https://help.autodesk.com/view/OARX/2025/CHS/?guid=GUID-3BD2F6DF-DCD5-4A00-9BDF-E2C7C7C286F8
  (defun writeXData (name allKeyList / partXDList newXDList x) 
    (setq newXDList '())
    (foreach appKeyList allKeyList 
      (setq partXDList '())
      (foreach key appKeyList 
        (setq xdTypeCode (get_attr key "label")
              xdData     (get_tile key)
              apData     nil
        )
        (cond 
          ((wcmatch xdTypeCode "程序名") (setq partXDList (cons xdData partXDList)))
          ((= "1000" xdTypeCode) (setq apData xdData))
          ((= "1001" xdTypeCode) (setq apData xdData))
          ((= "1002" xdTypeCode) (setq apData xdData))
          ((= "1003" xdTypeCode) (setq apData xdData))
          ((= "1004" xdTypeCode) (setq apData xdData))
          ((= "1005" xdTypeCode) (setq apData xdData))
          ((= "1010" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1020" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1030" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1011" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1021" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1031" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1012" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1022" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1032" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1013" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1023" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1033" xdTypeCode) (setq apData (stringToList xdData)))
          ((= "1040" xdTypeCode) (setq apData (atof xdData)))
          ((= "1041" xdTypeCode) (setq apData (atof xdData)))
          ((= "1042" xdTypeCode) (setq apData (atof xdData)))
          ((= "1070" xdTypeCode) (setq apData (atoi xdData)))
          ((= "1071" xdTypeCode) (setq apData (atoi xdData)))
        )
        (if apData (setq partXDList (append partXDList (list (cons (atoi xdTypeCode) apData)))))
      )
      (setq newXDList (append newXDList (list partXDList)))
    )
    (entmod 
      (append 
        (entget name)
        (list (cons -3 newXDList))
      )
    )
  )

  ;; 辅助函数：将列表转换为字符串
  (defun listToString (lst / n str) 
    (setq str "")
    (foreach n lst 
      (setq str (strcat str (if (= (type n) 'STR) n (rtos n 2 3)) " "))
    )
    str
  )

  ;; 辅助函数：将字符串转换为列表
  (defun stringToList (str / i lst str1) 
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

  ;; 主程序逻辑
  (setvar "cmdecho" 0) ; 关闭命令响应
  (vl-load-com)

  ; (prompt "\n    鼠标移动查询信息，左键编辑，右键退出！")
  ;; 初始化变量
  (setq loop    t
        txtList '()
        oldEnt  nil
  )

  ;; 创建一个临时多段线实体用于显示文本框
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
  (setq lwplBackground (entlast))
  (command "DRAWORDER" lwplBackground "" "B")

  ;; 主要循环：监听用户的鼠标操作
  (while loop 
    (setq gr   (grread t 4 2)
          code (car gr)
          pt   (cadr gr)
    )
    (cond 
      ((= code 3) ; 鼠标左击
       ;  (deleteTextObjects txtList)
       (if ent 
         (progn 
           (if (setq xDatas (cdr (assoc -3 (entget ent '("*"))))) 
             (progn 
               ;; 创建临时DCL文件
               (setq dclName (vl-filename-mktemp "yx.dcl")
                     f       (open dclName "w")
                     cnt     0
                     total   0
               )
               (write-line "yx1:dialog {" f)
               (write-line "label = 扩展数据 ; " f)
               (write-line ":row {" f)
               (write-line ":column {" f)
               ;; 控制控件行列数量,单列最多15个
               (foreach appDatas xDatas 
                 (foreach data appDatas 
                   (setq total (1+ total))
                 )
               )
               (if (> total 15) 
                 (setq eachCol (1+ (/ total 15))
                       eachCol (1+ (/ total eachCol))
                 )
                 (setq eachCol 15)
               )

               ;; 为每个扩展数据生成对应的控件,记录key
               (setq allKeyList '())
               (foreach appDatas xDatas 
                 (setq appKeyList '())
                 (foreach data appDatas 
                   (setq cnt    (1+ cnt)
                         cntStr (itoa cnt)
                         rflag  nil
                   )
                   (cond 
                     ;; 字符串组码
                     ((= (type data) 'STR) (write-line (strcat "    :text { label =\"程序名\"" "; key = \"" cntStr "\"; value=\"" data "\";}") f) (setq rflag 1))
                     ;; 列表组码
                     ((listp data)
                      (setq xdTypeCode    (car data)
                            xdTypeCodeStr (itoa xdTypeCode)
                            xdData        (cdr data)
                      )
                      ;; 根据组码生成相应的控件
                      (cond 
                        ((= xdTypeCode 1000) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1001) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1002) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1003) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1004) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1005) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" xdData "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1010) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1020) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1030) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1011) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1021) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1031) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1012) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1022) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1032) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1013) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1023) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1033) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (listToString xdData) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1040) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (rtos xdData 2 3) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1041) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (rtos xdData 2 3) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1042) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (rtos xdData 2 3) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1070) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (rtos xdData 2 0) "\";}") f) (setq rflag 1))
                        ((= xdTypeCode 1071) (write-line (strcat "    :edit_box { label =\"" xdTypeCodeStr "\"; key = \"" cntStr "\"; value=\"" (rtos xdData 2 0) "\";}") f) (setq rflag 1))
                        (T (princ (strcat "Unknown GroupCode" xdTypeCodeStr)))
                      )
                     )
                   )
                   (if rflag (setq appKeyList (append appKeyList (list cntStr))))
                   (if (= (rem cnt eachCol) 0) (progn (write-line " }" f) (write-line ":column {" f)))
                 )
                 (setq allKeyList (append allKeyList (list appKeyList)))
               )

               ;; 完成DCL文件的创建
               (write-line " }" f)
               (write-line " }" f)
               (write-line "    :row { :button { key = \"e01\" ; label = \"确认\" ;  is_default = true ;   }" f)
               (write-line "           :button { key = \"e02\" ; label = \"取消\" ; is_cancel = true ; } } }" f)
               (close f)

               ;; 加载并显示DCL对话框
               (setq dclRe (load_dialog dclName))
               (new_dialog "yx1" dclRe)
               ;  (setq i 0)
               (action_tile "e01" "(writeXData ent allKeyList)(done_dialog 1)")
               (start_dialog)
               (unload_dialog dclRe)
               (vl-file-delete dclName)
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
           (setq cnt      0
                 total    0
                 allLines '()
           )

           ;; 根据鼠标指向的实体生成扩展数据的显示文本
           (foreach appDatas xDatas 
             (setq appLines '())
             (foreach data appDatas 
               (setq cnt    (1+ cnt)
                     cntStr (itoa cnt)
                     rflag  nil
               )
               (cond 
                 ((listp data)
                  (setq xdTypeCode    (car data)
                        xdTypeCodeStr (itoa xdTypeCode)
                        xdData        (cdr data)
                  )
                  (cond 
                    ((= xdTypeCode 1000) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1001) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1002) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1003) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1004) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1005) (setq thisLine (strcat xdTypeCodeStr "      " xdData)) (setq rflag 1))
                    ((= xdTypeCode 1010) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1020) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1030) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1011) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1021) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1031) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1012) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1022) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1032) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1013) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1023) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1033) (setq thisLine (strcat xdTypeCodeStr "      " (listToString xdData))) (setq rflag 1))
                    ((= xdTypeCode 1040) (setq thisLine (strcat xdTypeCodeStr "      " (rtos xdData 2 3))) (setq rflag 1))
                    ((= xdTypeCode 1041) (setq thisLine (strcat xdTypeCodeStr "      " (rtos xdData 2 3))) (setq rflag 1))
                    ((= xdTypeCode 1042) (setq thisLine (strcat xdTypeCodeStr "      " (rtos xdData 2 3))) (setq rflag 1))
                    ((= xdTypeCode 1070) (setq thisLine (strcat xdTypeCodeStr "      " (rtos xdData 2 0))) (setq rflag 1))
                    ((= xdTypeCode 1071) (setq thisLine (strcat xdTypeCodeStr "      " (rtos xdData 2 0))) (setq rflag 1))
                  )
                 )
                 ((= (type data) 'STR) (setq thisLine data) (setq rflag 1))
               )
               (if rflag (setq appLines (cons thisLine appLines)))
             )
             (setq allLines (cons (reverse appLines) allLines))
           )

           ;; 更新显示文本
           (setq i       0
                 maxWide 0.0
                 pt      (calculateRelativePoint pt d (* -2 d))
           )
           ; (if (or (and ent (not oldEnt)) (not (equal oldEnt ent)))
           (if (/= ent oldEnt) 
             (progn 
               (if oldEnt (redraw oldEnt 4))
               (redraw ent 3)
               (setq oldEnt ent)
               (if txtList 
                 (progn 
                   (while (< 0 (length txtList)) 
                     (entdel (car txtList))
                     (setq txtList (cdr txtList))
                   )
                   (setq oldEnt nil)
                   (if lwplBackground 
                     (progn 
                       (setq en (entget lwplBackground)
                             en (updatePolyline en (list '(0.0 0.0) '(0.0 0.0)))
                       )
                       (entmod (modifyEntityDxf en 43 0.0))
                     )
                   )
                 )
               )
               (foreach y (reverse allLines) 
                 (foreach x y 
                   (entmake 
                     (list '(0 . "TEXT") 
                           (cons 10 (calculateRelativePoint pt 0 (* -1.5 d i)))
                           (cons 62 6)
                           (cons 40 d)
                           (cons 1 x)
                           '(41 . 0.9)
                     )
                   )
                   (setq en       (entlast)
                         txtList  (cons en txtList)
                         thisWide (car (cadr (textbox (entget en))))
                   )
                   (if (> thisWide maxWide) 
                     (setq maxWide thisWide)
                   )
                   (setq i (1+ i))
                 )
               )
             )
             (if txtList 
               (progn 
                 (foreach tx (reverse txtList) 
                   (setq en       (entget tx)
                         en       (modifyEntityDxf en 10 (calculateRelativePoint pt 0 (* -1.5 d i)))
                         en       (modifyEntityDxf en 40 d)
                         thisWide (car (cadr (textbox en)))
                   )
                   (entmod en)
                   (if (> thisWide maxWide) 
                     (setq maxWide thisWide)
                   )
                   (setq i (1+ i))
                 )
               )
             )
           )


           (setq h  (* -0.75 d (length txtList))
                 en (entget lwplBackground)
                 en (updatePolyline en (list (calculateRelativePoint pt 0 (+ h (* 1.5 d))) (calculateRelativePoint pt (+ (* 0.3 d) maxWide) (+ h (* 1.5 d)))))
                 en (modifyEntityDxf en 43 (+ (* -2 h) (* 0.65 d)))
           )
           (entmod en)
         )
         (if txtList 
           (progn 
             (while (< 0 (length txtList)) 
               (entdel (car txtList))
               (setq txtList (cdr txtList))
             )
             (setq oldEnt nil)
             (if lwplBackground 
               (progn 
                 (setq en (entget lwplBackground)
                       en (updatePolyline en (list '(0.0 0.0) '(0.0 0.0)))
                 )
                 (entmod (modifyEntityDxf en 43 0.0))
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
       (deleteTextObjects txtList)
       (setq loop nil)
      )
      (t)
    )
  )
  (princ)
)