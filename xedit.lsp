(defun c:xvg (/ eventCode scaleR apData dclHandle dclName elist mouseCoor mouseEntCur mouseEntLast f grList lineIndex cnt eachCol total inf xDatas xdTypeCode thisKey appKeyList allKeyList loop annoBgLwpl annoList maxWide xdTypeCode) 
  ; 编辑实体的扩展数据

  ;; 错误处理
  (defun *error* (inf) 
    (setq inf (strcase inf t))
    (if (wcmatch inf "*break*,*cancel*,*exit*,*取消*,*中断*") 
      (resetAnnoObjects)
      (entdel annoBgLwpl)
      ; (redraw mouseEntCur 4)
    )
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; 辅助函数：移除提示对象，仅程序结束时调用
  (defun resetAnnoObjects (/ ent txt) 
    (if annoList 
      (progn 
        (foreach txt annoList 
          (entdel txt)
        )
        (setq annoList '())
      )
    )
    (if annoBgLwpl 
      (progn 
        (setq ent (entget annoBgLwpl)
              ent (updatePolyline ent (list '(0.0 0.0) '(0.0 0.0)))
        )
        (entmod (modifyEntityDxf ent 43 0.0))
      )
    )
  )

  ;; 辅助函数：计算相对坐标
  (defun calculateRelativePoint (pt offX offY /) 
    (mapcar '+ pt (list offX offY))
  )

  ;; 辅助函数：获取实体的指定DXF代码值
  (defun getDxfValue (ent n /) 
    (if (= (type ent) 'ename) 
      (setq ent (entget ent))
    )
    (cdr (assoc n ent))
  )

  ;; 辅助函数：修改实体的指定DXF代码值
  (defun modifyEntityDxf (ent i n /) 
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

  ;; 辅助函数：写入扩展数据
  ;; get_attr (AutoLISP/DCL): https://help.autodesk.com/view/OARX/2025/CHS/?guid=GUID-AB9100B0-F19E-4C29-B82A-28E8A607BF4E
  ;; get_tile (AutoLISP/DCL): https://help.autodesk.com/view/OARX/2025/CHS/?guid=GUID-3BD2F6DF-DCD5-4A00-9BDF-E2C7C7C286F8
  (defun writeXData (ent allKeyList / partXDList newXDList) 
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
        (entget ent)
        (list (cons -3 newXDList))
      )
    )
  )

  ;; 辅助函数：将列表转换为字符串
  (defun listToString (oriList / each newStr) 
    (setq newStr "")
    (foreach each oriList 
      (setq newStr (strcat newStr (if (= (type each) 'newStr) each (rtos each 2 3)) " "))
    )
    newStr
  )

  ;; 辅助函数：将字符串转换为列表
  (defun stringToList (oriStr / index newList str) 
    (setq newList '()
          index   1
    )
    (while (/= oriStr "") 
      (if (= (substr oriStr index 1) " ") 
        (setq str     (substr oriStr 1 (1- index))
              newList (cons (atof str) newList)
              oriStr  (substr oriStr (1+ index))
              index   1
        )
        (setq index (1+ index))
      )
    )
    (reverse newList)
  )

  ;; 主程序逻辑
  (setvar "cmdecho" 0) ; 关闭命令响应
  (vl-load-com)

  (prompt "\n[xDataEdit] 鼠标移动查询信息，点选编辑，右键退出！")
  ;; 初始化变量
  (setq loop         t
        annoList     '()
        mouseEntCur  nil
        mouseEntLast nil
        doc          (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (vla-StartUndoMark doc)

  ;; 创建一个临时多段线实体用于显示文本框
  (entmake 
    (list '(0 . "LWPOLYLINE") 
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 62 186) ;; 背景颜色：186号色
          (cons 90 2)
          (cons 10 '(0.0 0.0))
          (cons 10 '(0.0 0.0))
    )
  )
  (setq annoBgLwpl (entlast))
  (command "DRAWORDER" annoBgLwpl "" "B")

  ;; 主要循环：监听用户的鼠标操作
  (while loop 
    (setq grList    (grread t 4 2)
          eventCode (car grList)
          mouseCoor (cadr grList)
          scaleR    (* (/ (getvar "viewsize") (cadr (getvar "screensize"))) (getvar "pickbox")) ;  scaleR = 当前视图的高度 / 屏幕像素高度(y) * 选择框大小
    )
    (cond 
      ((= eventCode 3) ; 鼠标左击
       ;  (deleteTextObjects)
       (if mouseEntCur 
         (progn 
           (if (setq xDatas (cdr (assoc -3 (entget mouseEntCur '("*"))))) 
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
               (setq dclHandle (load_dialog dclName))
               (new_dialog "yx1" dclHandle)
               ;  (setq i 0)
               (action_tile "e01" "(writeXData mouseEntCur allKeyList)(done_dialog 1)")
               (start_dialog)
               (unload_dialog dclHandle)
               (vl-file-delete dclName)
             )
             (princ "\n没有扩展数据!")
           )
         )
       )
      )

      ((= eventCode 5) ; 鼠标移动
       (if 
         (and 
           (setq mouseEntCur (nentselp mouseCoor)
                 mouseEntCur (if (and mouseEntCur (= (type (last (last mouseEntCur))) 'ename)) 
                               (last (last mouseEntCur))
                               (car mouseEntCur)
                             )
           )
           (setq xDatas (cdr (assoc -3 (entget mouseEntCur '("*")))))
         )
         ; Y：有找到带xdata的对象
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
           (setq lineIndex     0
                 maxWide       0.0
                 textBasePoint (calculateRelativePoint mouseCoor scaleR (* -2 scaleR))
           )
           ; (if (or (and ent (not mouseEntLast)) (not (equal mouseEntLast ent)))
           (if (/= mouseEntCur mouseEntLast) 
             ;; 更新提示内容
             (progn 
               ;  (if mouseEntLast (redraw mouseEntLast 4))
               ;  (redraw mouseEntCur 3)
               (setq mouseEntLast mouseEntCur)
               ; 删除当前提示文字
               (if annoList 
                 (progn 
                   (foreach txt annoList 
                     (entdel txt)
                   )
                   (setq annoList '())
                 )
               )
               ;; 绘制新的提示文字
               (foreach y (reverse allLines) 
                 (foreach x y 
                   (entmake 
                     (list '(0 . "TEXT") 
                           (cons 10 (calculateRelativePoint textBasePoint 0 (* -1.5 scaleR lineIndex)))
                           (cons 62 6)
                           (cons 40 scaleR)
                           (cons 1 x)
                           '(41 . 0.9)
                     )
                   )
                   (setq ent      (entlast)
                         annoList (cons ent annoList)
                         maxWide  (max maxWide (car (cadr (textbox (entget ent)))))
                   )
                   (setq lineIndex (1+ lineIndex))
                 )
               )
             )
             ;; 移动文字和调整字号
             (if annoList 
               (progn 
                 (foreach txt annoList 
                   (setq elist     (entget txt)
                         elist     (modifyEntityDxf elist 10 (calculateRelativePoint textBasePoint 0 (* -1.5 scaleR lineIndex)))
                         elist     (modifyEntityDxf elist 40 scaleR)
                         maxWide   (max maxWide (car (cadr (textbox elist))))
                         lineIndex (1+ lineIndex)
                   )
                   (entmod elist)
                 )
               )
             )
           )
           ;; 调整背景
           (setq bgHeight (* -0.75 scaleR (length annoList))
                 bgLeft   (calculateRelativePoint textBasePoint 0 (+ bgHeight (* 1.5 scaleR)))
                 bgRight  (calculateRelativePoint textBasePoint (+ (* 0.3 scaleR) maxWide) (+ bgHeight (* 1.5 scaleR)))
                 elist    (updatePolyline (entget annoBgLwpl) (list bgLeft bgRight))
                 elist    (modifyEntityDxf elist 43 (+ (* -2 bgHeight) (* 0.65 scaleR)))
           )
           (entmod elist)
           ;  (vla-MoveToBottom (vlax-ename->vla-object annoBgLwpl))
           ; (command "DRAWORDER" annoBgLwpl "" "B")
         )
         ;; N: 没找到符合条件的对象
         ;  (setq mouseEntLast nil)
         (resetAnnoObjects)
       )
       ;  (redraw)
      )

      ((or  ; 鼠标右击
           (= eventCode 11)
           (= eventCode 25)
       )
       (resetAnnoObjects)
       (setq loop nil)
      )
      (t)
    )
  )
  (princ)
)