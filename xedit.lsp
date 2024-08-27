(defun c:xvg (/ eventCode scaleR apData dclHandle dclName elist mouseCoor mouseEntCur mouseEntLast f grList lineIndex cnt eachCol total inf xDatas xdTypeCode thisKey appKeyList allKeyList loop annoBgLwpl annoList maxWide xdTypeCode) 
  ; �༭ʵ�����չ����

  ;; ������
  (defun *error* (inf) 
    (setq inf (strcase inf t))
    (if (wcmatch inf "*break*,*cancel*,*exit*,*ȡ��*,*�ж�*") 
      (resetAnnoObjects)
      (entdel annoBgLwpl)
      ; (redraw mouseEntCur 4)
    )
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; �����������Ƴ���ʾ���󣬽��������ʱ����
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

  ;; ���������������������
  (defun calculateRelativePoint (pt offX offY /) 
    (mapcar '+ pt (list offX offY))
  )

  ;; ������������ȡʵ���ָ��DXF����ֵ
  (defun getDxfValue (ent n /) 
    (if (= (type ent) 'ename) 
      (setq ent (entget ent))
    )
    (cdr (assoc n ent))
  )

  ;; �����������޸�ʵ���ָ��DXF����ֵ
  (defun modifyEntityDxf (ent i n /) 
    (subst 
      (cons i n)
      (assoc i ent)
      ent
    )
  )

  ;; �������������¶���߶���
  (defun updatePolyline (plEnt ptList / i) 
    (setq i -1) ; ��ʼ��������
    (mapcar 
      (function 
        (lambda (x) 
          (if (= (car x) 10)  ; ���������� 10
            (progn 
              (setq i (1+ i)) ; ��������
              (cons 10 (nth i ptList)) ; �滻Ϊ�µĶ�������
            )
            x ; �������뱣�ֲ���
          )
        )
      )
      plEnt ; ����Ķ����ʵ��� DXF �����б�
    )
  )

  ;; ����������д����չ����
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
          ((wcmatch xdTypeCode "������") (setq partXDList (cons xdData partXDList)))
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

  ;; �������������б�ת��Ϊ�ַ���
  (defun listToString (oriList / each newStr) 
    (setq newStr "")
    (foreach each oriList 
      (setq newStr (strcat newStr (if (= (type each) 'newStr) each (rtos each 2 3)) " "))
    )
    newStr
  )

  ;; �������������ַ���ת��Ϊ�б�
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

  ;; �������߼�
  (setvar "cmdecho" 0) ; �ر�������Ӧ
  (vl-load-com)

  (prompt "\n[xDataEdit] ����ƶ���ѯ��Ϣ����ѡ�༭���Ҽ��˳���")
  ;; ��ʼ������
  (setq loop         t
        annoList     '()
        mouseEntCur  nil
        mouseEntLast nil
        doc          (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (vla-StartUndoMark doc)

  ;; ����һ����ʱ�����ʵ��������ʾ�ı���
  (entmake 
    (list '(0 . "LWPOLYLINE") 
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 62 186) ;; ������ɫ��186��ɫ
          (cons 90 2)
          (cons 10 '(0.0 0.0))
          (cons 10 '(0.0 0.0))
    )
  )
  (setq annoBgLwpl (entlast))
  (command "DRAWORDER" annoBgLwpl "" "B")

  ;; ��Ҫѭ���������û���������
  (while loop 
    (setq grList    (grread t 4 2)
          eventCode (car grList)
          mouseCoor (cadr grList)
          scaleR    (* (/ (getvar "viewsize") (cadr (getvar "screensize"))) (getvar "pickbox")) ;  scaleR = ��ǰ��ͼ�ĸ߶� / ��Ļ���ظ߶�(y) * ѡ����С
    )
    (cond 
      ((= eventCode 3) ; ������
       ;  (deleteTextObjects)
       (if mouseEntCur 
         (progn 
           (if (setq xDatas (cdr (assoc -3 (entget mouseEntCur '("*"))))) 
             (progn 
               ;; ������ʱDCL�ļ�
               (setq dclName (vl-filename-mktemp "yx.dcl")
                     f       (open dclName "w")
                     cnt     0
                     total   0
               )
               (write-line "yx1:dialog {" f)
               (write-line "label = ��չ���� ; " f)
               (write-line ":row {" f)
               (write-line ":column {" f)
               ;; ���ƿؼ���������,�������15��
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

               ;; Ϊÿ����չ�������ɶ�Ӧ�Ŀؼ�,��¼key
               (setq allKeyList '())
               (foreach appDatas xDatas 
                 (setq appKeyList '())
                 (foreach data appDatas 
                   (setq cnt    (1+ cnt)
                         cntStr (itoa cnt)
                         rflag  nil
                   )
                   (cond 
                     ;; �ַ�������
                     ((= (type data) 'STR) (write-line (strcat "    :text { label =\"������\"" "; key = \"" cntStr "\"; value=\"" data "\";}") f) (setq rflag 1))
                     ;; �б�����
                     ((listp data)
                      (setq xdTypeCode    (car data)
                            xdTypeCodeStr (itoa xdTypeCode)
                            xdData        (cdr data)
                      )
                      ;; ��������������Ӧ�Ŀؼ�
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

               ;; ���DCL�ļ��Ĵ���
               (write-line " }" f)
               (write-line " }" f)
               (write-line "    :row { :button { key = \"e01\" ; label = \"ȷ��\" ;  is_default = true ;   }" f)
               (write-line "           :button { key = \"e02\" ; label = \"ȡ��\" ; is_cancel = true ; } } }" f)
               (close f)

               ;; ���ز���ʾDCL�Ի���
               (setq dclHandle (load_dialog dclName))
               (new_dialog "yx1" dclHandle)
               ;  (setq i 0)
               (action_tile "e01" "(writeXData mouseEntCur allKeyList)(done_dialog 1)")
               (start_dialog)
               (unload_dialog dclHandle)
               (vl-file-delete dclName)
             )
             (princ "\nû����չ����!")
           )
         )
       )
      )

      ((= eventCode 5) ; ����ƶ�
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
         ; Y�����ҵ���xdata�Ķ���
         (progn 
           (setq cnt      0
                 total    0
                 allLines '()
           )

           ;; �������ָ���ʵ��������չ���ݵ���ʾ�ı�
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

           ;; ������ʾ�ı�
           (setq lineIndex     0
                 maxWide       0.0
                 textBasePoint (calculateRelativePoint mouseCoor scaleR (* -2 scaleR))
           )
           ; (if (or (and ent (not mouseEntLast)) (not (equal mouseEntLast ent)))
           (if (/= mouseEntCur mouseEntLast) 
             ;; ������ʾ����
             (progn 
               ;  (if mouseEntLast (redraw mouseEntLast 4))
               ;  (redraw mouseEntCur 3)
               (setq mouseEntLast mouseEntCur)
               ; ɾ����ǰ��ʾ����
               (if annoList 
                 (progn 
                   (foreach txt annoList 
                     (entdel txt)
                   )
                   (setq annoList '())
                 )
               )
               ;; �����µ���ʾ����
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
             ;; �ƶ����ֺ͵����ֺ�
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
           ;; ��������
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
         ;; N: û�ҵ����������Ķ���
         ;  (setq mouseEntLast nil)
         (resetAnnoObjects)
       )
       ;  (redraw)
      )

      ((or  ; ����һ�
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