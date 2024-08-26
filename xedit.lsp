(defun c:xvg (/ code d apData dclRe dclName en ent f gr i cnt eachCol total inf thisKey appKeyList allKeyList xDatas loop lst1 lst2 lwplBackground n name nEnt oldEnt pt ptList ss str str1 txtList maxWide ww x y xdTypeCode) 
  ; �༭ʵ�����չ����

  ;; ������
  (defun *error* (inf) 
    (setq inf (strcase inf t))
    (if (wcmatch inf "*break*,*cancel*,*exit*,*ȡ��*,*�ж�*") 
      (deleteTextObjects txtList)
    )
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  )

  ;; ���������������������
  (defun calculateRelativePoint (pt w ww) 
    (list (+ (car pt) w) (+ (cadr pt) ww))
  )

  ;; ������������ȡʵ���ָ��DXF����ֵ
  (defun getDxfValue (ent n) 
    (if (= (type ent) 'ename) 
      (setq ent (entget ent))
    )
    (cdr (assoc n ent))
  )

  ;; �����������޸�ʵ���ָ��DXF����ֵ
  (defun modifyEntityDxf (ent i n) 
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

  ;; ����������ɾ����ʾ���ı������б�
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

  ;; ����������д����չ����
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
        (entget name)
        (list (cons -3 newXDList))
      )
    )
  )

  ;; �������������б�ת��Ϊ�ַ���
  (defun listToString (lst / n str) 
    (setq str "")
    (foreach n lst 
      (setq str (strcat str (if (= (type n) 'STR) n (rtos n 2 3)) " "))
    )
    str
  )

  ;; �������������ַ���ת��Ϊ�б�
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

  ;; �������߼�
  (setvar "cmdecho" 0) ; �ر�������Ӧ
  (vl-load-com)

  ; (prompt "\n    ����ƶ���ѯ��Ϣ������༭���Ҽ��˳���")
  ;; ��ʼ������
  (setq loop    t
        txtList '()
        oldEnt  nil
  )

  ;; ����һ����ʱ�����ʵ��������ʾ�ı���
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

  ;; ��Ҫѭ���������û���������
  (while loop 
    (setq gr   (grread t 4 2)
          code (car gr)
          pt   (cadr gr)
    )
    (cond 
      ((= code 3) ; ������
       ;  (deleteTextObjects txtList)
       (if ent 
         (progn 
           (if (setq xDatas (cdr (assoc -3 (entget ent '("*"))))) 
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
               (setq dclRe (load_dialog dclName))
               (new_dialog "yx1" dclRe)
               ;  (setq i 0)
               (action_tile "e01" "(writeXData ent allKeyList)(done_dialog 1)")
               (start_dialog)
               (unload_dialog dclRe)
               (vl-file-delete dclName)
             )
             (princ "\nû����չ����!")
           )
         )
       )
      )

      ((= code 5) ; ����ƶ�
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

      ((or  ; ����һ�
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