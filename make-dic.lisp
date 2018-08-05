(in-package :cl-tcode)

(defvar *kana-regex* "[ぁあ-ヶー—]")
(defvar *kanji-regex* "[^ぁあ-ヶー—]")
;;; 音節は小さい字、ん、音引で始まってはいけない
(defvar *onsetsu-regex* "[あいうえおか-ぢつ-もやゆよ-ろわゐゑを][ぁあ-ヶー—]*")

(defvar *kana-seq-regex* (format nil "~a+" *kana-regex*)
  "かな の並びにマッチする正規表現")

(defvar *any-yomi-regex* (format nil "((?:~a)+?)" *onsetsu-regex*)
  "どんな読みにもマッチする正規表現")

(defvar *ahead-behind-kanji* (format nil "((?=~a)|(?<=~:*~a))" *kanji-regex*)
  "漢字の直前、直後にマッチする正規表現")

(defun yomi-candidate-to-regex (lst)
  "読みの候補のリストから正規表現に変換する"
  (format nil "^~{(~a)~}$" 
	  (mapcar #'(lambda (x) (if (consp x)
				    (format nil "~{~a~^|~}" x)
				    x))
		  lst)))

(defvar *yomi-kanji* (make-hash-table :test #'equal))

;;; step1相当
(defun read-pubdic-file (file)
  (with-open-file (st file)
    (flet ((add-mark (str)
	     (concatenate 'string str "—")))
      (loop for line = (read-line st nil)
	 for (yomi kanji type) = (ppcre:split "\\s+" line)
	 while line
	 unless (or (ppcre:scan "^[あ-ヶー]+$" kanji)
		    (ppcre:scan "[ァ-ヶー０-９Ａ-ｚ0-9]" kanji))
	 collect (cond ((ppcre:scan ".行.*段" type)
			(list (add-mark yomi) (add-mark kanji) "1"))
		       ((or (ppcre:scan "一段" type) (string= type "形容詞"))
			(list (add-mark yomi) (add-mark kanji) type))
		       ((or (string= type "0") (string= type "2"))
			(list yomi kanji type))
		       (t
			(list yomi kanji "0")))))))

(defun tankan-yomi (yomi kanji)
  "kanji内の単漢字の読みを返す。"
  (ppcre:register-groups-bind (header tankan trailer) 
      ((format nil "^(~a*)(~a)(~a*)$" 	; 漢字が一つの場合のみ対応
	       *kana-regex* *kanji-regex* *kana-regex*)
       kanji)
    (ppcre:register-groups-bind (tankan-yomi)
	((format nil "^~a(.*)~a$" header trailer) yomi)
      (list tankan-yomi tankan))))
	  
(defun make-kanji->yomi (lst)
  (loop for (yomi kanji) in lst
     for (tyomi tkan) = (tankan-yomi yomi kanji)
     with tankan-yomi-map = (make-hash-table :test #'equal)
     do
       (when tyomi
	 (alexandria:ensure-gethash tkan tankan-yomi-map)
	 (pushnew tyomi (gethash tkan tankan-yomi-map) :test #'equal))
     finally
       (return tankan-yomi-map)))

(defvar *dakuon*
  (loop for c across "かきくけこさしすせそたちつてとはひふへほばびぶべぼ"
        for d across "がぎぐげござじずぜぞだぢづでどばびぶべぼぱぴぷぺぽ"
        with hash = (make-hash-table :test #'equal)
        do
           (setf (gethash (format nil "~c" c) hash)
                 (format nil "~c" d))
        finally
           (return hash)))

(defun sokuon-onbin (yomi)
  (labels ((dakuon (s reg) ;濁音を返す。ば行はぱ行を返す。
             (ppcre:regex-replace
              reg s #'(lambda (match reg1 reg2)
                        (declare (ignore match))
                        (format nil "~a~a" (gethash reg1 *dakuon*) reg2))
              :simple-calls t))
           (sokuon (s)
             (ppcre:regex-replace "[つく]$" s "っ")))
    (let* ((daku-yomi (dakuon yomi "^([かきくけこさしすせそたちつてとはひふへほ])(.*)$"))
           (handaku-yomi (dakuon daku-yomi "^([ばびぶべぼ])(.*)$"))
           (di (ppcre:regex-replace "^ぢ" daku-yomi "じ")))
      (remove-duplicates (remove yomi (list (sokuon yomi) daku-yomi
                                            (sokuon daku-yomi)
                                            handaku-yomi (sokuon handaku-yomi)
                                            di (sokuon di))
                                 :test #'equal)
                         :test #'equal))))

(defun add-onbin (yomi-list)
  (loop for yomi in yomi-list
     append (cons yomi (sokuon-onbin yomi))))

(defun split-kanji-kana (kanji)
   (ppcre:split *ahead-behind-kanji* kanji))		

;;; match-itr is from step2.pl
(defun match-itr (yomi kanji k-map)
  (let ((kana-kanji (split-kanji-kana kanji)))
    (labels ((kanji-yomi-list (yomi-array)
	       (loop for kk in kana-kanji
		  for y across yomi-array
		  if (ppcre:scan *kana-seq-regex* kk)
		    collect (list kk) into result
		  else
		    collect (list kk y) into result
		  finally
		    (return result)))
      	     (get-yomi-list (k p)
	       (if (and (string= k "々") )
		   (add-onbin (gethash p k-map))
		   (add-onbin (gethash k k-map)))))
      (loop for kk in kana-kanji
	 for prev = nil then kk
	 if (ppcre:scan *kana-seq-regex* kk) collect kk into pattern
	 else if (get-yomi-list kk prev) collect it into pattern
	 else 
	   count kk into unknown and
	   collect *any-yomi-regex* into pattern
	 finally
	   (return 
	     (if (> unknown 1)
		 (values nil "わからない字が複数")
		 (multiple-value-bind (match registers)
		     (ppcre:scan-to-strings (yomi-candidate-to-regex pattern) 
					   yomi)
		   (if match
		       (kanji-yomi-list registers)
		       (values nil (format nil "not match ~a ~a" yomi pattern))))))))))

(defun step2 (lst k-map)
  (loop for (yomi kanji type) in lst
     if (string/= type "2")
       if (= (length kanji) 1) collect (list yomi kanji) into tankan
       else if (match-itr yomi kanji k-map) collect it into kihon 
       else collect (list yomi kanji) into jukujiku end ; 読みの分からない熟語
     finally (return (values kihon tankan jukujiku))))

(defun make-mzk-kanji (l)
  (loop for (a) in l
     collect a into result
     finally
       (return (format nil "~{~a~}" result))))

(defun make-mzk-yomi (l)
  (loop for (a  b) in l
     for result = `((,a) ,@(if b `((,b))))
     then `(,@(mapcar #'(lambda (x) `(,a ,@x)) result)
	      ,@(if b (mapcar #'(lambda (x) `(,b ,@x)) result)))
     finally
       (return (mapcar #'(lambda (x)
			   (format nil "~{~a~}" (reverse x)))
		       result))))

(defun setup-simple-dic (lst dic)
  (loop for (yomi kanji) in lst
     for key = (if (conjugation-p yomi) (remove #\—  yomi) yomi)
     do
       (alexandria:ensure-gethash key dic (make-word-list))
       (if (conjugation-p yomi)
	   (push (remove #\— kanji)
		 (word-list-conjugation (gethash key dic)))
	   (push kanji (word-list-noun (gethash key dic))))))

(defun setup-mzk-dic (lst dic)
  (loop for l in lst
     for kanji = (make-mzk-kanji l)
     for yomi-list = (remove kanji (make-mzk-yomi l) :test #'equal)
     do
       (setup-simple-dic (mapcar #'(lambda (x) (list x kanji)) 
				 yomi-list)
                         dic)))

(defun make-mazegaki-dic (files base save)
  (let* ((contents (loop for f in files
                         append (read-pubdic-file (merge-pathnames f base))))
         (kanji->yomi (make-kanji->yomi contents))
         (mzk-dic (make-hash-table :test #'equal)))
    (multiple-value-bind (kihon tankan jukuji)
	(step2 contents kanji->yomi)
      (setup-simple-dic tankan mzk-dic)
      (setup-simple-dic jukuji mzk-dic)
      (setup-mzk-dic kihon mzk-dic))
    (cl-store:store mzk-dic save)))

