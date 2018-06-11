;;; isd-turtle.el --- Utility to dump ideographic-structure as Turtle files

;; Copyright (C) 2017, 2018 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: Ideographic Structures (漢字構造、解字), IDS, CHISE, RDF, Turtle

;; This file is a part of CHISE-ISD (Ideographic Structure Database).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cwiki-common)

(defvar isd-url-prefix "http://rdf.chise.org/data/")

(setq est-coded-charset-priority-list
  '(; =ucs
    =mj
    =adobe-japan1-0
    =adobe-japan1-1
    =adobe-japan1-2
    =adobe-japan1-3
    =adobe-japan1-4
    =adobe-japan1-5
    =adobe-japan1-6
    =ucs@iso
    =jis-x0208 =jis-x0208@1990
    =jis-x0213-1
    =jis-x0213-1@2000 =jis-x0213-1@2004
    =jis-x0213-2
    =jis-x0212
    =gt
    =hanyo-denshi/ks
    =hanyo-denshi/tk
    =ucs-itaiji-001
    =ucs-itaiji-002
    =ucs-itaiji-003
    =ucs-itaiji-004
    =ucs-itaiji-005
    =ucs-itaiji-006
    =ucs-itaiji-007
    =ucs-itaiji-008
    =ucs-itaiji-009
    =ucs-itaiji-010
    =ucs-itaiji-084
    =ucs-var-001
    =ucs-var-002
    =ucs-var-003
    =ucs-var-004
    =ucs-var-005
    =cns11643-1 =cns11643-2 =cns11643-3
    =cns11643-4 =cns11643-5 =cns11643-6 =cns11643-7
    =gb2312
    =big5-cdp
    =ks-x1001
    =gt-k
    =ucs@unicode
    =ucs@JP/hanazono
    =gb12345
    =ucs@cns
    =ucs@gb
    =zinbun-oracle =>zinbun-oracle
    =daikanwa
    =ruimoku-v6
    =cbeta =jef-china3
    =daikanwa/+2p
    =+>ucs@iso =+>ucs@unicode
    =+>ucs@jis
    =+>ucs@cns
    =+>ucs@ks
    =+>ucs@jis/1990
    =>mj
    =>jis-x0208 =>jis-x0213-1
    =>jis-x0208@1997
    =>ucs@iwds-1
    =>ucs@cognate
    =>ucs@component
    =>iwds-1
    =>ucs@iso
    =>ucs@unicode
    =>ucs@jis =>ucs@cns =>ucs@ks
    =>gt
    =>gt-k
    =>>ucs@iso =>>ucs@unicode
    =>>ucs@jis =>>ucs@cns =>>ucs@ks
    =>>gt-k
    =>>hanyo-denshi/ks
    ==mj
    ==ucs@iso
    ==ucs@unicode
    ==adobe-japan1-0
    ==adobe-japan1-1
    ==adobe-japan1-2
    ==adobe-japan1-3
    ==adobe-japan1-4
    ==adobe-japan1-5
    ==adobe-japan1-6
    ==ks-x1001
    ==hanyo-denshi/ks
    ==hanyo-denshi/tk
    ==ucs@jis
    ==gt
    ==cns11643-1 ==cns11643-2 ==cns11643-3
    ==cns11643-4 ==cns11643-5 ==cns11643-6 ==cns11643-7
    ==jis-x0212
    ==ucs@cns
    ==koseki
    ==daikanwa
    ==gt-k
    ==ucs@gb
    ==ucs-itaiji-003
    ==ucs@JP/hanazono
    ==daikanwa/+2p
    =>>jis-x0208 =>>jis-x0213-1 =>>jis-x0213-2
    =+>jis-x0208 =+>jis-x0213-1 =+>jis-x0213-2
    =+>hanyo-denshi/jt
    =+>jis-x0208@1978
    =>>gt
    =+>adobe-japan1
    =>>adobe-japan1
    =jis-x0208@1983 =jis-x0208@1978
    =>ucs-itaiji-001
    =>ucs-itaiji-002
    =>ucs-itaiji-003
    =>ucs-itaiji-004
    =>ucs-itaiji-005
    =>ucs-itaiji-006
    =>ucs-itaiji-007
    =>ucs-itaiji-009
    ==>ucs@bucs
    =big5
    =>cbeta
    ===mj
    ===ucs@iso
    ===ucs@unicode
    ===hanyo-denshi/ks
    ===ks-x1001
    ===gt
    ===gt-k
    ===ucs@ks
    ===ucs@gb
    =shinjigen
    =shinjigen@rev
    =shinjigen@1ed
    =shinjigen/+p@rev
    ==shinjigen
    ==shinjigen@rev
    ==daikanwa/+p
    ==shinjigen@1ed
    ===daikanwa/+p
    =>daikanwa/ho
    ===daikanwa/ho
    ))

;; (defvar isd-turtle-ccs-list nil)
(defvar chise-turtle-ccs-prefix-alist nil)

(defun charset-code-point-format-spec (ccs)
  (cond ((memq ccs '(=ucs))
	 "0x%04X")
	(t
	 (let ((ccs-name (symbol-name ccs)))
	   (cond
	    ((string-match
	      "\\(shinjigen\\|daikanwa/ho\\|=>iwds-1\\)"
	      ccs-name)
	     "%04d")
	    ((string-match
	      "\\(gt\\|daikanwa\\|adobe-japan1\\|cbeta\\|zinbun-oracle\\|hng\\)"
	      ccs-name)
	     "%05d")
	    ((string-match "\\(hanyo-denshi/ks\\|koseki\\|mj\\)" ccs-name)
	     "%06d")
	    ((string-match "hanyo-denshi/tk" ccs-name)
	     "%08d")
	    (t
	     "0x%X"))))))

;; (defun isd-turtle-uri-encode-feature-name (feature-name)
;;   (cond
;;    ((eq '=ucs feature-name)
;;     "a.ucs")
;;    ((eq '==>ucs@bucs feature-name)
;;     "bucs")
;;    (t
;;     (mapconcat (lambda (c)
;;                  (if (eq c ?@)
;;                      "_"
;;                    (char-to-string c)))
;;                (www-uri-encode-feature-name feature-name)
;;                ""))))
(defun chise-turtle-uri-encode-ccs-name (feature-name)
  (cond
   ((eq '=ucs feature-name)
    "a.ucs")
   ((eq '=big5 feature-name)
    "a.big5")
   ((eq '==>ucs@bucs feature-name)
    "bucs")
   (t
    (mapconcat (lambda (c)
		 (cond
		  ((eq c ?@)
		   "_")
		  ((eq c ?+)
		   "._.")
		  ((eq c ?=)
		   ".:.")
		  (t
		   (char-to-string c))))
	       (www-uri-encode-feature-name feature-name)
	       ""))))

;; (defun isd-turtle-format-ccs-code-point (ccs code-point)
;;   (unless (memq ccs isd-turtle-ccs-list)
;;     (setq isd-turtle-ccs-list (cons ccs isd-turtle-ccs-list)))
;;   (format "%s:%s"
;;           (isd-turtle-uri-encode-feature-name ccs)
;;           (format (charset-code-point-format-spec ccs)
;;                   code-point)))
(defun chise-turtle-format-ccs-code-point (ccs code-point)
  (let ((ccs-uri (chise-turtle-uri-encode-ccs-name ccs)))
    (unless (assoc ccs-uri chise-turtle-ccs-prefix-alist)
      (setq chise-turtle-ccs-prefix-alist
	    (cons (cons ccs-uri ccs)
		  chise-turtle-ccs-prefix-alist)))
    (format "%s:%s"
	    ccs-uri
	    (format (charset-code-point-format-spec ccs)
		    code-point))))

(defun isd-turtle-encode-char (object)
  (let ((ccs-list est-coded-charset-priority-list)
	ccs ret)
    (if (setq ret (encode-char object '=ucs))
	(chise-turtle-format-ccs-code-point '=ucs ret)
      (while (and ccs-list
		  (setq ccs (pop ccs-list))
		  (not (setq ret (encode-char object ccs 'defined-only)))))
      (cond (ret
	     (chise-turtle-format-ccs-code-point ccs ret)
	     )
	    ((and (setq ccs (car (split-char object)))
		  (setq ret (encode-char object ccs)))
	     (chise-turtle-format-ccs-code-point ccs ret)
	     )
	    (t
	     (format (if est-hide-cgi-mode
			 "system-char-id=0x%X"
		       "system-char-id:0x%X")
		     (encode-char object 'system-char-id))
	     )))))

(defun isd-turtle-format-component (component separator level prefix)
  (cond ((characterp component)
	 (format "%s %c # %c"
		 (isd-turtle-encode-char component)
		 separator
		 component)
	 )
	((consp component)
	 (let ((ret (find-char component)))
	   (cond (ret
		  (format "%s %c # %c"
			  (isd-turtle-encode-char ret) separator ret))
		 ((setq ret (assq 'ideographic-structure component))
		  (if (eq separator ?\;)
		      (format "%s ;"
			      (isd-turtle-format-char nil nil (cdr ret) (1+ level)
						      prefix))
		    (isd-turtle-format-char nil nil (cdr ret) (1+ level)
					    prefix))))))))

(defun isd-turtle-format-char (ccs code-point &optional ids-list level
				   prefix without-head-char)
  (unless level
    (setq level 0))
  (unless prefix
    (setq prefix ""))
  (let ((indent (make-string (* level 4) ?\ ))
	char
	idc idc-str
	p1 p2 p3
	c1 c2 c3
	ret)
    (unless ids-list
      (if (and ccs code-point
	       (setq char (decode-char ccs code-point)))
	  (setq ids-list (get-char-attribute char 'ideographic-structure))))
    (setq idc (car ids-list))
    (setq c1 (nth 1 ids-list)
	  c2 (nth 2 ids-list)
	  c3 (nth 3 ids-list))
    (if (char-ref-p idc)
	(setq idc (plist-get idc :char)))
    (if (and (consp idc)
	     (setq ret (find-char idc)))
	(setq idc ret))
    (if (and (consp c1)
	     (setq ret (find-char c1)))
	(setq c1 ret))
    (if (and (consp c2)
	     (setq ret (find-char c2)))
	(setq c2 ret))
    (if (and (consp c3)
	     (setq ret (find-char c3)))
	(setq c3 ret))
    (cond
     ((eq idc ?\u2FF0) ; ⿰
      (setq p1 'left
	    p2 'right)
      )
     ((eq idc ?⿱)
      (setq p1 'above
	    p2 'below)
      )
     ((eq idc ?⿲)
      (setq p1 'left
	    p2 'middle
	    p3 'right)
      )
     ((eq idc ?⿳)
      (setq p1 'above
	    p2 'middle
	    p3 'below)
      )
     ((memq idc '(?⿴ ?⿵ ?⿶ ?⿷ ?⿸ ?⿹ ?⿺))
      (setq p1 'surround
	    p2 'filling)
      )
     ((eq idc ?⿻)
      (setq p1 'underlying
	    p2 'overlaying)
      )
     ((and idc (eq (encode-char idc '=ucs-itaiji-001) #x2FF6))
      (setq idc-str "SLR")
      (setq p1 'surround
	    p2 'filling)
      )
     ((and idc (eq (encode-char idc '=ucs-var-001) #x2FF0))
      (setq idc-str "⿰・SLR")
      (setq p1 'left
	    p2 'right)
      )
     ((and idc (eq (encode-char idc '=>iwds-1) 307))
      (setq idc-str "⿰・⿺")
      (setq p1 'left
	    p2 'right)
      )
     ((and idc (eq (encode-char idc '=>iwds-1) 305))
      (setq idc-str "⿱・⿸")
      (setq p1 'above
	    p2 'below)
      )
     ((and idc (eq (encode-char idc '=>ucs@component) #x2FF5))
      (setq idc-str "⿱・⿵")
      (setq p1 'above
	    p2 'below)
      )
     )
    (cond
     (p3
      (format "%s
%s    %s:structure [ a idc:%s ;
%s        %s:%-8s %s
%s        %s:%-8s %s
%s        %s:%-8s %s
%s    ]%s"
	      (if without-head-char
		  ""
		(if (and ccs code-point)
		    (format "%s   # %c"
			    (chise-turtle-format-ccs-code-point ccs code-point)
			    char)
		  "["))
	      indent prefix (or idc-str (char-to-string idc))
	      indent prefix p1 (isd-turtle-format-component c1 ?\; (1+ level) prefix)
	      indent prefix p2 (isd-turtle-format-component c2 ?\; (1+ level) prefix)
	      indent prefix p3 (isd-turtle-format-component c3 ?\  (1+ level) prefix)
	      indent
	      (if without-head-char
		  ""
		(if (null char)
		    (format "\n%s]"
			    indent)
		  "")))
      )
     (idc
      (format "%s
%s    %s:structure [ a idc:%s ;
%s        %s:%-8s %s
%s        %s:%-8s %s
%s    ]%s"
	      (if without-head-char
		  ""
		(if (and ccs code-point)
		    (format "%s   # %c"
			    (chise-turtle-format-ccs-code-point ccs code-point)
			    char)
		  "["))
	      indent prefix (or idc-str (char-to-string idc))
	      indent prefix p1 (isd-turtle-format-component c1 ?\; (1+ level) prefix)
	      indent prefix p2 (isd-turtle-format-component c2 ?\  (1+ level) prefix)
	      indent
	      (if without-head-char
		  ""
		(if (null char)
		    (format "\n%s]"
			    indent)
		  "")))))
    ))

(defun isd-turtle-insert-char (ccs code-point)
  (let ((ret (isd-turtle-format-char ccs code-point)))
    (when ret
      (insert ret)
      (insert " .\n"))))

(defun isd-turtle-insert-ccs-ranges (ccs &rest ranges)
  (let (range code max-code)
    (while ranges
      (setq range (car ranges))
      (cond ((consp range)
	     (setq code (car range)
		   max-code (cdr range))
	     (while (<= code max-code)
	       (isd-turtle-insert-char ccs code)
	       (setq code (1+ code)))
	     )
	    ((integerp range)
	     (isd-turtle-insert-char ccs range)
	     )
	    (t (error 'wrong-type-argument range)))
      (setq ranges (cdr ranges)))))

(defun isd-turtle-dump-range (file path func &rest args)
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8-mcs-er)
          ;; isd-turtle-ccs-list
	  chise-turtle-ccs-prefix-alist)
      (if (file-directory-p path)
	  (setq path (expand-file-name file path)))
      (apply func args)
      (goto-char (point-min))
      ;; (dolist (ccs (sort isd-turtle-ccs-list
      ;;                    #'char-attribute-name<))
      ;;   (insert (format "@prefix %s: <%s%s=> .\n"
      ;;                   (isd-turtle-uri-encode-feature-name ccs)
      ;;                   "http://www.chise.org/est/view/character/"
      ;;                   (www-uri-encode-feature-name ccs))))
      (dolist (cell (sort chise-turtle-ccs-prefix-alist
			  (lambda (a b)
			    (char-attribute-name< (cdr a)(cdr b)))))
	(insert (format "@prefix %s: <%s/%s=> .\n"
			(car cell)
			"http://www.chise.org/est/view/character"
			(www-uri-encode-feature-name (cdr cell)))))
      (insert "\n")
      (goto-char (point-min))
      (insert "# -*- coding: utf-8-mcs-er -*-\n")
      (insert "@prefix : <http://rdf.chise.org/rdf/property/character/isd/> .
@prefix idc: <http://rdf.chise.org/rdf/type/character/idc/> .\n")
      (write-region (point-min)(point-max) path))))

;;;###autoload
(defun isd-turtle-dump-ucs-basic (filename)
  (interactive "Fdump ISD-UCS-Basic : ")
  (isd-turtle-dump-range "ISD-UCS-Basic.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=ucs '(#x4E00 . #x9FA5)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-a (filename)
  (interactive "Fdump ISD-UCS-Ext-A : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-A.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=ucs '(#x3400 . #x4DB5) #xFA1F #xFA23))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-1 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-1 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-1.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x20000 . #x21FFF)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-2 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-2 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-2.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x22000 . #x23FFF)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-3 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-3 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-3.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x24000 . #x25FFF)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-4 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-4 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-4.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x26000 . #x27FFF)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-5 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-5 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-5.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x28000 . #x29FFF)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-b-6 (filename)
  (interactive "Fdump IDS-UCS-Ext-B-6 : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-B-6.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x2A000 . #x2A6D6)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-c (filename)
  (interactive "Fdump IDS-UCS-Ext-C : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-C.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x2A700 . #x2B734)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-d (filename)
  (interactive "Fdump IDS-UCS-Ext-D : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-D.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x2B740 . #x2B81D)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-e (filename)
  (interactive "Fdump IDS-UCS-Ext-E : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-E.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x2B820 . #x2CEA1)))

;;;###autoload
(defun isd-turtle-dump-mj-0 (filename)
  (interactive "Fdump ISD-MJ-0 : ")
  (isd-turtle-dump-range "ISD-MJ-0.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(1 . 9999)))

;;;###autoload
(defun isd-turtle-dump-mj-1 (filename)
  (interactive "Fdump ISD-MJ-1 : ")
  (isd-turtle-dump-range "ISD-MJ-1.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(10000 . 19999)))

;;;###autoload
(defun isd-turtle-dump-mj-2 (filename)
  (interactive "Fdump ISD-MJ-2 : ")
  (isd-turtle-dump-range "ISD-MJ-2.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(20000 . 29999)))

;;;###autoload
(defun isd-turtle-dump-mj-3 (filename)
  (interactive "Fdump ISD-MJ-3 : ")
  (isd-turtle-dump-range "ISD-MJ-3.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(30000 . 39999)))

;;;###autoload
(defun isd-turtle-dump-mj-4 (filename)
  (interactive "Fdump ISD-MJ-4 : ")
  (isd-turtle-dump-range "ISD-MJ-4.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(40000 . 49999)))

;;;###autoload
(defun isd-turtle-dump-mj-5 (filename)
  (interactive "Fdump ISD-MJ-5 : ")
  (isd-turtle-dump-range "ISD-MJ-5.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(50000 . 59999)))

;;;###autoload
(defun isd-turtle-dump-mj-6 (filename)
  (interactive "Fdump ISD-MJ-6 : ")
  (isd-turtle-dump-range "ISD-MJ-6.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 '=mj '(60000 . 69999)))

;;;###autoload
(defun isd-turtle-dump-all (directory)
  (interactive "DISD directory : ")
  (isd-turtle-dump-ucs-basic directory)
  (isd-turtle-dump-ucs-ext-a directory)
  (isd-turtle-dump-ucs-ext-b-1 directory)
  (isd-turtle-dump-ucs-ext-b-2 directory)
  (isd-turtle-dump-ucs-ext-b-3 directory)
  (isd-turtle-dump-ucs-ext-b-4 directory)
  (isd-turtle-dump-ucs-ext-b-5 directory)
  (isd-turtle-dump-ucs-ext-b-6 directory)
  (isd-turtle-dump-ucs-ext-c directory)
  (isd-turtle-dump-ucs-ext-d directory)
  (isd-turtle-dump-ucs-ext-e directory)
  (isd-turtle-dump-mj-0 directory)
  (isd-turtle-dump-mj-1 directory)
  (isd-turtle-dump-mj-2 directory)
  (isd-turtle-dump-mj-3 directory)
  (isd-turtle-dump-mj-4 directory)
  (isd-turtle-dump-mj-5 directory)
  (isd-turtle-dump-mj-6 directory)
  )


;;; @ End.
;;;

(provide 'isd-turtle)

;;; isd-turtle.el ends here
