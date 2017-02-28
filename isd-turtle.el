;;; isd-turtle.el --- Utility to dump ideographic-structure as Turtle files

;; Copyright (C) 2017 MORIOKA Tomohiko

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
    =adobe-japan1
    =ucs@iso
    =jis-x0208 =jis-x0208@1990
    =jis-x0213-1
    =jis-x0213-1@2000 =jis-x0213-1@2004
    =jis-x0213-2
    =jis-x0212
    =ucs-itaiji-001
    =ucs-itaiji-002
    =ucs-itaiji-003
    =ucs-itaiji-005
    =ucs-var-001
    =gt
    =cns11643-1 =cns11643-2 =cns11643-3
    =cns11643-4 =cns11643-5 =cns11643-6 =cns11643-7
    =gb2312
    =big5-cdp
    =gt-k
    =ucs@unicode
    =ucs@JP/hanazono
    =gb12345
    =zinbun-oracle =>zinbun-oracle
    =daikanwa
    =ruimoku-v6
    =cbeta =jef-china3
    =>jis-x0208 =>jis-x0213-1
    =>jis-x0208@1997
    =>ucs@iwds-1
    =>ucs@component
    =>iwds-1
    =>ucs@iso
    =>ucs@unicode
    =+>ucs@iso =+>ucs@unicode
    =>ucs@jis =>ucs@cns =>ucs@ks
    =>>ucs@iso =>>ucs@unicode
    =>>ucs@jis =>>ucs@cns =>>ucs@ks
    ==mj
    =>>jis-x0208 =>>jis-x0213-1 =>>jis-x0213-2
    =+>jis-x0208 =+>jis-x0213-1 =+>jis-x0213-2
    =+>jis-x0208@1978
    =>>gt
    =+>adobe-japan1
    =>>adobe-japan1
    =jis-x0208@1983 =jis-x0208@1978
    =>ucs-itaiji-005
    ==ucs@unicode
    ==>ucs@bucs
    =big5
    =>cbeta
    ))

(defvar isd-turtle-ccs-list nil)

(defun isd-turtle-uri-encode-feature-name (feature-name)
  (cond
   ((eq '=ucs feature-name)
    "a.ucs")
   ((eq '==>ucs@bucs feature-name)
    "bucs")
   (t
    (mapconcat (lambda (c)
		 (if (eq c ?@)
		     "_"
		   (char-to-string c)))
	       (www-uri-encode-feature-name feature-name)
	       ""))))
		     
;; (defun isd-turtle-encode-char (char)
;;   (let ((ucs (encode-char char '=ucs)))
;;     (if ucs
;;         (format "ucs:0x%04X" ucs)
;;       (www-uri-encode-object char))))

(defun isd-turtle-encode-char (object)
  (let ((ccs-list est-coded-charset-priority-list)
	ccs ret)
    (if (setq ret (encode-char object '=ucs))
	(prog1
	    (format "a.ucs:0x%04X" ret)
	  (unless (memq '=ucs isd-turtle-ccs-list)
	    (setq isd-turtle-ccs-list (cons '=ucs isd-turtle-ccs-list))))
      (while (and ccs-list
		  (setq ccs (pop ccs-list))
		  (not (setq ret (encode-char object ccs 'defined-only)))))
      (cond (ret
	     (unless (memq ccs isd-turtle-ccs-list)
	       (setq isd-turtle-ccs-list (cons ccs isd-turtle-ccs-list)))
	     (format (cond ((memq ccs '(=gt
					=gt-k =daikanwa =adobe-japan1
					=cbeta =zinbun-oracle))
			    "%s:%05d")
			   ((memq ccs '(=hanyo-denshi/ks
					=koseki
					=mj))
			    "%s:%06d")
			   (t
			    "%s:0x%X"))
		     (isd-turtle-uri-encode-feature-name ccs)
		     ret))
	    ((and (setq ccs (car (split-char object)))
		  (setq ret (encode-char object ccs)))
	     (unless (memq ccs isd-turtle-ccs-list)
	       (setq isd-turtle-ccs-list (cons ccs isd-turtle-ccs-list)))
	     (format "%s:0x%X"
		     (isd-turtle-uri-encode-feature-name ccs)
		     ret))
	    (t
	     (format (if est-hide-cgi-mode
			 "system-char-id=0x%X"
		       "system-char-id:0x%X")
		     (encode-char object 'system-char-id))
	     )))))

(defun isd-turtle-format-component (component separator level)
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
			      (isd-turtle-format-char nil (cdr ret) (1+ level)))
		    (isd-turtle-format-char nil (cdr ret) (1+ level)))))))))

(defun isd-turtle-format-char (char &optional ids-list level)
  (unless ids-list
    (setq ids-list (get-char-attribute char 'ideographic-structure)))
  (unless level
    (setq level 0))
  (let ((indent (make-string (* level 4) ?\ ))
	(idc (car ids-list))
	p1 p2 p3
	(c1 (nth 1 ids-list))
	(c2 (nth 2 ids-list))
	(c3 (nth 3 ids-list))
	ret)
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
      ))
    (cond
     (p3
      (format "%s
%s    :structure [ a idc:%c ;
%s        :%-8s %s
%s        :%-8s %s
%s        :%-8s %s
%s    ]%s"
	      (if char
		  (isd-turtle-format-component char ?\  0)
		"[")
	      indent idc
	      indent p1 (isd-turtle-format-component c1 ?\; (1+ level))
	      indent p2 (isd-turtle-format-component c2 ?\; (1+ level))
	      indent p3 (isd-turtle-format-component c3 ?\  (1+ level))
	      indent
	      (if (null char)
		  (format "\n%s]"
			  indent)
		""))
      )
     (idc
      (format "%s
%s    :structure [ a idc:%c ;
%s        :%-8s %s
%s        :%-8s %s
%s    ]%s"
	      (if char
		  (isd-turtle-format-component char ?\  0)
		"[")
	      indent idc
	      indent p1 (isd-turtle-format-component c1 ?\; (1+ level))
	      indent p2 (isd-turtle-format-component c2 ?\  (1+ level))
	      indent
	      (if (null char)
		  (format "\n%s]"
			  indent)
		""))))
    ))

(defun isd-turtle-insert-char (char)
  (let ((ret (isd-turtle-format-char char)))
    (when ret
      (insert ret)
      (insert " .\n"))))

(defun isd-turtle-insert-ccs-ranges (ccs &rest ranges)
  (let (range code max-code char)
    (while ranges
      (setq range (car ranges))
      (cond ((consp range)
	     (setq code (car range)
		   max-code (cdr range))
	     (while (<= code max-code)
	       (if (setq char (decode-char ccs code))
		   (isd-turtle-insert-char char))
	       (setq code (1+ code))))
	    ((integerp range)
	     (if (setq char (decode-char ccs code))
		 (isd-turtle-insert-char char)))
	    (t (error 'wrong-type-argument range)))
      (setq ranges (cdr ranges)))))

(defun isd-turtle-dump-range (file path func &rest args)
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8-mcs-er)
	  isd-turtle-ccs-list)
      (if (file-directory-p path)
	  (setq path (expand-file-name file path)))
      (apply func args)
      (goto-char (point-min))
      (dolist (ccs (sort isd-turtle-ccs-list
			 #'char-attribute-name<))
	(insert (format "@prefix %s: <%s%s=> .\n"
			(isd-turtle-uri-encode-feature-name ccs)
			"http://www.chise.org/est/view/character/"
			(www-uri-encode-feature-name ccs))))
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
			 'ucs '(#x4E00 . #x9FA5)))

;;;###autoload
(defun isd-turtle-dump-ucs-ext-a (filename)
  (interactive "Fdump ISD-UCS-Ext-A : ")
  (isd-turtle-dump-range "ISD-UCS-Ext-A.ttl" filename
			 #'isd-turtle-insert-ccs-ranges
			 'ucs '(#x3400 . #x4DB5) #xFA1F #xFA23))



;;; @ End.
;;;

(provide 'isd-turtle)

;;; isd-turtle.el ends here
