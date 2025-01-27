; Author: Jay Bhatia
; CSCI 330 - Homework 1
; Date: 01/26/2025

;; Define example lists for extraction
(defparameter *listA* '(a b x d))         ; Simple flat list
(defparameter *listB* '(a (b (x d))))     ; List with nesting
(defparameter *listC* '(((a (b (x) d))))) ; Deeply nested structure

;; Extracting 'x' using CAR/CDR
(format t "~%Finding 'x' from different lists:")
(format t "~%From listA: ~A" (caddr *listA*))  
(format t "~%From listB: ~A" (car (car (cdr (car (cdr *listB*))))))  
(format t "~%From listC: ~A" (car (car (cdr (car (cdr (car (car *listC*))))))))  

;; Rebuilding lists using cons
(format t "~%~%Recreating lists with cons:")

(defparameter *listA-cons* (cons 'a (cons 'b (cons 'x (cons 'd nil)))))
(format t "~%ListA (reconstructed): ~A" *listA-cons*)  

(defparameter *listB-cons* (cons 'a (cons (cons 'b (cons (cons 'x (cons 'd nil)) nil)) nil)))
(format t "~%ListB (reconstructed): ~A" *listB-cons*)  

(defparameter *listC-cons* (cons (cons (cons 'a (cons (cons 'b (cons (cons 'x nil) (cons 'd nil))) nil)) nil) nil))
(format t "~%ListC (reconstructed): ~A" *listC-cons*)  
