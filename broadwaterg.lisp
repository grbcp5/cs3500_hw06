;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:
;;;   broadwaterg.lisp
;;;
;;; Author:
;;;   Grant Broadwater
;;;
;;; Description:
;;;   Deliverable for the MST F17 CS3500
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allBlackRow(size)
  "Returns an all black row"
  (let* ((row ()))
    (dotimes (i size)
      (setf row 
        (cons 'b row)
      )
    )
  row
  )
)

(defun printRow(row)
  "Prints a row of red/black cells"
  (format t "(~{#\\~a~})~%" row)
)

(defun placeRedBlockUnit (
  minRedBlockUnitSize 
  redBlockUnitSize 
  rowSize 
  start 
  configurations 
  prevConfig
  )
  "Places all possible red block units possible starting with given configuration"
  (setf newConfig (copy-list prevConfig))
  (setf (nth 1 newConfig) 'r)
  (printRow prevConfig)
  (printRow newConfig)
)

(defun placeRedBlocks (size) 
  "Returns a list of all the possible red block combinations in a row"
  
  (defvar allBlack (allBlackRow size))
  
  (placeRedBlockUnit
    0
    0
    0
    0
    ()
    allBlack
  ) 
)

(write-line "Hello, world!")
(placeRedBlocks 7)

;;; Exit out of gcl
(quit)
