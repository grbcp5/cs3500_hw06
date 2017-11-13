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
  prevConfig
  )
  "Places all possible red block units possible starting with given configuration"
  (setf config (copy-list prevConfig))

  (if (>= rowSize (+ redBlockUnitSize start))
    (progn
      (loop for pos from 0 to (- redBlockUnitSize 1) by 1 do
        (setf (nth (+ start pos) config) 'r)
      )
      (printRow config)
    )
  )

)

(defun placeRedBlocks (rowSize) 
  "Returns a list of all the possible red block combinations in a row"
  
  (defvar allBlack (allBlackRow rowSize))
  (defvar minRedBlockUnitSize 3)

  (printRow allBlack)

  
  (defvar unitSize 3)
   ;(loop for unitSize from minRedBlockUnitSize to rowSize by 1 do
    (loop for start from 0 to rowSize by 1 do
      (placeRedBlockUnit 
        minRedBlockUnitSize
        unitSize
        rowSize
        start
        allBlack
      )
    )
  ;)

)

(write-line "Hello, world!")
(placeRedBlocks 7)

;;; Exit out of gcl
(quit)
