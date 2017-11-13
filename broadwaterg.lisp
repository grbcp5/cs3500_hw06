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

(defun printAllRows(list)
  "Prints each row in the list"
  (loop for row in list
    do (printRow row)
  )
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
      (setf allConfigs (cons config allConfigs))
      ;(printRow config)

      (loop for otherUnitSize from minRedBlockUnitSize to rowSize by 1 do
        (placeRedBlockUnit
          minRedBlockUnitSize
          otherUnitSize
          rowSize
          (+ start redBlockUnitSize 1)
          config
        )
      )
    )
  )
)

(defun placeRedBlocks (rowSize) 
  "Returns a list of all the possible red block combinations in a row"
  
  (defvar allBlack (allBlackRow rowSize))
  (defvar minRedBlockUnitSize 3)
  (defvar allConfigs (cons allBlack nil))

  ;(printRow allBlack)
  
  (defvar unitSize 3)
   (loop for unitSize from minRedBlockUnitSize to rowSize by 1 do
    (loop for start from 0 to rowSize by 1 do
      (progn  
        (placeRedBlockUnit 
          minRedBlockUnitSize
          unitSize
          rowSize
          start
          allBlack
        )
      )
    )
  )

  (return-from placeRedBlocks allConfigs)  
)

;(write-line "Hello, world!")
;(print (length (placeRedBlocks 12)))

;;; Exit out of gcl
;(quit)
