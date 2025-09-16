(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype cell () '(unsigned-byte 4))
(deftype mask () '(unsigned-byte 16))
(defconstant +digits+ #b111111111)

(defmacro cell (b i j) `(aref ,b ,i ,j))
(defmacro digit->mask (d) `(ash 1 (1- ,d)))
(defmacro single-bit-p (m) `(= 1 (logcount ,m)))
(defmacro mask->digit (m) `(integer-length ,m))

(defmacro do-cells ((i j &optional (n 9)) &body body)
  `(dotimes (,i ,n) (dotimes (,j ,n) ,@body)))

(defmacro do-box ((r c i j) &body body)
  (let ((r0 (gensym)) (c0 (gensym)))
    `(let* ((,r0 (* 3 (floor ,i 3))) (,c0 (* 3 (floor ,j 3))))
       (loop for ,r from ,r0 below (+ ,r0 3) do
         (loop for ,c from ,c0 below (+ ,c0 3) do ,@body)))))

(defmacro do-fixpoint ((changed) &body body)
  `(loop with ,changed = t
         while ,changed do (setf ,changed nil) ,@body
         finally (return t)))

(defmacro do-set-bits ((d m) &body body)
  (let ((mm (gensym "MM")) (lb (gensym "LB")))
    `(do ((,mm ,m (logand ,mm (1- ,mm)))) ((zerop ,mm))
       (let* ((,lb (logand ,mm (- ,mm)))
              (,d  (integer-length ,lb))) ,@body))))

(defmacro when-let ((var expr) &body body)
  `(let ((,var ,expr)) (when ,var ,@body)))

(defmacro with-mask ((m) &body body) `(let ((,m 0)) ,@body ,m))
(defmacro ior! (place expr) `(setf ,place (logior ,place ,expr)))

(defmacro with-sudoku ((board rows) &body body)
  `(let ((,board (make-board ,rows))) ,@body))

(defun make-board (rows)
  (let ((b (make-array '(9 9) :element-type 'cell)))
    (do-cells (i j) (setf (cell b i j) (nth j (nth i rows))))
    b))

(defun row-mask (b i)
  (with-mask (m)
    (dotimes (j 9)
      (let ((v (cell b i j)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun col-mask (b j)
  (with-mask (m)
    (dotimes (i 9)
      (let ((v (cell b i j)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun box-mask (b i j)
  (with-mask (m)
    (do-box (r c i j)
      (let ((v (cell b r c)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun candidates-mask (b i j)
  (let ((v (cell b i j)))
    (logandc2 +digits+ (logior (row-mask b i)
                               (col-mask b j)
                               (box-mask b i j)))))

(defun propagate (b)
  (do-fixpoint (changed)
    (do-cells (i j)
      (when (zerop (cell b i j))
        (let ((m (candidates-mask b i j)))
          (when (zerop m) (return-from propagate nil))
          (when (single-bit-p m)
            (setf (cell b i j) (mask->digit m)
                  changed t))))))
  t)

(defun solved-p (b)
  (loop for i below 9 always (loop for j below 9 always (> (cell b i j) 0))))

(defun find-mrv (b)
  (let ((best-i nil)
        (best-j nil)
        (best-m 0)
        (best-k 10))
    (do-cells (i j)
      (when (zerop (cell b i j))
        (let* ((m (candidates-mask b i j))
               (k (logcount m)))
          (when (< k best-k)
            (setf best-k k best-i i best-j j best-m m)))))
    (values best-i best-j best-m)))

(defun copy-board (b)
  (let ((nb (make-array '(9 9) :element-type 'cell)))
    (do-cells (i j) (setf (cell nb i j) (cell b i j)))
    nb))

(defun solve (b)
  (unless (propagate b) (return-from solve nil))
  (when (solved-p b) (return-from solve b))
  (multiple-value-bind (i j m) (find-mrv b)
    (when (null i) (return-from solve nil))
    (do-set-bits (d m)
      (let ((nb (copy-board b)))
        (setf (cell nb i j) d)
        (when-let (res (solve nb))
          (return-from solve res))))
    nil))

(defun print-board (b &optional (out *standard-output*))
  (dotimes (i 9)
    (dotimes (j 9)
      (format out "~D~:[ ~;~]" (aref b i j) (= j 8)))
    (terpri out)))


(defparameter *puzzle*
  '((3 0 6 5 0 8 4 0 0)
    (5 2 0 0 0 0 0 0 0)
    (0 8 7 0 0 0 0 3 1)
    (0 0 3 0 1 0 0 8 0)
    (9 0 0 8 6 3 0 0 5)
    (0 5 0 0 9 0 6 0 0)
    (1 3 0 0 0 0 2 5 0)
    (0 0 0 0 0 0 0 7 4)
    (0 0 5 2 0 6 3 0 0)))

(defun main ()
  (with-sudoku (b *puzzle*)
    (let ((res (solve b)))
      (if res
          (print-board res)
          (progn (format t "unsatisfiable~%")
                 #+sbcl (sb-ext:exit :code 1))))))

(main)
