(format t "test-+:  ~%")

(deftest test-+ ()
	   (check
	     (= (+ 1 2) 3)
	     (= (+ 1 2 3) 6)
	     (= (+ -1 -3) -4)))

(format t "~a" (test-+))


(format t "~%~%test-*: ~%")

(deftest test-* ()
	   (check
	     (= (* 2 2) 4)
	     (= (* 3 5) 15)
	     (= (* -1 3) 3)))

(format t "~a" (test-*))

(format t "~%~%test-arithmetic: ~%")

(deftest test-arithmetic ()
	   (combine-results
	     (test-+)
	     (test-*)
	     ))

(format t "~a~%~%" (test-arithmetic))
