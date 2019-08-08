(define-library (suffix-array sa-is)
   (import 
     (scheme write)
     (scheme base))
   (export make-suffix-array make-suffix-array-u8)

   (begin
    (define (vector-for-each-with-index lmd . vs)
     (let* ((min-length (apply min (map vector-length vs))))
       (let loop ((i 0))
         (unless (= i min-length)
               (apply 
                 lmd 
                 (cons i 
                    (map 
                      (lambda (x)
                        (vector-ref x i))
                      vs)))
           (loop (+ i 1))))))

     (define (inducted-sort vs char-kinds sl-vector lms-vector vs-ref vs-length)
       (let ((res (make-vector (vs-length vs) #f))
             (bin (make-bin vs char-kinds vs-ref vs-length)))
          ;step1
         (let ((counts (make-vector char-kinds 1)))
           (let loop ((i (- (vector-length lms-vector) 1)))
             (when (>= i 0)
               (let* ((index (vector-ref lms-vector i))
                      (char-code (vs-ref vs index)))
                 (vector-set! 
                   res
                   (- (vector-ref bin (+ char-code 1))
                      (vector-ref counts char-code))
                   index)
                 (vector-set!
                   counts
                   char-code
                   (+ (vector-ref counts char-code) 1)))
               (loop (- i 1)))))

          ;step2
          (let ((counts (make-vector char-kinds 0)))
            (let loop ((i 0))
              (unless (= i (vector-length sl-vector))
                (let ((j (vector-ref res i)))
                   (when (and 
                           j
                           (not (zero? j))
                           (eq? (vector-ref sl-vector (- j 1)) 'L))
                     (let ((char-code (vs-ref vs (- j 1))))
                       (vector-set! 
                         res
                         (+ (vector-ref bin char-code)
                            (vector-ref counts char-code))
                         (- j 1))
                       (vector-set!
                         counts
                         char-code
                         (+ (vector-ref counts char-code) 1))))
                   (loop (+ i 1))))))

          ;step3
          (let ((counts (make-vector char-kinds 1)))
            (let loop ((i (- (vector-length sl-vector) 1)))
              (unless (= i -1)
                (let ((j (vector-ref res i)))
                   (when (and j
                              (not (zero? j))
                              (eq? (vector-ref sl-vector (- j 1)) 'S))
                     (let ((char-code (vs-ref vs (- j 1))))
                       (vector-set!
                         res
                         (- (vector-ref bin (+ char-code 1))
                            (vector-ref counts char-code))
                         (- j 1))

                       (vector-set!
                         counts
                         char-code
                         (+ (vector-ref counts char-code) 1)))))
                     (loop (- i 1)))))
          res))

     (define (make-sl-vector vs vs-ref vs-length)
       (let ((res (make-vector (vs-length vs) 'L)))
         (let loop ((i (- (vs-length vs) 1)))
           (unless (zero? i)
             (cond
               ((or (= (vs-ref vs i) 0)
                    (< (vs-ref vs i)
                       (vs-ref vs (+ i 1))))
                (vector-set! 
                  res
                  i
                  'S))
               ((= (vs-ref vs i)
                   (vs-ref vs (+ i 1)))
                (vector-set!
                  res
                  i
                  (vector-ref res (+ i 1)))))
             (loop (- i 1))))
         res))

      (define (lms? sl-vector index)
        (and 
          (> index  0)
          (eq? (vector-ref sl-vector (- index 1)) 'L)
          (eq? (vector-ref sl-vector index ) 'S)))

      (define (make-left-most-s-vector sl)
        (let ((res '()))
           (let loop ((i 1))
             (unless (= (vector-length sl) i)
               (when (lms? sl i)
                 (set! res (cons i res)))
               (loop (+ i 1))))
           (list->vector (reverse res))));TODO*

      (define (make-bin vs char-kind vs-ref vs-length)
        (let ((counts (make-vector char-kind 0)))
          (let loop ((i 0))
            (unless (= i (vs-length vs))
              (let ((c (vs-ref vs i)))
                 (vector-set!
                   counts
                   c
                   (+ (vector-ref counts c)
                      1))
                  (loop (+ i 1)))))

          (let ((accm 0))
            (vector-map
              (lambda (n)
                (let ((prev-accm accm))
                  (set! accm (+ n accm))
                  prev-accm))
              counts))))


     (define (make-suffix-array-aux vs char-kind vs-ref vs-length)
       (let* ((sl-vector (make-sl-vector vs vs-ref vs-length))
              (lms-vector (make-left-most-s-vector sl-vector))

                 (new-lms-vector (make-vector (vector-length lms-vector)))
              )
         (let ((pre-res (inducted-sort vs char-kind sl-vector lms-vector vs-ref vs-length)))
           (let (
                 (i 0)
                 (j 0))
             (vector-for-each
               (lambda (suffix)
                 (when (lms? sl-vector suffix)
                   (vector-set! 
                     new-lms-vector 
                     j
                     suffix)
                   (set! j (+ j 1)))
                 (vector-set! pre-res i #f)
                 (set! i (+ i 1)))
               pre-res)
             )


           (vector-set! pre-res (vector-ref new-lms-vector 0) 0)
           (let loop1 ((i 0)
                       (lms-index 1))
             (cond 
               ((< i (- (vector-length lms-vector) 1))
                  (let ((first (vector-ref  new-lms-vector i))
                        (second (vector-ref new-lms-vector (+ i 1))))

                    (let loop2 ((j 0))
                      (cond 
                        ;Unique lms-char
                        ((or (not 
                                   (= 
                                     (vs-ref 
                                       vs 
                                       (+ first j))
                                     (vs-ref
                                       vs
                                       (+ second j))))
                                (not 
                                  (eq? 
                                    (lms? sl-vector (+ first j))
                                    (lms? sl-vector (+ second j)))))
                            (begin
                              (vector-set! 
                                pre-res 
                                second
                                lms-index)
                              (loop1 (+ i 1) (+ lms-index 1))
                              ))

                        ;Duplicated lms-char
                        ((and (not (zero? j))
                              (or (lms? sl-vector (+ first j))
                                  (lms? sl-vector (+ second j))))
                           (vector-set!
                             pre-res
                             second
                             (- lms-index 1))
                           (loop1 (+ i 1) lms-index))
                        (else 
                          (loop2 (+ j 1)))))))
               (else 
                  (let* ((new-lms-string 
                           (make-vector (vector-length lms-vector) 0))
                         (index 0))
                     (vector-for-each
                        (lambda (c)
                          (when c
                            (vector-set! 
                              new-lms-string
                              index
                              c)
                            (set! index (+ index 1))))
                        pre-res)
                     (let ((new-lms-string-suffix-array
                       (cond 
                         ((< lms-index (vector-length new-lms-string))
                          (make-suffix-array-aux 
                            new-lms-string
                            lms-index
                            vector-ref
                            vector-length)) 
                         (else
                           (let ((tmp (make-vector (vector-length new-lms-string))))
                             (vector-for-each-with-index 
                               (lambda (index k)
                                 (vector-set! tmp k index))
                               new-lms-string)
                             tmp)
                           ))))

                         
                       (inducted-sort
                         vs
                         char-kind
                         sl-vector
                         (vector-map (lambda (x) (vector-ref lms-vector x)) new-lms-string-suffix-array)
                         vs-ref
                         vs-length)
                       ))))))))

     (define (string->utf8-with-zero-char s)
       (let ((res (make-bytevector (+ (string-length s) 1))))
         (let loop ((i 0))
           (when (< i (string-length s))
             (bytevector-u8-set! res i (char->integer (string-ref s i)))
             (loop (+ i 1))))
         (bytevector-u8-set! res  (string-length s) 0)
         res))

     (define (make-suffix-array s)
       (let ((utf8-s (string->utf8-with-zero-char s)))
         (make-suffix-array-aux 
           utf8-s 
           256 
           bytevector-u8-ref 
           bytevector-length)))

     (define (make-suffix-array-u8 utf8-s)
       (make-suffix-array-aux
         utf8-s
         256
         bytevector-u8-ref
         bytevector-length))
     ))
