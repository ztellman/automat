(ns automat.stream
  (:import
    [java.nio
     Buffer
     ByteBuffer
     ShortBuffer
     IntBuffer
     LongBuffer]
    [automat.utils
     Wrappers$InputStreamWrapper
     Wrappers$ReaderWrapper
     Wrappers$ByteBufferWrapper
     Wrappers$ShortBufferWrapper
     Wrappers$IntBufferWrapper
     Wrappers$LongBufferWrapper
     InputStream]))

(def ^:const byte-array-class (class (byte-array 0)))
(def ^:const short-array-class (class (short-array 0)))
(def ^:const int-array-class (class (int-array 0)))
(def ^:const long-array-class (class (long-array 0)))

(defn to-stream [x]
  (condp instance? x

    InputStream
    x

    java.io.InputStream
    (Wrappers$InputStreamWrapper. x)

    java.io.Reader
    (Wrappers$ReaderWrapper. x)

    Buffer
    (condp instance? x
      ByteBuffer (Wrappers$ByteBufferWrapper. x)
      ShortBuffer (Wrappers$ShortBufferWrapper. x)
      IntBuffer (Wrappers$IntBufferWrapper. x)
      LongBuffer (Wrappers$LongBufferWrapper. x))

    (if (.isArray (class x))
      (condp instance? x
        byte-array-class
        (Wrappers$ByteBufferWrapper. (ByteBuffer/wrap x))

        short-array-class
        (Wrappers$ShortBufferWrapper. (ShortBuffer/wrap x))

        int-array-class
        (Wrappers$IntBufferWrapper. (IntBuffer/wrap x))

        long-array-class
        (Wrappers$LongBufferWrapper. (LongBuffer/wrap x)))

      (let [s (atom (seq x))]
        (reify InputStream
          (nextInput [_ eof]
            (let [s' @s]
              (if (empty? s')
                eof
                (let [x (first s')]
                  (swap! s rest)
                  x))))
          (nextNumericInput [_ eof]
            (let [s' @s]
              (if (empty? s')
                eof
                (let [x (first s')]
                  (swap! s rest)
                  x)))))))))
