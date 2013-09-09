(ns automat.stream
  (:use
    [potemkin])
  (:import
    [java.nio
     ByteBuffer]
    [automat.utils
     Wrappers$InputStreamWrapper
     Wrappers$ReaderWrapper
     Wrappers$BufferWrapper
     InputStream]))

(def ^:const byte-array-class (class (byte-array 0)))

(defn to-input-stream [x]
  (condp instance? x

    java.io.InputStream
    (Wrappers$InputStreamWrapper. x)

    java.io.Reader
    (Wrappers$ReaderWrapper. x)

    ByteBuffer
    (Wrappers$BufferWrapper. x)

    byte-array-class
    (Wrappers$BufferWrapper. (ByteBuffer/wrap x))))
