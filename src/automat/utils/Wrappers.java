package automat.utils;

import java.io.IOException;


public class Wrappers {

    //
    public static class InputStreamWrapper implements InputStream {
        java.io.InputStream stream;

        public InputStreamWrapper(java.io.InputStream s) {
            stream = s;
        }

        public Object nextInput(Object eof) throws IOException {
            long val = stream.read();
            return val == -1 ? eof : val;
        }

        public long nextNumericInput(long eof) throws IOException {
            long val = stream.read();
            return val == -1 ? eof : val;
        }
    }

    //
    public static class ReaderWrapper implements InputStream {
        java.io.Reader reader;

        public ReaderWrapper(java.io.Reader r) {
            reader = r;
        }

        public Object nextInput(Object eof) throws IOException {
            long val = reader.read();
            return val == -1 ? eof : val;
        }

        public long nextNumericInput(long eof) throws IOException {
            long val = reader.read();
            return val == -1 ? eof : val;
        }
    }

    //
    public static class ByteBufferWrapper implements InputStream {
        java.nio.ByteBuffer buf;

        public ByteBufferWrapper(java.nio.ByteBuffer b) {
            buf = b;
        }

        public Object nextInput(Object eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
        
        public long nextNumericInput(long eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
    }

    public static class ShortBufferWrapper implements InputStream {
        java.nio.ShortBuffer buf;

        public ShortBufferWrapper(java.nio.ShortBuffer b) {
            buf = b;
        }

        public Object nextInput(Object eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
        
        public long nextNumericInput(long eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
    }

    public static class IntBufferWrapper implements InputStream {
        java.nio.IntBuffer buf;

        public IntBufferWrapper(java.nio.IntBuffer b) {
            buf = b;
        }

        public Object nextInput(Object eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
        
        public long nextNumericInput(long eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
    }

    public static class LongBufferWrapper implements InputStream {
        java.nio.LongBuffer buf;

        public LongBufferWrapper(java.nio.LongBuffer b) {
            buf = b;
        }

        public Object nextInput(Object eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
        
        public long nextNumericInput(long eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
    }

}
