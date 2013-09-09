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
            int val = stream.read();
            return val == -1 ? eof : val;
        }

        public int nextNumericInput(int eof) throws IOException {
            int val = stream.read();
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
            int val = reader.read();
            return val == -1 ? eof : val;
        }

        public int nextNumericInput(int eof) throws IOException {
            int val = reader.read();
            return val == -1 ? eof : val;
        }
    }

    //
    public static class BufferWrapper implements InputStream {
        java.nio.ByteBuffer buf;

        public BufferWrapper(java.nio.ByteBuffer b) {
            buf = b;
        }

        public Object nextInput(Object eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
        
        public int nextNumericInput(int eof) {
            return (buf.remaining() == 0) ? eof : buf.get();
        }
    }
}
