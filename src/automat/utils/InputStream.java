package automat.utils;

import java.io.IOException;

public interface InputStream {
    Object nextInput(Object eof) throws IOException;
    long nextNumericInput(long eof) throws IOException;
}
