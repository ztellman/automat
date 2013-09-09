package automat.utils;

import java.io.IOException;

public interface InputStream {
    Object nextInput(Object eof) throws IOException;
    int nextNumericInput(int eof) throws IOException;
}
