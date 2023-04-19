package uk.gov.hmcts.reform.sendletter.model;

import net.schmizz.sshj.xfer.InMemoryDestFile;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class InMemoryDownloadedFile extends InMemoryDestFile {

    private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

    @Override
    public long getLength() {
        return outputStream.size();
    }

    @Override
    public OutputStream getOutputStream() {
        return outputStream;
    }

    @Override
    public OutputStream getOutputStream(boolean append) throws IOException {
        return outputStream;
    }

    public byte[] getBytes() {
        return outputStream.toByteArray();
    }
}
