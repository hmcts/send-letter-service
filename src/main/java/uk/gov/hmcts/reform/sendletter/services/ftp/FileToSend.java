package uk.gov.hmcts.reform.sendletter.services.ftp;

import net.schmizz.sshj.xfer.InMemorySourceFile;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

/**
 * This class represents the file to send.
 */
public class FileToSend extends InMemorySourceFile {

    public final String filename;
    public final byte[] content;
    public final boolean isSmokeTest;

    /**
     * Constructor.
     *
     * @param filename the filename
     * @param content the content
     * @param isSmokeTest the is smoke test
     */
    public FileToSend(String filename, byte[] content, boolean isSmokeTest) {
        this.filename = filename;
        this.content = content;
        this.isSmokeTest = isSmokeTest;
    }

    @Override
    public String getName() {
        return this.filename;
    }

    @Override
    public long getLength() {
        return this.content.length;
    }

    @Override
    public InputStream getInputStream() {
        return new ByteArrayInputStream(this.content);
    }
}
