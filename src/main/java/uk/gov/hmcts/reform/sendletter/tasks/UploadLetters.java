package uk.gov.hmcts.reform.sendletter.tasks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.FileNameHelper;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.PdfDoc;
import uk.gov.hmcts.reform.slc.services.steps.sftpupload.FtpClient;

import java.sql.Timestamp;
import java.time.Instant;

public class UploadLetters {
    private final LetterRepository repo;
    private final FtpClient ftp;
    private static final Logger logger = LoggerFactory.getLogger(UploadLetters.class);

    @Autowired
    public UploadLetters(LetterRepository repo, FtpClient ftp) {
        this.repo = repo;
        this.ftp = ftp;
    }

    @Transactional
    public void run() {
        repo.findByState(LetterState.Created).forEach(this::process);
    }

    public void process(Letter letter) {
        String name = FileNameHelper.generateName(letter, "pdf", letter.getService(), letter.getId().toString());
        logger.debug("Uploading letter {}, messageId {}, filename {}",
            letter.getId(), letter.getMessageId(), name);
        PdfDoc doc = new PdfDoc(name, letter.getPdf());

        ftp.upload(doc);

        logger.debug("Successfully uploaded letter {}", letter.getId());

        letter.setState(LetterState.Uploaded);
        letter.setSentToPrintAt(Timestamp.from(Instant.now()));
    }
}
