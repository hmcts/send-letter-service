package uk.gov.hmcts.reform.sendletter.transitions;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import net.schmizz.sshj.SSHClient;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.context.ConfigFileApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import uk.gov.hmcts.reform.sendletter.Application;
import uk.gov.hmcts.reform.sendletter.MockSftpServer;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.sendletter.entity.LetterTest;
import uk.gov.hmcts.reform.slc.config.FtpConfigProperties;
import uk.gov.hmcts.reform.slc.logging.AppInsights;
import uk.gov.hmcts.reform.slc.services.steps.sftpupload.FtpClient;

import java.io.IOException;
import java.util.function.Supplier;

import static org.assertj.core.api.Assertions.assertThat;


@RunWith(SpringJUnit4ClassRunner.class)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(classes = Application.class, initializers = ConfigFileApplicationContextInitializer.class)
@DataJpaTest
public class UploadLetterTest {

    @Autowired
    private LetterRepository repository;

    @Before
    public void setup() {
        repository.save(LetterTest.getTestLetter());
    }

    @Rule
    public TemporaryFolder testFolder = new TemporaryFolder();

    @Test
    public void finds_created_letters() {
        int created = repository.findByState(LetterState.Created).size();
        assertThat(created).isEqualTo(1);
        int uploaded = repository.findByState(LetterState.Uploaded).size();
        assertThat(uploaded).isEqualTo(0);
    }

}
