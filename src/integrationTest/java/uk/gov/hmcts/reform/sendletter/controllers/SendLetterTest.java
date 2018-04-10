package uk.gov.hmcts.reform.sendletter.controllers;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.logging.AppDependency;
import uk.gov.hmcts.reform.sendletter.logging.AppDependencyCommand;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.logging.Dependency;

import java.io.IOException;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@RunWith(SpringRunner.class)
@SpringBootTest
@Transactional
public class SendLetterTest {

    @Autowired
    private MockMvc mvc;

    @SpyBean
    private AppInsights insights;

    @Test
    public void should_return_200_when_single_letter_is_sent() throws Throwable {
        ArgumentCaptor<Dependency> dependencyCaptor = ArgumentCaptor.forClass(Dependency.class);

        MvcResult result = send(readResource("letter.json"))
            .andExpect(status().isOk())
            .andReturn();

        assertThat(result.getResponse().getContentAsString()).isNotNull();
        verify(insights, times(2)).trackDependency(any(ProceedingJoinPoint.class), dependencyCaptor.capture());

        List<Dependency> dependencies = dependencyCaptor.getAllValues();
        List<String> dependencyNames = dependencies.stream().map(Dependency::value).collect(toList());
        List<String> dependencyCommands = dependencies.stream().map(Dependency::command).collect(toList());

        assertThat(dependencyNames).containsExactly(
            AppDependency.AUTH_SERVICE,
            AppDependency.PDF_CLIENT
        );
        assertThat(dependencyCommands).containsExactly(
            AppDependencyCommand.AUTH_SERVICE_HEADER,
            AppDependencyCommand.PDF_CREATE
        );
    }

    @Test
    public void should_return_400_when_bad_letter_is_sent() throws Exception {
        send("").andExpect(status().isBadRequest());

        verifyNoMoreInteractions(insights);
    }

    private ResultActions send(String content) throws Exception {
        MockHttpServletRequestBuilder request =
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);

        return mvc.perform(request);
    }

    private String readResource(final String fileName) throws IOException {
        return Resources.toString(Resources.getResource(fileName), Charsets.UTF_8);
    }
}
