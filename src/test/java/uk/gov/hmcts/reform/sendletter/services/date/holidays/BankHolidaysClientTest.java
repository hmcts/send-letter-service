package uk.gov.hmcts.reform.sendletter.services.date.holidays;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import uk.gov.hmcts.reform.sendletter.services.date.holidays.response.Event;
import uk.gov.hmcts.reform.sendletter.services.date.holidays.response.Holidays;

import java.time.LocalDate;

import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.notFound;
import static com.github.tomakehurst.wiremock.client.WireMock.okJson;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

public class BankHolidaysClientTest {

    @Rule
    public WireMockRule api = new WireMockRule();

    private BankHolidaysClient client;

    @Before
    public void setUp() {
        this.client = new BankHolidaysClient(new RestTemplate(), "http://localhost:8080");
    }

    @Test
    public void should_fetch_holidays() {
        // given
        api.stubFor(get("/").willReturn(
            okJson("{"
                + "\"division\": \" england-and-wales\","
                + "\"events\": ["
                + "  {"
                + "    \"title\": \"Good Friday\","
                + "    \"date\": \"2012-04-06\""
                + "  },"
                + "  {"
                + "    \"title\": \"Easter Monday\","
                + "    \"date\": \"2012-04-09\""
                + "  }"
                + "]"
                + "}"
            )));

        // when
        Holidays holidays = client.getForEngland();

        // then
        assertThat(holidays).isNotNull();

        assertThat(holidays.events)
            .usingRecursiveFieldByFieldElementComparator()
            .containsExactly(
                new Event(LocalDate.of(2012, 4, 6), "Good Friday"),
                new Event(LocalDate.of(2012, 4, 9), "Easter Monday")
            );
    }

    @Test
    public void should_throw_exception_if_error_occurred() {
        // given
        api.stubFor(get("/").willReturn(notFound()));

        // when
        Throwable exc = catchThrowable(client::getForEngland);

        // then
        assertThat(exc).isInstanceOf(RestClientException.class);
    }
}
