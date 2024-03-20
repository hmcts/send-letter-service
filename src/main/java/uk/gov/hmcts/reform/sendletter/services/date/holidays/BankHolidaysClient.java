package uk.gov.hmcts.reform.sendletter.services.date.holidays;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import uk.gov.hmcts.reform.sendletter.services.date.holidays.response.Holidays;

/**
 * This class represents the bank holidays client.
 */
@Component
public class BankHolidaysClient {

    private static final String DEFAULT_URL = "https://www.gov.uk/bank-holidays/england-and-wales.json";

    private final RestTemplate restTemplate;
    private final String url;

    /**
     * Constructor.
     *
     * @param restTemplate the rest template
     */
    @Autowired
    public BankHolidaysClient(RestTemplate restTemplate) {
        this(restTemplate, DEFAULT_URL);
    }

    /**
     * Constructor.
     *
     * @param restTemplate the rest template
     * @param url the url
     */
    public BankHolidaysClient(RestTemplate restTemplate, String url) {
        this.restTemplate = restTemplate;
        this.url = url;
    }

    /**
     * Get the holidays.
     *
     * @return the holidays
     */
    public Holidays getHolidays() {
        return restTemplate.getForObject(url, Holidays.class);
    }
}
