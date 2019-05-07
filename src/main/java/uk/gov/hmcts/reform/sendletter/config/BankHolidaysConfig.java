package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.context.annotation.Bean;
import org.springframework.web.client.RestTemplate;
import uk.gov.hmcts.reform.sendletter.services.date.holidays.BankHolidaysClient;

public class BankHolidaysConfig {

    @Bean
    public BankHolidaysClient client(RestTemplate restTemplate) {
        return new BankHolidaysClient(restTemplate, " https://www.gov.uk/bank-holidays");
    }
}
