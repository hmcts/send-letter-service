package uk.gov.hmcts.reform.sendletter.config;

import feign.Client;
import feign.httpclient.ApacheHttpClient;
import org.apache.hc.core5.util.Timeout;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.impl.client.HttpClientBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

/**
 * Configuration for RestTemplate.
 */
@Configuration
public class RestTemplateConfiguration {

    /**
     * Create a Feign HttpClient.
     * @return The Feign HttpClient
     */
    @Bean
    public Client getFeignHttpClient() {
        return new ApacheHttpClient(getHttpClient());
    }

    /**
     * Create a RestTemplate.
     * @return The RestTemplate
     */
    @Bean
    public RestTemplate restTemplate() {
        return new RestTemplate(clientHttpRequestFactory());
    }

    /**
     * Create a HttpComponentsClientHttpRequestFactory.
     * @return The HttpComponentsClientHttpRequestFactory
     */
    @Bean
    public HttpComponentsClientHttpRequestFactory clientHttpRequestFactory() {
        return new HttpComponentsClientHttpRequestFactory(getHttp5Client());
    }

    private org.apache.hc.client5.http.classic.HttpClient getHttp5Client() {
        org.apache.hc.client5.http.config.RequestConfig config =
            org.apache.hc.client5.http.config.RequestConfig.custom()
                .setConnectionRequestTimeout(Timeout.ofSeconds(30))
                .build();

        return org.apache.hc.client5.http.impl.classic.HttpClientBuilder
            .create()
            .useSystemProperties()
            .setDefaultRequestConfig(config)
            .build();
    }

    private HttpClient getHttpClient() {
        RequestConfig config = RequestConfig.custom()
            .setConnectTimeout(30000)
            .setConnectionRequestTimeout(30000)
            .setSocketTimeout(60000)
            .build();

        return HttpClientBuilder
            .create()
            .useSystemProperties()
            .setDefaultRequestConfig(config)
            .build();
    }
}
