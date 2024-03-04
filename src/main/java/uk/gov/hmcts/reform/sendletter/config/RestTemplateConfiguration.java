package uk.gov.hmcts.reform.sendletter.config;

import feign.Client;
import feign.hc5.ApacheHttp5Client;
import org.apache.hc.client5.http.config.ConnectionConfig;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.io.BasicHttpClientConnectionManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.TimeUnit;

@Configuration
public class RestTemplateConfiguration {

    @Bean
    public Client getFeignHttpClient() {
        return new ApacheHttp5Client(getHttpClient());
    }

    @Bean
    public RestTemplate restTemplate() {
        return new RestTemplate(clientHttpRequestFactory());
    }

    @Bean
    public HttpComponentsClientHttpRequestFactory clientHttpRequestFactory() {
        return new HttpComponentsClientHttpRequestFactory(getHttpClient());
    }

    private CloseableHttpClient getHttpClient() {
        ConnectionConfig connectionConfig = ConnectionConfig.custom()
            .setConnectTimeout(30000, TimeUnit.MILLISECONDS)
            .setSocketTimeout(60000, TimeUnit.MILLISECONDS)
            .build();

        RequestConfig requestConfig = RequestConfig.custom()
            .setConnectionRequestTimeout(30000, TimeUnit.MILLISECONDS)
            .build();

        BasicHttpClientConnectionManager cm = new BasicHttpClientConnectionManager();
        cm.setConnectionConfig(connectionConfig);

        return HttpClientBuilder.create()
            .setDefaultRequestConfig(requestConfig)
            .setConnectionManager(cm)
            .build();
    }
}
