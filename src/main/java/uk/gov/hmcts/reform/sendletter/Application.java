package uk.gov.hmcts.reform.sendletter;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.hystrix.EnableHystrix;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableHystrix
@EnableAsync
@EnableFeignClients(basePackages = {"uk.gov.hmcts.reform"})
@SuppressWarnings("HideUtilityClassConstructor") // Spring needs a constructor, its not a utility class
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
