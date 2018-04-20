FROM postgres:9.6


ADD http://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/5.0.7/flyway-commandline-5.0.7.zip /flyway.zip

RUN unzip /flyway.zip && rm /flyway.zip && mv /flyway-5.0.7 /flyway && ln -s /flyway/flyway /usr/local/bin/flyway

COPY run-flyway.sh /scripts/run-flyway.sh

WORKDIR /flyway

ENTRYPOINT ["/scripts/run-flyway.sh"]
