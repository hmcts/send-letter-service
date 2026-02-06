package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;

public interface ReportRepository extends JpaRepository<Report, UUID> {

    Optional<Report> findFirstByServiceAndReportDate(String service, LocalDate reportDate);
}


