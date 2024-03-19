package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

/**
 * Print repository.
 */
public interface PrintRepository extends JpaRepository<Print, UUID> {

    /**
     * Count by status.
     * @param status The status
     * @return The count
     */
    int countByStatus(PrintStatus status);
}
