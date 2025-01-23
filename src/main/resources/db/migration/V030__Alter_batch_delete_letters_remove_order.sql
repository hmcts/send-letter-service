CREATE OR REPLACE FUNCTION batch_delete_letters(
  batch_size INT,
  civil_general_applications_interval INTERVAL,
  civil_service_interval INTERVAL,
  cmc_claim_store_interval INTERVAL,
  divorce_frontend_interval INTERVAL,
  finrem_case_orchestration_interval INTERVAL,
  finrem_document_generator_interval INTERVAL,
  fpl_case_service_interval INTERVAL,
  nfdiv_case_api_interval INTERVAL,
  prl_cos_api_interval INTERVAL,
  probate_backend_interval INTERVAL,
  send_letter_tests_interval INTERVAL,
  sscs_interval INTERVAL
)
RETURNS INT AS
$$
DECLARE
rows_deleted INT;

BEGIN
  -- Adding manual locks but have to lock each manually since we are using DELETE CASCADE.
  -- Lock is released automatically at the end of transaction/commit.
  LOCK
TABLE letters, documents, letter_events IN ACCESS EXCLUSIVE MODE;

-- Perform deletion in a batch and get the number of rows deleted
WITH letters_to_delete AS (
  SELECT id -- Selecting indexed id column is all we need, for better performance
  FROM letters
  WHERE created_at < (
    CASE
      WHEN service = 'civil_general_applications' THEN NOW() - civil_general_applications_interval
      WHEN service = 'civil_service' THEN NOW() - civil_service_interval
      WHEN service = 'cmc_claim_store' THEN NOW() - cmc_claim_store_interval
      WHEN service = 'divorce_frontend' THEN NOW() - divorce_frontend_interval
      WHEN service = 'finrem_case_orchestration' THEN NOW() - finrem_case_orchestration_interval
      WHEN service = 'finrem_document_generator' THEN NOW() - finrem_document_generator_interval
      WHEN service = 'fpl_case_service' THEN NOW() - fpl_case_service_interval
      WHEN service = 'nfdiv_case_api' THEN NOW() - nfdiv_case_api_interval
      WHEN service = 'prl_cos_api' THEN NOW() - prl_cos_api_interval
      WHEN service = 'probate_backend' THEN NOW() - probate_backend_interval
      WHEN service = 'send_letter_tests' THEN NOW() - send_letter_tests_interval
      WHEN service = 'sscs' THEN NOW() - sscs_interval
      -- no default case
    END
  )
  -- For below don't delete old unprocessed / things that need investigating at some point
  AND status IN ('Posted', 'PostedLocally', 'Aborted', 'Skipped', 'Uploaded')
  LIMIT batch_size -- Limit the number of rows selected in each batch
)
DELETE
FROM letters USING letters_to_delete
WHERE letters.id = letters_to_delete.id;

-- Get the number of rows deleted by the DELETE statement
GET DIAGNOSTICS rows_deleted = ROW_COUNT;

RETURN rows_deleted;
END;
$$ LANGUAGE plpgsql;
