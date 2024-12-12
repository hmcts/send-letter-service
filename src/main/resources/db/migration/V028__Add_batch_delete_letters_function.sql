CREATE OR REPLACE FUNCTION batch_delete_letters(batch_size INT)
RETURNS INT AS
$$
DECLARE
rows_deleted INT;
BEGIN
  -- Adding manual locks but have to lock each manually since we using delete cascade.
  -- lock released automatically at end of transaction/commit.
	LOCK TABLE letters, documents, letter_events IN ACCESS EXCLUSIVE MODE;

  -- Perform deletion in a batch and get the number of rows deleted
  WITH letters_to_delete AS (
    SELECT id -- Selecting indexed id column is all we need, for better performance
    FROM letters
    WHERE created_at < (
      CASE
        WHEN service = 'civil_general_applications' THEN NOW() - INTERVAL '6 YEARS'
        WHEN service = 'civil_service' THEN NOW() - INTERVAL '6 YEARS'
        WHEN service = 'cmc_claim_store' THEN NOW() - INTERVAL '2 YEARS'
        WHEN service = 'divorce_frontend' THEN NOW() - INTERVAL '3 MONTHS'
        WHEN service = 'finrem_case_orchestration' THEN NOW() - INTERVAL '3 MONTHS'
        WHEN service = 'finrem_document_generator' THEN NOW() - INTERVAL '3 MONTHS'
        WHEN service = 'fpl_case_service' THEN NOW() - INTERVAL '2 YEARS'
        WHEN service = 'nfdiv_case_api' THEN NOW() - INTERVAL '3 MONTHS'
        WHEN service = 'prl_cos_api' THEN NOW() - INTERVAL '18 YEARS'
        WHEN service = 'probate_backend' THEN NOW() - INTERVAL '1 YEAR'
        WHEN service = 'send_letter_tests' THEN NOW() - INTERVAL '2 YEARS'
        WHEN service = 'sscs' THEN NOW() - INTERVAL '3 MONTHS'
        -- no default case
        END
      )
      -- For below don't delete old unprocessed / things that need investigating at some point
      AND status IN ('Posted', 'PostedLocally', 'Aborted', 'Skipped')
    ORDER BY created_at ASC -- Prioritize oldest rows first
    LIMIT batch_size -- Limit the number of rows selected in each batch
    )
  DELETE FROM letters
  USING letters_to_delete
  WHERE letters.id = letters_to_delete.id;

  -- Get the number of rows deleted by the DELETE statement
  GET DIAGNOSTICS rows_deleted = ROW_COUNT;

  -- Return the number of rows deleted
  RETURN rows_deleted;
END;
$$ LANGUAGE plpgsql;
