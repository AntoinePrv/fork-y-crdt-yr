# This is the quick start example from yrs, https://docs.rs/yrs/latest/yrs/
for (version in c("v1", "v2")) {
  local({
    test_that(paste("Synchronize two docs", version), {
      doc <- Doc$new()
      text <- doc$get_or_insert_text("article")

      trans <- Transaction$lock(doc, mutable = TRUE)
      text$insert(trans, 0L, "hello")
      text$insert(trans, 5L, " world")
      trans$commit()

      expect_equal(text$get_string(trans), "hello world")
      trans$unlock()

      # Synchronize state with remote replica
      remote_doc <- Doc$new()
      remote_text <- remote_doc$get_or_insert_text("article")

      remote_trans <- Transaction$lock(remote_doc)
      remote_sv_raw <- remote_trans$state_vector()[[paste0("encode_", version)]]()
      remote_trans$unlock()

      # Get update with contents not observed by remote_doc
      local_trans <- Transaction$lock(doc)
      remote_sv <- StateVector[[paste0("decode_", version)]](remote_sv_raw)
      update <- local_trans[[paste0("encode_diff_", version)]](remote_sv)
      local_trans$unlock()

      # Apply update on remote doc
      remote_trans_mut <- Transaction$lock(remote_doc, mutable = TRUE)
      remote_trans_mut[[paste0("apply_update_", version)]](update)
      remote_trans_mut$commit()

      expect_equal(remote_text$get_string(remote_trans_mut), "hello world")
      remote_trans_mut$unlock()
    })
  }, list(version = version))
}
