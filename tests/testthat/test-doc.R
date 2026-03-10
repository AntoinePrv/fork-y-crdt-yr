test_that("ArDoc can be created", {
  doc <- ArDoc$new()
  expect_true(inherits(doc, "ArDoc"))
})

test_that("ArDoc has a positive client_id", {
  doc <- ArDoc$new()
  expect_true(doc$client_id() > 0)
})

test_that("two ArDocs have different client_ids", {
  expect_false(ArDoc$new()$client_id() == ArDoc$new()$client_id())
})
