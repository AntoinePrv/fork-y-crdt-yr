test_that("Map insert_text and contains_key", {
  doc <- Doc$new()
  map <- doc$get_or_insert_map("data")

  trans <- Transaction$lock(doc, mutable = TRUE)
  on.exit(trans$unlock())
  map$insert_text(trans, "key")

  expect_equal(map$len(trans), 1L)
  expect_true(map$contains_key(trans, "key"))
  expect_false(map$contains_key(trans, "other"))
})

test_that("Map remove decreases len", {
  doc <- Doc$new()
  map <- doc$get_or_insert_map("data")

  trans <- Transaction$lock(doc, mutable = TRUE)
  on.exit(trans$unlock())
  map$insert_text(trans, "a")
  map$insert_text(trans, "b")
  map$remove(trans, "a")

  expect_equal(map$len(trans), 1L)
  expect_false(map$contains_key(trans, "a"))
})

test_that("Map clear removes all entries", {
  doc <- Doc$new()
  map <- doc$get_or_insert_map("data")

  trans <- Transaction$lock(doc, mutable = TRUE)
  on.exit(trans$unlock())
  map$insert_text(trans, "a")
  map$insert_text(trans, "b")
  map$clear(trans)

  expect_equal(map$len(trans), 0L)
})

test_that("Map insert methods return usable nested types", {
  doc <- Doc$new()
  map <- doc$get_or_insert_map("data")
  trans <- Transaction$lock(doc, mutable = TRUE)
  on.exit(trans$unlock())

  map$insert_any(trans, "string", "hello")
  map$insert_any(trans, "number", 1.5)
  map$insert_any(trans, "integer", 42L)
  map$insert_any(trans, "bool", TRUE)
  expect_equal(map$get(trans, "string"), "hello")
  expect_equal(map$get(trans, "number"), 1.5)
  expect_equal(map$get(trans, "integer"), 42L)
  expect_equal(map$get(trans, "bool"), TRUE)

  text <- map$insert_text(trans, "content")
  expect_true(inherits(text, "TextRef"))
  expect_true(inherits(map$get(trans, "content"), "TextRef"))
  text$push(trans, "hello")
  text$push(trans, " world")
  expect_equal(text$get_string(trans), "hello world")

  arr <- map$insert_array(trans, "list")
  expect_true(inherits(arr, "ArrayRef"))
  expect_true(inherits(map$get(trans, "list"), "ArrayRef"))
  arr$insert_any(trans, 0L, TRUE)
  expect_equal(arr$len(trans), 1L)

  nested <- map$insert_map(trans, "nested")
  expect_true(inherits(nested, "MapRef"))
  expect_true(inherits(map$get(trans, "nested"), "MapRef"))
  nested$insert_any(trans, "k", 42L)
  expect_equal(nested$len(trans), 1L)

  expect_equal(map$len(trans), 7L)
  expect_null(map$get(trans, "missing"))
})
