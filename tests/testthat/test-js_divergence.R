test_that("exact matches return 0", {
  expect_equal(js_divergence("Jon Smith", "Jon Smith"), 0)
  expect_equal(js_divergence("john", "john"), 0)
  expect_equal(js_divergence("A", "A"), 0)
})

test_that("empty strings match exactly", {
  expect_equal(js_divergence("", ""), 0)
})

test_that("identical names differing in case and spaces return 0", {
  expect_equal(js_divergence("Jon Smith", "jon smith"), 0)
  expect_equal(js_divergence("JON SMITH", "jon smith"), 0)
  expect_equal(js_divergence("Jon Smith", "JonSmith"), 0)
  expect_equal(js_divergence("Jon  Smith", "Jon Smith"), 0)
})

test_that("vectorised over pairs", {
  got <- js_divergence(
    c("Jon Smith", "Jon Smith"),
    c("John Smith", "Jon Smith")
  )
  expect_length(got, 2)
  expect_true(got[1] > 0)
  expect_equal(got[2], 0)
  expect_true(all(is.finite(got) & got >= 0 & got <= 1))
})

test_that("exact match in vector mixed with divergence", {
  got <- js_divergence(
    c("Jon Smith", "Jon Smith", "Jon Smith"),
    c("John Smith", "Jon Smith", "Elizabeth Howell")
  )
  expect_equal(got[2], 0)
  expect_gt(got[1], 0)
  expect_gt(got[3], 0)
})

test_that("names_1 and names_2 must be same length", {
  expect_error(
    js_divergence("Jon", c("Jon", "Jon")),
    "names_1 and names_2 must be vectors of the same length"
  )
  expect_error(
    js_divergence(c("Jon", "Jon"), "Jon"),
    "names_1 and names_2 must be vectors of the same length"
  )
})

test_that("names must be character", {
  expect_error(js_divergence(1, "Jon"), "must be character vectors")
  expect_error(js_divergence("Jon", 1), "must be character vectors")
  expect_error(js_divergence(1, 2), "must be character vectors")
})

test_that("missing values propagate as NA", {
  expect_true(is.na(js_divergence(NA_character_, "Jon Smith")))
  expect_true(is.na(js_divergence("Jon Smith", NA_character_)))
  expect_true(is.na(js_divergence(NA_character_, NA_character_)))
})

test_that("NA handling in vectors is per-element", {
  got <- js_divergence(c("Jon", NA, "Jon", NA), c("Jon", "Jon", NA, NA))
  expect_equal(got[1], 0)
  expect_true(is.na(got[2]))
  expect_true(is.na(got[3]))
  expect_true(is.na(got[4]))
})

test_that("length-zero input returns length-zero output", {
  expect_equal(js_divergence(character(0), character(0)), numeric(0))
})

test_that("result is symmetric", {
  a <- js_divergence("Jon Smith", "John Smith")
  b <- js_divergence("John Smith", "Jon Smith")
  expect_equal(a, b)
})

test_that("n argument is respected", {
  expect_equal(
    js_divergence("Jon Smith", "John Smith", n = 3),
    js_divergence("John Smith", "Jon Smith", n = 3)
  )
})

test_that("result is bounded within [0, 1]", {
  set.seed(1)
  n1 <- replicate(
    20,
    paste0(sample(letters, sample(3:10, 1), replace = TRUE), collapse = "")
  )
  n2 <- replicate(
    20,
    paste0(sample(letters, sample(3:10, 1), replace = TRUE), collapse = "")
  )
  got <- js_divergence(n1, n2)
  expect_true(all(got >= 0 & got <= 1))
})

test_that("different strings generally give positive divergence", {
  expect_gt(js_divergence("Elizabeth Howell", "Robert Andrews"), 0)
})

test_that("single character strings give length-derived results", {
  expect_equal(js_divergence("a", "b"), 1)
  expect_equal(js_divergence("a", "a"), 0)
})

test_that("get_ngram_lists lowercases and strips spaces", {
  got <- get_ngram_lists("Jon Smith", 2)
  expect_equal(got[[1]], c("jo", "on", "ns", "sm", "mi", "it", "th"))
})

test_that("get_ngram_lists returns whole string when shorter than n", {
  got <- get_ngram_lists("ab", 3)
  expect_equal(got[[1]], "ab")
  expect_equal(get_ngram_lists("a", 2)[[1]], "a")
})

test_that("get_ngram_lists handles n equal to string length", {
  expect_equal(get_ngram_lists("abc", 3)[[1]], "abc")
})

test_that("get_ngram_lists returns a list with one element per string", {
  got <- get_ngram_lists(c("Jon", "Smith"), 2)
  expect_type(got, "list")
  expect_length(got, 2)
})

test_that("get_js_divergence is zero for identical distributions", {
  p <- c(0.5, 0.5)
  q <- c(0.5, 0.5)
  m <- 0.5 * (p + q)
  expect_equal(get_js_divergence(p, q, m), 0)
})

test_that("get_js_divergence is one for fully disjoint distributions", {
  p <- c(1, 0)
  q <- c(0, 1)
  m <- 0.5 * (p + q)
  expect_equal(get_js_divergence(p, q, m), 1)
})

test_that("get_js_divergence is symmetric in p and q", {
  p <- c(0.9, 0.1)
  q <- c(0.1, 0.9)
  m <- 0.5 * (p + q)
  expect_equal(
    get_js_divergence(p, q, m),
    get_js_divergence(q, p, m)
  )
})

test_that("get_js_divergence is bounded in [0, 1]", {
  p <- c(0.75, 0.25)
  q <- c(0.25, 0.75)
  m <- 0.5 * (p + q)
  got <- get_js_divergence(p, q, m)
  expect_true(got >= 0 && got <= 1)
})

test_that("get_js_divergence returns a numeric scalar", {
  p <- c(0.6, 0.4)
  q <- c(0.4, 0.6)
  m <- 0.5 * (p + q)
  got <- get_js_divergence(p, q, m)
  expect_type(got, "double")
  expect_length(got, 1)
})
