# tests/testthat/test-terms.R

test_that("as_terms handles strict term vectors vs length-1 shorthands", {
  # 1. Strict term label vector (length >= 2): preserves order and "b:a" notation
  t_strict <- as_terms(c("b:a", "a", "c:b"))
  expect_s3_class(t_strict, "terms")
  expect_equal(get_labels(t_strict), c("b:a", "a", "c:b"))
  expect_equal(unname(get_order(t_strict)), c(2L, 1L, 2L))
  expect_false(has_intercept(t_strict))

  # 2. Deduplication of reversed interaction terms in strict vector
  t_dup <- as_terms(c("a", "b:a", "a:b"))
  expect_equal(get_labels(t_dup), c("a", "b:a"))

  # 3. Length-1 strict term: preserves "b:a" notation
  t_single <- as_terms("b:a")
  expect_equal(get_labels(t_single), "b:a")

  # 4. Length-1 formula shorthand ("a * b + c + 1"): expands and sorts by order
  t_short <- as_terms("a * b + c + 1")
  expect_s3_class(t_short, "terms")
  expect_equal(get_labels(t_short), c("a", "b", "c", "a:b"))
  expect_true(has_intercept(t_short))

  # 5. Length-1 dot shorthand ("." and ".^2") with data
  df <- data.frame(y = 1:5, x1 = 1:5, x2 = 1:5, x3 = 1:5)
  t_dot1 <- as_terms(".", y = "y", data = df)
  expect_true(has_response(t_dot1))
  expect_equal(get_labels(t_dot1), c("x1", "x2", "x3"))

  t_dot2 <- as_terms(".^2", y = "y", data = df)
  expect_equal(
    get_labels(t_dot2),
    c("x1", "x2", "x3", "x1:x2", "x1:x3", "x2:x3")
  )

  # 6. Invalid inputs throw errors in as_terms(), but return FALSE in is_terms()
  expect_error(as_terms(c("a * b", "c")), "single valid term label")
  expect_error(as_terms(c("a + b", "c")), "single valid term label")
  expect_error(as_terms(c(".", "x1"), data = df), "single valid term label")
  expect_error(as_terms(c("x1", NA_character_)), "NA values")
  expect_error(as_terms(c("x1", "")), "empty strings")
  expect_error(as_terms(123), "cannot coerce")
})


test_that("as_terms handles intercept tokens, responses, and empty character(0)", {
  # Default intercept = FALSE, overridden by "1" or "+1"
  expect_false(has_intercept(as_terms(c("x1", "x2"))))
  expect_true(has_intercept(as_terms(c("x1", "1"))))
  expect_true(has_intercept(as_terms(c("1", "x1"))))
  expect_false(has_intercept(as_terms(c("x1", "-1"), intercept = TRUE)))

  # Empty character(0) produces valid intercept-only or empty terms object
  t_empty0 <- as_terms(character(0), intercept = FALSE)
  expect_s3_class(t_empty0, "terms")
  expect_equal(get_labels(t_empty0), character(0))
  expect_false(has_intercept(t_empty0))
  expect_null(get_response(t_empty0))

  t_empty1 <- as_terms(character(0), y = "log(Ozone)", intercept = TRUE)
  expect_s3_class(t_empty1, "terms")
  expect_true(has_intercept(t_empty1))
  expect_true(has_response(t_empty1))
  expect_equal(get_response(t_empty1, raw = FALSE), "log(Ozone)")
  expect_equal(get_response(t_empty1, raw = TRUE), "Ozone")
})


test_that("get_variables handles three levels of variables and each.term", {
  fml <- cbind(y1, y2) ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2) + I(1:100):x3

  # Response extraction
  expect_equal(get_response(fml, raw = FALSE), "cbind(y1, y2)")
  expect_equal(get_response(fml, raw = TRUE), c("y1", "y2"))

  # Level 1: Term Labels
  expect_equal(
    get_labels(fml),
    c("x1", "log(x2)", "I(x1 * x2)", "x1:log(x2)", "I(1:100):x3")
  )

  # Level 2: Model Variables (raw = FALSE)
  expect_equal(
    get_variables(fml, raw = FALSE),
    c("x1", "log(x2)", "I(x1 * x2)", "I(1:100)", "x3")
  )

  # Level 3: Raw Variables (raw = TRUE)
  expect_equal(
    get_variables(fml, raw = TRUE),
    c("x1", "x2", "x3")
  )

  # each.term = TRUE returns a named list even for main-effect-only models
  vlist_main <- get_variables(y ~ x1 + x2, raw = TRUE, each.term = TRUE)
  expect_type(vlist_main, "list")
  expect_equal(vlist_main, list(x1 = "x1", x2 = "x2"))

  vlist_complex <- get_variables(fml, raw = FALSE, each.term = TRUE)
  expect_equal(vlist_complex[["I(1:100):x3"]], c("I(1:100)", "x3"))
})


test_that("filter_terms subsets by order, require/remove, and keep/drop safely", {
  fml <- log(Ozone) ~ Wind + Temp + Wind:Temp + Solar.R

  # 1. Filter by order
  t_main <- filter_terms(fml, order = 1)
  expect_equal(get_labels(t_main), c("Wind", "Temp", "Solar.R"))
  expect_true(has_response(t_main))
  expect_true(has_intercept(t_main))
  expect_equal(get_response(t_main), "log(Ozone)")

  # 2. Filter by require & remove (variable level)
  t_sub <- filter_terms(fml, require = "Wind", remove = "Temp")
  expect_equal(get_labels(t_sub), "Wind")

  # 3. Filter by keep & drop (term level, order-invariant & index support)
  t_drop_int <- filter_terms(fml, drop = "Temp:Wind", response = FALSE)
  expect_equal(get_labels(t_drop_int), c("Wind", "Temp", "Solar.R"))
  expect_false(has_response(t_drop_int))

  t_keep_idx <- filter_terms(fml, keep = c(1, 4))
  expect_equal(get_labels(t_keep_idx), c("Wind", "Wind:Temp"))

  # 4. Filter resulting in 0 terms retains response and intercept without crashing
  t_null <- filter_terms(fml, order = 3)
  expect_s3_class(t_null, "terms")
  expect_equal(get_labels(t_null), character(0))
  expect_true(has_response(t_null))
  expect_true(has_intercept(t_null))
})


test_that("is_single_term and is_fully_crossed safely return FALSE on invalid inputs", {
  expect_true(is_single_term("x1"))
  expect_true(is_single_term("x2:x1"))
  expect_true(is_single_term("I(1:100):x1"))
  expect_false(is_single_term("x1 * x2"))
  expect_false(is_single_term("x1 + x2"))
  expect_false(is_single_term("x1 + 1"))
  expect_false(is_single_term("."))
  expect_false(is_single_term(".^2"))
  expect_false(is_single_term("1"))
  expect_false(is_single_term(c("x1", "x2")))
  expect_false(is_single_term(c("a * b", "c")))
  expect_false(is_single_term(NA_character_))
  expect_false(is_single_term(123))

  expect_true(is_fully_crossed(y ~ a * b * c))
  expect_false(is_fully_crossed(y ~ (a + b + c)^2))
  expect_false(is_fully_crossed(y ~ a))
  expect_false(is_fully_crossed(c("a * b", "c")))
})


test_that("make_signature and match_labels handle complex terms and strictness", {
  fml <- y ~ x1 + log(x2) + x1:log(x2) + I(1:100):x1

  # Signature generation
  sigs <- make_signature(c("log(x2):x1", "x1:I(1:100)", "x1 * x2", "x1 + x2", "."))
  expect_equal(sigs, c("log(x2):x1", "I(1:100):x1", NA, NA, NA))

  # Order-invariant matching
  matched <- match_labels(c("x1:I(1:100)", "log(x2):x1", "x1"), fml)
  expect_equal(matched, c("x1:I(1:100)", "x1:log(x2)", "x1"))

  # nomatch behaviors
  expect_equal(
    match_labels(c("x1", "x1 * x2", "unknown"), fml, nomatch = "pass"),
    c("x1", NA, NA)
  )
  expect_equal(
    match_labels(c("x1", "x1 * x2", "unknown"), fml, nomatch = "omit"),
    "x1"
  )
  expect_error(
    match_labels(c("x1", "x1 * x2"), fml, nomatch = "fail"),
    "1 invalid label found: x1 \\* x2"
  )
})
