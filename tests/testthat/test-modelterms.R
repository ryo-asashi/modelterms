# tests/testthat/test-terms.R

test_that("as_terms handles strict term vectors vs length-1 shorthands", {
  # 1. Strict term label vector (length >= 2): preserves order and "b:a" notation
  t_strict <- as_terms(c("b:a", "a", "c:b"))
  expect_s3_class(t_strict, "terms")
  expect_equal(get_labels(t_strict), c("b:a", "a", "c:b"))
  expect_equal(get_order(t_strict), c("b:a" = 2L, "a" = 1L, "c:b" = 2L))
  expect_equal(get_order(t_strict, use.names = FALSE), c(2L, 1L, 2L))
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
  t_dot1 <- as_terms(".", response = "y", data = df)
  expect_true(has_response(t_dot1))
  expect_equal(get_labels(t_dot1), c("x1", "x2", "x3"))

  t_dot2 <- as_terms(".^2", response = "y", data = df)
  expect_equal(
    get_labels(t_dot2),
    c("x1", "x2", "x3", "x1:x2", "x1:x3", "x2:x3")
  )

  # 6. Invalid inputs throw informative errors
  expect_error(as_terms(c("a * b", "c")), "single valid term label")
  expect_error(as_terms(c("a + b", "c")), "single valid term label")
  expect_error(as_terms(c(".", "x1"), data = df), "single valid term label")
  expect_error(as_terms(c("x1", NA_character_)), "NA values")
  expect_error(as_terms(c("x1", "")), "empty strings")
  expect_error(as_terms(123), "cannot coerce")
})


test_that("as_terms handles intercept, response, and empty character(0)", {
  # Default intercept = NULL (FALSE for character), overridden by "1", "+1", or intercept = TRUE/FALSE
  expect_false(has_intercept(as_terms(c("x1", "x2"))))
  expect_true(has_intercept(as_terms(c("x1", "x2"), intercept = TRUE)))
  expect_true(has_intercept(as_terms(c("x1", "1"))))
  expect_true(has_intercept(as_terms(c("1", "x1"))))
  expect_false(has_intercept(as_terms(c("x1", "-1"))))
  expect_true(has_intercept(as_terms(c("x1", "-1"), intercept = TRUE)))
  expect_false(has_intercept(as_terms(c("x1", "1"), intercept = FALSE)))
  # Empty character(0) produces valid intercept-only or empty terms object
  t_empty0 <- as_terms(character(0), intercept = FALSE)
  expect_s3_class(t_empty0, "terms")
  expect_equal(get_labels(t_empty0), character(0))
  expect_false(has_intercept(t_empty0))
  expect_false(has_response(t_empty0))
  expect_null(get_response(t_empty0))

  t_empty1 <- as_terms(character(0), response = "log(Ozone)", intercept = TRUE)
  expect_s3_class(t_empty1, "terms")
  expect_true(has_intercept(t_empty1))
  expect_true(has_response(t_empty1))
  expect_equal(get_response(t_empty1, raw = FALSE), "log(Ozone)")
  expect_equal(get_response(t_empty1, raw = TRUE), "Ozone")

  # Modifying response and intercept on existing formulas / terms objects via as_terms()
  fml <- y ~ x1 + x2
  t_no_resp <- as_terms(fml, response = FALSE)
  expect_false(has_response(t_no_resp))
  expect_true(has_intercept(t_no_resp))

  t_mod <- as_terms(fml, response = "new_y", intercept = FALSE)
  expect_equal(get_response(t_mod), "new_y")
  expect_false(has_intercept(t_mod))
  expect_equal(get_labels(t_mod), c("x1", "x2"))
})


test_that("get_variables handles three levels of variables and by.term", {
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

  # by.term = TRUE returns a named list even for main-effect-only or empty models
  vlist_main <- get_variables(y ~ x1 + x2, raw = TRUE, by.term = TRUE)
  expect_type(vlist_main, "list")
  expect_equal(vlist_main, list(x1 = "x1", x2 = "x2"))

  vlist_complex <- get_variables(fml, raw = FALSE, by.term = TRUE)
  expect_equal(vlist_complex[["I(1:100):x3"]], c("I(1:100)", "x3"))

  expect_equal(get_variables(y ~ 1, by.term = FALSE), character(0))
  expect_equal(
    get_variables(y ~ 1, by.term = TRUE),
    stats::setNames(list(), character(0))
  )
})


test_that("filter_terms subsets and modifies response/intercept safely", {
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

  # 3. Direct subsetting via positional keep, drop, and integer indices
  t_pos <- filter_terms(fml, "Temp:Wind")
  expect_equal(get_labels(t_pos), "Wind:Temp")

  t_drop_int <- filter_terms(fml, drop = "Temp:Wind", response = FALSE)
  expect_equal(get_labels(t_drop_int), c("Wind", "Temp", "Solar.R"))
  expect_false(has_response(t_drop_int))

  t_keep_idx <- filter_terms(fml, keep = c(1, 4))
  expect_equal(get_labels(t_keep_idx), c("Wind", "Wind:Temp"))

  # 4. Replacing response and toggling intercept in filter_terms()
  t_replaced <- filter_terms(fml, order = 1, response = "yhat", intercept = FALSE)
  expect_equal(get_response(t_replaced), "yhat")
  expect_false(has_intercept(t_replaced))
  expect_equal(get_labels(t_replaced), c("Wind", "Temp", "Solar.R"))

  # 5. Filter resulting in 0 terms retains response and intercept without crashing
  t_null <- filter_terms(fml, order = 3)
  expect_s3_class(t_null, "terms")
  expect_equal(get_labels(t_null), character(0))
  expect_true(has_response(t_null))
  expect_true(has_intercept(t_null))
})


test_that("internal helpers and match_labels handle complex terms and nomatch", {
  fml <- y ~ x1 + log(x2) + x1:log(x2) + I(1:100):x1

  # Internal strict label check and canonical formatting
  expect_true(is_strict_label("x2:x1"))
  expect_true(is_strict_label("I(1:100):x1"))
  expect_false(is_strict_label("x1 * x2"))
  expect_false(is_strict_label("x1 + x2"))
  expect_false(is_strict_label("."))
  expect_false(is_strict_label("1"))

  sigs <- format_labels(c("log(x2):x1", "x1:I(1:100)", "x1 * x2", "x1 + x2", "."))
  expect_equal(sigs, c("log(x2):x1", "I(1:100):x1", NA, NA, NA))
  expect_equal(
    format_labels(fml),
    c("x1", "log(x2)", "log(x2):x1", "I(1:100):x1")
  )

  # Order-invariant matching (named by input labels by default)
  query <- c("I(1:100):x1", "log(x2):x1", "x1")
  matched <- match_labels(query, fml)
  expect_equal(
    matched,
    c("I(1:100):x1" = "x1:I(1:100)", "log(x2):x1" = "x1:log(x2)", "x1" = "x1")
  )
  expect_equal(
    match_labels(query, fml, use.names = FALSE),
    c("x1:I(1:100)", "x1:log(x2)", "x1")
  )

  # nomatch behaviors ("omit" is default, "pass" keeps NAs, "fail" errors)
  mixed_query <- c("x1", "x1 * x2", "unknown")
  expect_equal(
    match_labels(mixed_query, fml, nomatch = "omit"),
    c(x1 = "x1")
  )
  expect_equal(
    match_labels(mixed_query, fml, nomatch = "pass"),
    c(x1 = "x1", "x1 * x2" = NA_character_, unknown = NA_character_)
  )
  expect_error(
    match_labels(c("x1", "x1 * x2"), fml, nomatch = "fail"),
    "1 invalid label found: x1 \\* x2"
  )
})

