
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modelterms

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**modelterms** is a lightweight, zero-dependency R package providing a
unified interface for manipulating `stats::terms` objects, formulas,
fitted models, and character vectors of term labels.

It is designed both as a standalone utility for statistical modeling
workflows and as a core infrastructure module for other packages
(including [**midr**](https://github.com/ryo-asashi/midr)).

## Why **modelterms**?

In R, model terms serve as the structural blueprint for predictive
models—a role of paramount importance in additive models built with
`stats::lm()`, `stats::glm()`, or GAMs. However, the word “terms” has
historically suffered from ambiguity in R. For instance, the `terms`
argument in `stats::predict.lm()` expects a character vector of term
labels, whereas the `"terms"` object stored inside a fitted model is a
formula-like object packed with complex attributes. Despite representing
the same structural concepts, they are not directly interchangeable.

Furthermore, while Base R’s `stats::terms()` and `stats::drop.terms()`
provide powerful formula parsing, working with them programmatically
often involves sharp edges:

1.  **Inconsistent Coercion**: `stats::terms()` cannot directly coerce a
    character vector of term labels like `c("x1", "x2")`, and
    `stats::reformulate(character(0))` throws an error.
2.  **Unwanted Reordering & Renaming**: Even with `keep.order = TRUE`,
    `stats::terms()` silently rewrites interaction labels based on
    variable appearance order (e.g., turning `c("a", "b:a")` into
    `c("a", "a:b")`).
3.  **Fragile Subsetting**: `stats::drop.terms()` requires numeric
    indices, loses the response variable by default, and crashes when
    all predictor terms are dropped.
4.  **Ambiguous “Variables”**: Base R lacks a unified API to distinguish
    between *term labels* (additive components, e.g., `"x1:log(x2)"`),
    *model variables* (transformed expressions in a formula, e.g.,
    `"log(x2)"`), and underlying *raw variables* (column names in a
    `data.frame`, e.g., `"x2"`).

**modelterms** resolves all of these issues with a clean, pipe-friendly
functional API.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("ryo-asashi/modelterms")
```

## Core Concepts: Three Levels of Model Components

Consider the formula `y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)`.
**modelterms** clearly distinguishes three structural levels:

| Level | Concept | Function | Output for Example Formula |
|:---|:---|:---|:---|
| **Term Labels** | Individual additive components (main effects & interactions) | `get_labels(x)` | `c("x1", "log(x2)", "I(x1 * x2)", "x1:log(x2)")` |
| **Model Variables** | Distinct predictor expressions (rows of the `factors` matrix) | `get_variables(x, raw = FALSE)` | `c("x1", "log(x2)", "I(x1 * x2)")` |
| **Raw Variables** | Underlying column names required in a `data.frame` | `get_variables(x, raw = TRUE)` | `c("x1", "x2")` |

Setting `by.term = TRUE` in `get_variables()` returns a named list
mapping each term label to its constituent variables:

``` r
library(modelterms)

fml_demo <- y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)
get_variables(fml_demo, raw = TRUE, by.term = TRUE)
#> $x1
#> [1] "x1"
#> 
#> $`log(x2)`
#> [1] "x2"
#> 
#> $`I(x1 * x2)`
#> [1] "x1" "x2"
#> 
#> $`x1:log(x2)`
#> [1] "x1" "x2"
```

## Usage Examples

### 1. Flexible and Safe Coercion with `as_terms()`

`as_terms()` seamlessly converts `terms` objects, formulas, fitted
models (`lm`, `glm`, etc.), or character vectors into a valid
`stats::terms` object, while allowing on-the-fly modification of the
response variable and intercept.

- **Strict Term Label Vectors (`length >= 2`)**: Every element must be a
  valid single term. Input order and exact interaction notation (such as
  `"b:a"`) are strictly preserved.
- **Formula Shorthand (`length == 1`)**: A single string containing
  formula operators (e.g., `".^2"` or `"a * b + c"`) is expanded as a
  right-hand side formula expression.

``` r
# Preserves exact order and "b:a" notation for term label vectors
t1 <- as_terms(c("b:a", "a", "1"))
get_labels(t1)
#> [1] "b:a" "a"
has_intercept(t1)
#> [1] TRUE

# Expands length-1 formula shorthands (including "." with data)
t2 <- as_terms(".^2", response = "Ozone", data = airquality[, 1:4])
get_labels(t2)
#> [1] "Solar.R"      "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp"
#> [6] "Wind:Temp"
```

### 2. Subsetting, Filtering, and Modifying Terms with `filter_terms()`

`filter_terms()` is a declarative, crash-safe replacement for
`stats::drop.terms()`, `stats::delete.response()`, and `[.terms`. You
can subset terms directly by label or index, filter by interaction order
or constituent variables, and modify the response or intercept—even if
all predictor terms are filtered out.

``` r
fml <- log(Ozone) ~ Solar.R + Wind + Temp + Wind:Temp + Solar.R:Wind

# Extract main effects only
fml |> filter_terms(order = 1) |> get_labels()
#> [1] "Solar.R" "Wind"    "Temp"

# Keep terms involving "Wind", but exclude any term involving "Solar.R"
sub_terms <- filter_terms(fml, require = "Wind", remove = "Solar.R")
get_labels(sub_terms)
#> [1] "Wind"      "Wind:Temp"

# Drop specific interactions (order-invariantly) and replace the response variable
filter_terms(fml, drop = "Temp:Wind", response = "Ozone")
#> Ozone ~ 1 + Solar.R + Wind + Temp + Solar.R:Wind
#> attr(,"variables")
#> list(Ozone, Solar.R, Wind, Temp)
#> attr(,"factors")
#>         Solar.R Wind Temp Solar.R:Wind
#> Ozone         0    0    0            0
#> Solar.R       1    0    0            1
#> Wind          0    1    0            1
#> Temp          0    0    1            0
#> attr(,"term.labels")
#> [1] "Solar.R"      "Wind"         "Temp"         "Solar.R:Wind"
#> attr(,"order")
#> [1] 1 1 1 2
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: R_GlobalEnv>

# Directly use the filtered terms object to fit a submodel
lm_sub <- lm(sub_terms, data = airquality)
coef(lm_sub)
#>  (Intercept)         Wind    Wind:Temp 
#>  4.187217000 -0.410375120  0.004366689
```

### 3. Order-Invariant Term Matching with `match_labels()`

When users specify interaction terms in plotting or evaluation
functions, they may write `"Temp:Wind"` instead of `"Wind:Temp"`.
`match_labels()` normalizes and matches terms regardless of variable
order while safely handling multi-term expressions like `"Wind * Temp"`.

``` r
labs <- c("x1", "log(x2)", "x1:log(x2)", "I(1:100):x1")

# Matches reversed interaction notation to the canonical model labels
match_labels(c("log(x2):x1", "x1:I(1:100)", "x1"), labs)
#>    log(x2):x1   x1:I(1:100)            x1 
#>  "x1:log(x2)" "I(1:100):x1"          "x1"

# Safely handles unmatched or invalid expressions ("omit", "pass", or "fail")
match_labels(c("x1", "x1 * x2", "unknown"), labs, nomatch = "pass")
#>      x1 x1 * x2 unknown 
#>    "x1"      NA      NA
```

## Function Reference Summary

- **Coercion & Filtering** (returns a `terms` object): `as_terms()`,
  `filter_terms()`
- **Label Extraction & Matching** (returns a `character` vector):
  `get_labels()`, `match_labels()`
- **Variable & Attribute Extraction**: `get_variables()`,
  `get_response()`, `get_order()`
- **Structural Predicates** (returns a scalar `logical`):
  `has_intercept()`, `has_response()`
