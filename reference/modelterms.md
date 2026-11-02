# Coerce, Inspect, Filter, and Match Model Terms and Labels

A standalone, zero-dependency suite of functions to seamlessly
manipulate [`terms`](https://rdrr.io/r/stats/terms.html) objects,
formulas, fitted models, and character vectors of term labels.

## Usage

``` r
as_terms(
  x,
  y = NULL,
  intercept = FALSE,
  data = NULL,
  keep.order = NULL,
  env = parent.frame(),
  ...
)

has_intercept(x, ...)

has_response(x, ...)

get_response(x, raw = FALSE, ...)

get_order(x, ...)

get_labels(x, ...)

get_variables(x, raw = FALSE, each.term = FALSE, ...)

filter_terms(
  x,
  keep = NULL,
  drop = NULL,
  order = NULL,
  require = NULL,
  remove = NULL,
  raw = TRUE,
  response = TRUE,
  intercept = TRUE,
  ...
)

is_single_term(x, ...)

is_fully_crossed(x, ...)

make_signature(x, strict = TRUE, ...)

match_labels(x, y, strict = TRUE, nomatch = c("pass", "omit", "fail"), ...)
```

## Arguments

- x, y:

  an object representing model terms: a
  [`terms`](https://rdrr.io/r/stats/terms.html) object, a
  [`formula`](https://rdrr.io/r/stats/formula.html), a fitted model
  object, or a character vector. In `as_terms()`, `y` specifies an
  optional response variable (as a character string or language object).
  In `match_labels()`, `y` is the target object whose term labels are
  matched against.

- intercept:

  logical. In `as_terms()`, should an intercept be included by default
  when coercing a character vector? Defaults to `FALSE`. Explicit tokens
  in `x` (such as `"1"` or `"+1"`) override this default. In
  `filter_terms()`, whether to retain the intercept of `x`.

- data:

  a `data.frame` used to resolve `.` in formulas or shorthand strings.

- keep.order:

  logical. Should terms be kept in their input order rather than sorted
  by interaction order? Defaults to `TRUE` for strict term label vectors
  and `FALSE` for formulas or length-1 formula shorthands.

- env:

  an environment to attach to the resulting `terms` object when coercing
  from a character vector. Defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

- ...:

  additional arguments passed to
  [`terms`](https://rdrr.io/r/stats/terms.html) or `as_terms()`.

- raw:

  logical. If `FALSE` (default in `get_variables()` and
  `get_response()`), returns model expressions (e.g., `"log(x2)"`). If
  `TRUE` (default in `filter_terms()`), extracts or filters by
  underlying raw column names (e.g., `"x2"`).

- each.term:

  logical. If `FALSE` (default), `get_variables()` returns a flat
  character vector across the whole model. If `TRUE`, returns a named
  list mapping each term label to its constituent variables.

- keep, drop:

  a character vector of term labels (matched order-invariantly via
  `match_labels()`) or an integer vector of term indices to explicitly
  retain (`keep`) or exclude (`drop`) in `filter_terms()`.

- order:

  an integer vector specifying interaction orders to retain in
  `filter_terms()` (e.g., `1L` for main effects, `2L` for two-way
  interactions).

- require:

  a character vector of variable names. Only terms containing at least
  one of these variables are retained.

- remove:

  a character vector of variable names. Any terms containing these
  variables are excluded.

- response:

  logical. Whether to retain the response variable in `filter_terms()`.
  Defaults to `TRUE`.

- strict:

  logical. If `TRUE` (default), `make_signature()` and `match_labels()`
  require each element to be a single valid term.

- nomatch:

  character string specifying how to handle unmatched labels in
  `match_labels()`: `"pass"` (returns `NA`), `"omit"` (drops unmatched
  elements), or `"fail"` (throws an error).

## Value

- `as_terms()`, `filter_terms()`: a
  [`terms`](https://rdrr.io/r/stats/terms.html) object.

- `has_intercept()`, `has_response()`, `is_single_term()`,
  `is_fully_crossed()`: a scalar logical (`TRUE` or `FALSE`).

- `get_order()`: a named integer vector of term orders.

- `get_labels()`, `make_signature()`, `match_labels()`: a character
  vector.

- `get_response()`: a character string (when `raw = FALSE`), a character
  vector of variable names (when `raw = TRUE`), or `NULL` if no response
  exists.

- `get_variables()`: a character vector (when `each.term = FALSE`) or a
  named list of character vectors (when `each.term = TRUE`).

## Details

### Three Levels of Model Components

For a model formula such as
`y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)`, this package distinguishes
three structural levels:

1.  **Term Labels** (`get_labels(x)`): The individual additive
    components of the model:
    `c("x1", "log(x2)", "I(x1 * x2)", "x1:log(x2)")`.

2.  **Model Variables** (`get_variables(x, raw = FALSE)`): The distinct
    predictor expressions forming the terms (rows of the `"factors"`
    matrix): `c("x1", "log(x2)", "I(x1 * x2)")`.

3.  **Raw Variables** (`get_variables(x, raw = TRUE)`): The underlying
    column names required in `data`: `c("x1", "x2")`.

### Character Input Rules in `as_terms()`

When `x` is a character vector, `as_terms()` distinguishes between a
vector of strict term labels and a length-1 formula shorthand:

- **Strict Term Labels (`character(n)`)**: If every element (excluding
  intercept tokens like `"1"` or `"-1"`) represents strictly a single
  valid term (e.g., `c("b:a", "a", "I(x^2)")`), input order and exact
  interaction notation (`"b:a"`) are preserved by default
  (`keep.order = TRUE`).

- **Length-1 Formula Shorthand (`character(1)`)**: If `x` is a single
  string containing formula operators or `.` (e.g., `".^2"`, `"."`, or
  `"a * b + c + 1"`), it is expanded as a right-hand side formula
  expression (using `data` if needed) and sorted by interaction order by
  default (`keep.order = FALSE`).

- **Invalid Mixed Vectors (`length(x) >= 2`)**: If `x` has length 2 or
  more and contains any non-single-term expression (such as
  `c("a * b", "c")` or `c(".", "a")`), an informative error is thrown.

### Safe Alternatives to Base R `stats` Functions

`filter_terms()` serves as a declarative, crash-safe replacement for
[`drop.terms`](https://rdrr.io/r/stats/delete.response.html),
[`delete.response`](https://rdrr.io/r/stats/delete.response.html), and
`[.terms`:

- `filter_terms(x, response = FALSE)` safely removes the response
  variable like `stats::delete.response(x)`.

- `filter_terms(x, drop = ...)` and `filter_terms(x, keep = ...)` subset
  specific terms by character label (order-invariant) or numeric index
  without crashing when all terms are dropped.

## Examples

``` r
# Coercion with as_terms()
t1 <- as_terms(c("b:a", "a", "1"))
get_labels(t1)
#> [1] "b:a" "a"  
has_intercept(t1)
#> [1] TRUE

t2 <- as_terms(".^2", y = "Ozone", data = airquality[, 1:4])
get_labels(t2)
#> [1] "Solar.R"      "Wind"         "Temp"         "Solar.R:Wind" "Solar.R:Temp"
#> [6] "Wind:Temp"   

# Extracting labels, variables, and response at three levels
fml <- log(y) ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)
get_response(fml, raw = FALSE)
#> [1] "log(y)"
get_response(fml, raw = TRUE)
#> [1] "y"
get_labels(fml)
#> [1] "x1"         "log(x2)"    "I(x1 * x2)" "x1:log(x2)"
get_variables(fml, raw = FALSE)
#> [1] "x1"         "log(x2)"    "I(x1 * x2)"
get_variables(fml, raw = TRUE)
#> [1] "x1" "x2"
get_variables(fml, raw = TRUE, each.term = TRUE)
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
#> 

# Filtering terms by order, variables, or specific term labels
fml |> filter_terms(order = 1) |> get_labels()
#> [1] "x1"         "log(x2)"    "I(x1 * x2)"
fml |> filter_terms(require = "x2", remove = "x1", raw = TRUE) |> get_labels()
#> [1] "log(x2)"
fml |> filter_terms(drop = "log(x2):x1", response = FALSE)
#> ~1 + x1 + log(x2) + I(x1 * x2)
#> attr(,"variables")
#> list(x1, log(x2), I(x1 * x2))
#> attr(,"factors")
#>            x1 log(x2) I(x1 * x2)
#> x1          1       0          0
#> log(x2)     0       1          0
#> I(x1 * x2)  0       0          1
#> attr(,"term.labels")
#> [1] "x1"         "log(x2)"    "I(x1 * x2)"
#> attr(,"order")
#> [1] 1 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 0
#> attr(,".Environment")
#> <environment: 0x5f32d1b3d848>

# Signatures and order-invariant label matching
make_signature(c("log(x2):x1", "x1 + x2"))
#> [1] "log(x2):x1" NA          
match_labels(c("x1", "log(x2):x1", "I(x2 * x1)"), fml, nomatch = "pass")
#> [1] "x1"         "x1:log(x2)" NA          
```
