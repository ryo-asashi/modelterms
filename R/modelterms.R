#' Coerce, Inspect, Filter, and Match Model Terms and Labels
#'
#' @description
#' A standalone, zero-dependency suite of functions to seamlessly manipulate
#' \code{\link[stats]{terms}} objects, formulas, fitted models, and character
#' vectors of term labels.
#'
#' @details
#' \subsection{Three Levels of Model Components}{
#' For a model formula such as \code{y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)},
#' this package distinguishes three structural levels:
#' \enumerate{
#'   \item \strong{Term Labels} (\code{get_labels()}):
#'     The additive components of the model: \code{c("x1", "log(x2)", "x1:log(x2)", "I(x1 * x2)")}.
#'   \item \strong{Model Variables} (\code{get_variables(x, raw = FALSE)}):
#'     The distinct expressions forming the terms (rows of the \code{"factors"} matrix):
#'     \code{c("x1", "log(x2)", "I(x1 * x2)")}.
#'   \item \strong{Raw Variables} (\code{get_variables(x, raw = TRUE)}):
#'     The underlying column names required in a data frame: \code{c("x1", "x2")}.
#' }
#' }
#'
#' \subsection{Character Input Rules in \code{as_terms()}}{
#' When \code{x} is a character vector, \code{as_terms()} distinguishes between
#' a vector of strict term labels and a length-1 formula shorthand:
#' \itemize{
#'   \item \strong{Strict Term Labels (\code{character(n)})}: If every element (excluding
#'     intercept tokens like \code{"1"} or \code{"-1"}) represents strictly a single valid
#'     term (e.g., \code{c("b:a", "a", "I(x^2)")}), input order and exact interaction notation
#'     (\code{"b:a"}) are preserved by default (\code{keep.order = TRUE}).
#'   \item \strong{Length-1 Formula Shorthand (\code{character(1)})}: If \code{x} is a single
#'     string containing formula operators or \code{.} (e.g., \code{".^2"}, \code{"."},
#'     or \code{"a * b + c + 1"}), it is expanded as a right-hand side formula expression
#'     (using \code{data} if needed) and sorted by interaction order by default
#'     (\code{keep.order = FALSE}).
#'   \item \strong{Invalid Mixed Vectors (\code{length(x) >= 2})}: If \code{x} has length 2 or
#'     more and contains any non-single-term expression (such as \code{c("a * b", "c")} or
#'     \code{c(".", "a")}), coercion fails and returns \code{NULL}.
#' }
#' }
#'
#' @param x,y an object representing model terms: a \code{\link[stats]{terms}} object,
#'   a \code{\link[stats]{formula}}, a fitted model object, or a character vector.
#'   In \code{as_terms()}, \code{y} specifies an optional response variable (as a character
#'   string or language object). In \code{match_labels()}, \code{y} is the target object
#'   whose term labels are matched against.
#' @param intercept logical. In \code{as_terms()}, should an intercept be included by default
#'   when coercing a character vector? Defaults to \code{FALSE}. Explicit tokens in \code{x}
#'   (such as \code{"1"} or \code{"+1"}) override this default. In \code{filter_terms()},
#'   whether to retain the intercept of \code{x}.
#' @param data a \code{data.frame} used to resolve \code{.} in formulas or shorthand strings.
#' @param keep.order logical. Should terms be kept in their input order rather than sorted by
#'   interaction order? Defaults to \code{TRUE} for strict term label vectors and \code{FALSE}
#'   for formulas or length-1 formula shorthands.
#' @param env an environment to attach to the resulting \code{terms} object when coercing
#'   from a character vector. Defaults to \code{parent.frame()}.
#' @param raw logical. If \code{FALSE} (default in \code{get_variables()}), returns predictor
#'   expressions (e.g., \code{"log(x2)"}). If \code{TRUE} (default in \code{filter_terms()}),
#'   extracts or filters by underlying raw column names (e.g., \code{"x2"}).
#' @param each.term logical. If \code{FALSE} (default), \code{get_variables()} returns a flat
#'   character vector across the whole model. If \code{TRUE}, returns a named list mapping
#'   each term label to its constituent variables.
#' @param order an integer vector specifying interaction orders to retain in \code{filter_terms()}
#'   (e.g., \code{1L} for main effects, \code{2L} for two-way interactions).
#' @param require a character vector of variable names. Only terms containing at least one
#'   of these variables are retained.
#' @param remove a character vector of variable names. Any terms containing these variables
#'   are excluded.
#' @param response logical. Whether to retain the response variable in \code{filter_terms()}.
#'   Defaults to \code{TRUE}.
#' @param strict logical. If \code{TRUE} (default), \code{make_signature()} and \code{match_labels()}
#'   require each element to be a single valid term.
#' @param nomatch character string specifying how to handle unmatched labels in \code{match_labels()}:
#'   \code{"pass"} (returns \code{NA}), \code{"omit"} (drops unmatched elements), or
#'   \code{"fail"} (throws an error).
#' @param ... additional arguments passed to \code{\link[stats]{terms}}.
#'
#' @examples
#' # Coercion rules for character vectors
#' as_terms(c("b:a", "a", "1"))                 # Strict labels: order & "b:a" preserved
#' as_terms("a * b + c + 1")                    # Length-1 shorthand: expanded & sorted
#' as_terms(".^2", data = iris[, 1:3])          # Length-1 dot shorthand with data
#'
#' # Extracting labels and variables at three levels
#' fml <- y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)
#' get_labels(fml)
#' get_variables(fml, raw = FALSE)
#' get_variables(fml, raw = TRUE)
#' get_variables(fml, raw = TRUE, each.term = TRUE)
#'
#' # Filtering terms and piping to get_labels()
#' fml |> filter_terms(order = 1) |> get_labels()
#' fml |> filter_terms(require = "x2", remove = "x1", raw = TRUE) |> get_labels()
#'
#' # Signatures and order-invariant label matching
#' make_signature(c("log(x2):x1", "x1 + x2"))
#' match_labels(c("log(x2):x1", "x1"), fml)
#' @returns
#' \itemize{
#'   \item \code{as_terms()}, \code{filter_terms()}: A \code{\link[stats]{terms}} object
#'     (or \code{NULL} if \code{as_terms()} fails).
#'   \item \code{has_intercept()}, \code{has_response()}, \code{is_single_term()},
#'     \code{is_fully_crossed()}: A scalar logical (\code{TRUE} or \code{FALSE}).
#'   \item \code{get_order()}: A named integer vector of term orders.
#'   \item \code{get_labels()}, \code{make_signature()}, \code{match_labels()}:
#'     A character vector.
#'   \item \code{get_variables()}: A character vector (when \code{each.term = FALSE}) or
#'     a named list of character vectors (when \code{each.term = TRUE}).
#' }
#'
#' @name modelterms
#'
NULL


# general helpers

`%||%` <- function(x, y) if (is.null(x)) y else x


add_s <- function(n, word) {
  sprintf("%d %s%s", n, word, if (n == 1L) "" else "s")
}


# special helpers

is_strict_label <- function(str) {
  if (is.na(str) || !nzchar(trimws(str))) return(FALSE)
  expr <- tryCatch(str2lang(str), error = function(e) NULL)
  if (is.null(expr) || "." %in% all.vars(expr)) return(FALSE)
  fml <- tryCatch(
    stats::reformulate(c("-1", str), intercept = TRUE),
    error = function(e) NULL
  )
  if (is.null(fml)) return(FALSE)
  obj <- tryCatch(stats::terms(fml), error = function(e) NULL)
  !is.null(obj) &&
    length(attr(obj, "term.labels")) == 1L &&
    attr(obj, "intercept") == 0L
}


try_terms <- function(x, ...) {
  tryCatch(as_terms(x, ...), error = function(e) NULL)
}


# coerce and inspector functions

#' @rdname modelterms
#' @export
#'
as_terms <- function(
    x, y = NULL, intercept = FALSE, data = NULL, keep.order = NULL,
    env = parent.frame(), ...
) {
  if (inherits(x, "terms")) {
    return(x)
  }
  if (!is.character(x)) {
    ko <- keep.order %||% FALSE
    obj <- tryCatch(
      stats::terms(x, keep.order = ko, data = data, ...),
      error = function(e) NULL
    )
    if (inherits(obj, "terms")) {
      return(obj)
    }
    x <- attr(x, "term.labels") %||% (if (is.list(x)) x$term.labels else NULL)
    if (!is.character(x)) {
      stop("cannot coerce 'x' to a 'terms' object", call. = FALSE)
    }
  }
  if (anyNA(x)) {
    stop("'x' must not contain NA values", call. = FALSE)
  }
  x <- trimws(x)
  if (any(!nzchar(x))) {
    stop("'x' must not contain empty strings", call. = FALSE)
  }
  is_int_tok <- grepl("^[+-]?\\s*[01]$", x)
  term_part <- x[!is_int_tok]
  all_strict <- all(vapply(term_part, is_strict_label, logical(1L)))
  if (!all_strict && length(x) >= 2L) {
    stop(
      "when 'x' has length >= 2, each element must be a single valid term label",
      call. = FALSE
    )
  }
  ko <- keep.order %||% all_strict
  head_term <- if (intercept) "1" else "-1"
  fml <- tryCatch(
    stats::reformulate(
      termlabels = c(head_term, x), response = y, intercept = TRUE, env = env
    ),
    error = function(e) stop("failed to construct a valid formula from 'x'", call. = FALSE)
  )
  obj <- tryCatch(
    stats::terms(fml, keep.order = ko, data = data, ...),
    error = function(e) stop(conditionMessage(e), call. = FALSE)
  )
  if (all_strict && ko && length(term_part) > 0L) {
    sigs <- vapply(term_part, function(lbl) {
      ti <- stats::terms(stats::reformulate(c("-1", lbl), intercept = TRUE))
      paste(sort.int(rownames(attr(ti, "factors"))), collapse = "\r")
    }, character(1L))
    orig_unique <- term_part[!duplicated(sigs)]
    labs <- attr(obj, "term.labels")
    if (length(labs) == length(orig_unique)) {
      attr(obj, "term.labels") <- orig_unique
      if (!is.null(attr(obj, "factors"))) {
        colnames(attr(obj, "factors")) <- orig_unique
      }
    }
  }
  obj
}


#' @rdname modelterms
#' @export
#'
has_intercept <- function(x) {
  isTRUE(attr(as_terms(x), "intercept") > 0L)
}


#' @rdname modelterms
#' @export
#'
has_response <- function(x) {
  isTRUE(attr(as_terms(x), "response") > 0L)
}


#' @rdname modelterms
#' @export
#'
get_order <- function(x) {
  obj <- as_terms(x)
  if (is.null(obj)) return(integer(0L))
  ord <- attr(obj, "order") %||% integer(0L)
  stats::setNames(ord, attr(obj, "term.labels"))
}


#' @rdname modelterms
#' @export
get_labels <- function(x) {
  attr(as_terms(x), "term.labels") %||% character(0L)
}


#' @rdname modelterms
#' @export
#'
get_variables <- function(x, raw = FALSE, each.term = FALSE) {
  facs <- attr(as_terms(x), "factors")
  if (is.null(facs) || length(facs) == 0L) {
    return(if (each.term) stats::setNames(list(), character(0L)) else character(0L))
  }
  vars <- rownames(facs)
  if (raw) {
    raw_map <- lapply(vars, function(v) all.vars(str2lang(v)))
  }
  if (each.term) {
    return(apply(facs > 0L, MARGIN = 2L, FUN = function(col) {
      if (raw) {
        unique(unlist(raw_map[col], use.names = FALSE))
      } else {
        vars[col]
      }
    }, simplify = FALSE))
  }
  active <- rowSums(facs) > 0L
  if (raw) {
    unique(unlist(raw_map[active], use.names = FALSE))
  } else {
    vars[active]
  }
}


#' @rdname modelterms
#' @export
#'
get_response <- function(x, raw = FALSE) {
  obj <- as_terms(x)
  if (is.null(obj) || !has_response(obj)) return(NULL)
  expr <- attr(obj, "variables")[[2L]]
  if (raw) {
    return(all.vars(expr))
  }
  deparse1(expr, backtick = TRUE)
}


#' @rdname modelterms
#' @export
#'
filter_terms <- function(
    x, order = NULL, require = NULL, remove = NULL, raw = TRUE,
    response = TRUE, intercept = TRUE, ...
) {
  obj <- as_terms(x, ...)
  if (is.null(obj)) {
    stop("'x' cannot be coerced to a valid 'terms' object", call. = FALSE)
  }
  labs <- get_labels(obj)
  keep <- rep(TRUE, length(labs))
  if (length(labs) > 0L) {
    if (!is.null(order)) {
      keep <- keep & (attr(obj, "order") %in% order)
    }
    if (!is.null(require) || !is.null(remove)) {
      vlist <- get_variables(obj, raw = raw, each.term = TRUE)
      if (!is.null(require)) {
        keep <- keep & vapply(vlist, function(v) any(v %in% require), logical(1L))
      }
      if (!is.null(remove)) {
        keep <- keep & !vapply(vlist, function(v) any(v %in% remove), logical(1L))
      }
    }
  }
  if (all(keep) && response && intercept) return(obj)
  as_terms(
    x = labs[keep],
    y = if (response) get_response(obj, raw = FALSE) else NULL,
    intercept = intercept && has_intercept(obj),
    keep.order = TRUE,
    env = environment(obj) %||% parent.frame()
  )
}


# predicate functions

#' @rdname modelterms
#' @export
#'
is_single_term <- function(x) {
  if (is.character(x)) {
    return(length(x) == 1L && is_strict_label(x))
  }
  obj <- try_terms(x)
  !is.null(obj) && length(get_labels(obj)) == 1L
}


#' @rdname modelterms
#' @export
#'
is_fully_crossed <- function(x) {
  obj <- try_terms(x)
  if (is.null(obj)) return(FALSE)
  n <- length(get_variables(obj, raw = FALSE, each.term = FALSE))
  if (n <= 1L) return(FALSE)
  length(get_labels(obj)) == (2^n - 1L)
}


# matching functions

#' @rdname modelterms
#' @export
#'
make_signature <- function(x, strict = TRUE) {
  if (!is.character(x) && !strict) {
    vlist <- get_variables(x, raw = FALSE, each.term = TRUE)
    if (length(vlist) == 0L) return(character(0L))
    return(unname(vapply(vlist, function(v) {
      paste(sort.int(v), collapse = ":")
    }, character(1L))))
  }
  if (!is.character(x)) {
    x <- get_labels(x)
  }
  if (length(x) == 0L) return(character(0L))

  unname(vapply(x, function(lab) {
    if (strict && !is_strict_label(lab)) return(NA_character_)
    obj <- as_terms(lab)
    if (is.null(obj)) return(NA_character_)
    vars <- sort.int(get_variables(obj, raw = FALSE))
    if (length(vars) == 0L) return(NA_character_)
    paste(vars, collapse = ":")
  }, character(1L)))
}


#' @rdname modelterms
#' @export
#'
match_labels <- function(
    x, y, strict = TRUE, nomatch = c("pass", "omit", "fail")
) {
  nomatch <- match.arg(nomatch)
  xlabs <- if (is.character(x)) x else get_labels(x)
  ylabs <- if (is.character(y)) y else get_labels(y)
  xsigs <- make_signature(xlabs, strict = strict)
  ysigs <- make_signature(ylabs, strict = strict)
  idx <- match(xsigs, ysigs, nomatch = NA_integer_, incomparables = NA_character_)
  if (nomatch != "pass" && anyNA(idx)) {
    ng <- is.na(idx)
    if (nomatch == "fail") {
      stop(
        add_s(sum(ng), "invalid label"), " found: ",
        paste0(xlabs[ng], collapse = ", "),
        call. = FALSE
      )
    }
    idx <- idx[!ng]
  }
  ylabs[idx]
}
