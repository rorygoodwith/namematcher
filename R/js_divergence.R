get_ngram_lists <- function(strings, n) {
  strings <- tolower(gsub(" ", "", strings))
  lengths <- nchar(strings)

  get_ngrams <- function(string, length, n) {
    if (length < n) {
      return(string)
    }
    substring(string, 1:(length - n + 1), n:length)
  }

  mapply(get_ngrams, strings, lengths, MoreArgs = list(n = n), SIMPLIFY = FALSE)
}

get_js_divergence <- function(p, q, m) {
  p_non_zero <- p > 0
  q_non_zero <- q > 0
  kl_div_pm <- sum(p[p_non_zero] * log2(p[p_non_zero] / m[p_non_zero]))
  kl_div_qm <- sum(q[q_non_zero] * log2(q[q_non_zero] / m[q_non_zero]))
  return(0.5 * (kl_div_pm + kl_div_qm))
}

#' Jensen-Shannon Divergence for name similarity
#'
#' Calculates the Jensen-Shannon Divergence (JSD) across two character vectors, `names_1` and `names_2`.
#' Each element of the input vectors is split into n-grams, and the JSD is computed pairwise across the vectors.
#' Lower values indicate less divergence between a pair of names, and therefore a greater chance that those names belong to the same individual or entity.
#'
#' @param names_1 Character vector TEST
#' @param names_2 Character vector
#' @param n Integer length of character n-grams (default 2)
#' @return Numeric scalar bounded [0, 1]
#' @export
#' @examples
#' js_divergence("Jon Smith", "John Smith")
#' js_divergence("Jon Smith", "Elizabeth Howell")
js_divergence <- function(names_1, names_2, n = 2) {
  if (length(names_1) != length(names_2)) {
    stop(
      "names_1 and names_2 must be vectors of the same length",
      call. = FALSE
    )
  }
  if (!is.character(names_1) | !is.character(names_2)) {
    stop("names_1 and names_2 must be character vectors", call. = FALSE)
  }

  # Handle exact matches and missing values without computation
  result <- rep(1, length(names_1))
  result[(is.na(names_1) | is.na(names_2))] <- NA
  result[(names_1 == names_2)] <- 0
  to_compute <- which(!is.na(result) & result != 0)
  if (length(to_compute) == 0) {
    return(result)
  }

  ngrams_list_1 <- get_ngram_lists(names_1[to_compute], n)
  ngrams_list_2 <- get_ngram_lists(names_2[to_compute], n)

  get_jsd_from_pair <- function(ngrams_1, ngrams_2) {
    intersection <- unique(c(ngrams_1, ngrams_2))
    # Probability vectors P and Q
    p <- table(factor(ngrams_1, levels = intersection)) / length(ngrams_1)
    q <- table(factor(ngrams_2, levels = intersection)) / length(ngrams_2)
    # Create mixture distribution
    m <- 0.5 * (p + q)

    get_js_divergence(p, q, m)
  }

  js_divergences <- mapply(get_jsd_from_pair, ngrams_list_1, ngrams_list_2)
  result[to_compute] <- js_divergences
  return(result)
}
