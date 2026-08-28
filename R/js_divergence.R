get_ngrams <- function(string, n) {
  string <- tolower(gsub(" ", "", string))
  len <- nchar(string)
  if (len < n) {
    return(string)
  }
  return(substring(tolower(string), 1:(len - n + 1), n:len))
}

#' Calculate Jensen-Shannon Divergence based on character n-grams
#' @param name_1 Character string
#' @param name_2 Character string
#' @param n Integer length of character n-grams (default 2)
#' @return Numeric scalar bounded [0, 1]
#' @export
#' @examples
#' js_divergence("Jon Smith", "John Smith")
#' js_divergence("Jon Smith", "Elizabeth Howell")
js_divergence <- function(name_1, name_2, n = 2) {
  if (!is.character(name_1) || !is.character(name_2)) {
    stop("name_1 and name_2 should be character vectors", call. = FALSE)
  }
  if (is.na(name_1) || is.na(name_2)) {
    return(NA)
  }
  if (name_1 == name_2) {
    return(0)
  }

  ngrams_1 <- get_ngrams(name_1, n)
  ngrams_2 <- get_ngrams(name_2, n)

  vocab <- unique(c(ngrams_1, ngrams_2))

  # Probability vectors P and Q
  p <- table(factor(ngrams_1, levels = vocab)) / length(ngrams_1)
  q <- table(factor(ngrams_2, levels = vocab)) / length(ngrams_2)

  # Convert table to numeric vectors
  p <- as.numeric(p)
  q <- as.numeric(q)

  # Create mixture distribution
  m <- 0.5 * (p + q)

  get_kl_divergence <- function(x, y) {
    nz <- x > 0
    sum(x[nz] * log2(x[nz] / y[nz]))
  }

  jsd <- 0.5 * get_kl_divergence(p, m) + 0.5 * get_kl_divergence(q, m)
  return(jsd)
}
