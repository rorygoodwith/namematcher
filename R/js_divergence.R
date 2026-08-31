get_ngram_lists <- function(strings, n) {
  strings <- tolower(gsub(" ", "", strings))
  lengths <- nchar(strings)

  get_ngrams <- function(string, length, n) {
    if (length < n) {
      return(string)
    }
    substring(tolower(string), 1:(length - n + 1), n:length)
  }

  mapply(get_ngrams, strings, lengths, MoreArgs = list(n = n), SIMPLIFY = FALSE)
}

#' Calculate Jensen-Shannon Divergence based on character n-grams
#' @param names_1 Character vector
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
  n_pairs <- length(ngrams_list_1)

  lens_1 <- lengths(ngrams_list_1)
  lens_2 <- lengths(ngrams_list_2)
  pair_1 <- rep(seq_len(n_pairs), lens_1)
  pair_2 <- rep(seq_len(n_pairs), lens_2)
  tokens_1 <- unlist(ngrams_list_1, use.names = FALSE)
  tokens_2 <- unlist(ngrams_list_2, use.names = FALSE)

  # Assign each (pair, n-gram) combination a global index in one pass across
  # both members of every pair, so p and q share the same vocabulary per pair.
  code_all <- as.integer(interaction(
    c(pair_1, pair_2),
    c(tokens_1, tokens_2),
    drop = TRUE
  ))
  n_tokens_1 <- length(tokens_1)
  code_1 <- code_all[seq_len(n_tokens_1)]
  code_2 <- code_all[seq_len(length(code_all) - n_tokens_1) + n_tokens_1]
  n_codes <- max(code_all)

  cnt_1 <- tabulate(code_1, nbins = n_codes)
  cnt_2 <- tabulate(code_2, nbins = n_codes)

  # Map each code back to the pair it belongs to; interaction orders levels by
  # pair, so codes for each pair are contiguous within the per-pair vocabulary.
  pair_of_code <- c(pair_1, pair_2)[match(seq_len(n_codes), code_all)]

  p_code <- cnt_1 / lens_1[pair_of_code]
  q_code <- cnt_2 / lens_2[pair_of_code]
  m_code <- 0.5 * (p_code + q_code)

  kl_1 <- p_code * log2(p_code / m_code)
  kl_1[p_code == 0] <- 0
  kl_2 <- q_code * log2(q_code / m_code)
  kl_2[q_code == 0] <- 0

  js_divergences <- as.numeric(
    0.5 * rowsum(kl_1, pair_of_code) + 0.5 * rowsum(kl_2, pair_of_code)
  )
  result[to_compute] <- js_divergences
  return(result)
}
