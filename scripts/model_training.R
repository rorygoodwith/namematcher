library(namematcher)
library(dplyr)
library(purrr)
library(car)
library(stringdist)
library(ggplot2)

set.seed(123)
source("R/js_divergence.R")

FEATURE_NAMES <- c("jaro_winkler", "jensen_shannon")


read_and_clean_data <- function(filepath = "data/DBLP10k.csv") {
  df_names <- read.csv(filepath, sep = ";")

  df_non_exact_matches <- df_names |>
    filter(author1 != author2) |>
    select(sameentity, author1, author2)

  df_features <- df_non_exact_matches |>
    mutate(
      jaro_winkler = stringdist::stringdist(author1, author2, method = "jw"),
      levenshtein = stringdist::stringdist(author1, author2, method = "lv")
    )
  df_features$jensen_shannon <- map2_dbl(
    df_features$author1,
    df_features$author2,
    js_divergence
  )

  df_features <- mutate(
    df_features,
    across(
      .cols = FEATURE_NAMES,
      .fns = ~ scale(.)[, 1]
    )
  )

  return(df_features)
}


train_test_split <- function(df) {
  if (!is.data.frame(df)) {
    stop("df must be of type data.frame")
  }

  df$index <- 1:nrow(df)
  train <- slice_sample(df, prop = 0.8)
  test <- filter(df, !(index %in% train$index))

  return(list("train" = train, "test" = test))
}


train_model <- function(train) {
  if (!is.data.frame(train)) {
    stop("df must be of type data.frame")
  }
  model <- glm(
    formula = "sameentity ~ jaro_winkler + jensen_shannon",
    family = binomial(link = "logit"),
    data = train
  )

  if (any(vif(model) > 5)) {
    warning(
      paste0(
        "High multicollinearity between some model features:\n",
        vif(model)
      ),
      call. = FALSE
    )
  }

  print(summary(model))
  return(model)
}


test_model <- function(model, test) {
  test$predictions <- predict(model, newdata = test, type = "response")

  ggplot(test, aes(predictions, fill = sameentity)) +
    geom_histogram()

  test$predictions_bin <- ifelse(test$predictions > 0.5, TRUE, FALSE)

  results <- list()

  results[["confusion_matrix"]] <- test |>
    summarise(n(), .by = c(sameentity, predictions_bin))

  results[["accuracy"]] <- sum(test$sameentity == test$predictions_bin) /
    nrow(test)
  results[["recall"]] <- sum(test$sameentity & test$predictions_bin) /
    sum(test$sameentity)
  results[["precision"]] <- sum(!test$sameentity & !test$predictions_bin) /
    sum(!test$sameentity)

  get_f1 <- function(precision, recall) {
    2 * ((precision * recall) / (precision + recall))
  }

  results["f1"] <- get_f1(results[["precision"]], results[["recall"]])

  print(results)
}


main <- function() {
  train_test_list <- read_and_clean_data() |>
    train_test_split()

  trained_model <- train_model(train_test_list[["train"]])

  test_model(trained_model, train_test_list[["test"]])
}

main()
