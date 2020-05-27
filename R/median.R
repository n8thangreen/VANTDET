
median <- purrr::partial(.f = quantile,
                         probs = 0.5,
                         na.rm = TRUE,
                         names = FALSE)
