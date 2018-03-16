
median <- purrr::partial(...f = quantile,
                         probs = quant,
                         na.rm = TRUE,
                         names = FALSE)
