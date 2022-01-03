# What is the largest magnitude of any sum of two different snailfish numbers
# from the homework assignment?

n <- length(dat)
combinations <- expand.grid(1:n, 1:n) |>
  dplyr::filter(Var1 != Var2)
results <- c()

for (i in seq_len(nrow(combinations))) {
  # cat("\r", i, "of", nrow(combinations))
  first <- combinations$Var1[i]
  second <- combinations$Var2[i]
  results[i] <- add_snailfish(dat[first], dat[second]) |>
    check_magnitude()
}

max(results)

