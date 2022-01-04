#' @rdname day6
#' @export
#'
simulate_lanternfish2 <- function(dat) {
  # Initialise variables
  days <- 256

  # Generate a frequency table
  fish_counts <- data.frame(age = dat) |>
    dplyr::count(age) |>                     # Count fish
    tidyr::complete(age = 0:8, fill = list(n = 0))  # Fill in the missing categories

  for (i in seq_len(days)) {
    # Number of zeroes
    n_zeroes <- fish_counts$n[fish_counts$age == 0]
    # Subtract 1 from all fish
    fish_counts$n[1:(nrow(fish_counts) - 1)] <- fish_counts$n[2:nrow(fish_counts)]
    # Reset zeroes to six
    age_6 <- which(fish_counts$age == 6)
    fish_counts$n[age_6] <- fish_counts$n[age_6] + n_zeroes
    # Add eights for each zero
    fish_counts$n[which(fish_counts$age == 8)] <- n_zeroes
  }
  sum(fish_counts$n) |>
    format(scientific = FALSE)
}
