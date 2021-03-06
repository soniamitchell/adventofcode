# Define functions --------------------------------------------------------

dirac <- function(start) {
  pos1 <- start[1]
  pos2 <- start[2]
  score1 <- 0
  score2 <- 0
  i <- 0

}

# Play a practice game using the dirac die --------------------------------

pos1 <- 4
pos2 <- 8
score1 <- 0
score2 <- 0
i <- 0

data.frame(rolls = 1:10, universes = 3^(1:10))

totals <- expand.grid(1:3, 1:3, 1:3) |>
  as.matrix() |>
  rowSums() |>
  table()



tmp <- combn(9,3) %>%
  t() %>%
  data.frame() %>%
  dplyr::mutate_if(is.numeric, list(~ dplyr::case_when(.%%3 == 0 ~ .%/%2 * 3,
                                         .%%2 == 0 ~ .%/%2 * 2,
                                         TRUE ~ as.numeric(.)))) %>%
  dplyr::mutate(sumVar = rowSums(.[1:3]))


while (all(c(score1, score2) < 21)) {
  i <- i + 1

  if (i%%2 != 0) {
    pos1 <- pos1 + sum(((i * 3) - 2):(i * 3))
    pos1 <- dplyr::if_else(pos1 > 10, pos1%%10, pos1)
    pos1 <- dplyr::if_else(pos1 == 0, 10, pos1) # since pos1 = 100 returns 0
    score1 <- score1 + pos1

  } else {
    pos2 <- pos2 + sum(((i * 3) - 2):(i * 3))
    pos2 <- dplyr::if_else(pos2 > 10, pos2%%10, pos2)
    pos2 <- dplyr::if_else(pos2 == 0, 10, pos2)
    score2 <- score2 + pos2
  }
}

