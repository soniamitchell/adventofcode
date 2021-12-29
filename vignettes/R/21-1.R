# Read in data ------------------------------------------------------------
dat <- here("inst", "2021", "day21.txt") |>
  readLines()

start <- dat |> vapply(function(x) gsub(".*([0-9])$", "\\1", x),
                       character(1)) |>
  as.numeric()

# Play a practice game using the deterministic 100-sided die --------------

pos1 <- start[1]
pos2 <- start[2]
score1 <- 0
score2 <- 0
i <- 0

while (all(c(score1, score2) < 1000)) {
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

# The moment either player wins, what do you get if you multiply the score of
# the losing player by the number of times the die was rolled during the game?

min(score1, score2) * i * 3
