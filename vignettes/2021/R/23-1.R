# Read in data ------------------------------------------------------------

# dat <- here("inst", "2021", "day23.txt") |>
#   readLines()

data <- scan(text = "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########", what = "character")




# Define functions --------------------------------------------------------

tidy_dat23 <- function(dat) {
  len <- lapply(dat, nchar) |>
    unique() |>
    unlist()
  pad <- (max(len) - min(len)) / 2
  lapply(dat, \(x) {
    tmp <- strsplit(x, "")[[1]]
    if(length(tmp) == min(len)) {
      tmp <- c(rep("#", pad), tmp, rep("#", pad))
    }
    tmp
  }) |>
    do.call(what = rbind)
}

amphipod_shell <- R6::R6Class("amphipod", list(
  dat = NULL,
  moved = NULL,
  move = NULL,

  initialize = function(dat = dat) {
    self$dat <- dat
    invisible(self)
  },

  print = function(...) {
    print(self$dat)
    invisible(self)
  },

  shift = function(df) {
    dat <- self$dat
    df <- df |> dplyr::select(-type)
    move_this <- dplyr::filter(df, row == min(row)) |> unlist()
    partner <- dplyr::filter(df, row == max(row)) |> unlist()

    if (move_this[1] == 2) {
      go_here <- setNames(c(3, partner[2]), c("row", "col"))

    } else if (move_this[2] < partner[2]) {
      go_here <- setNames(c(2, move_this[2] - 1), c("row", "col"))

    } else {
      go_here <- setNames(c(2, partner[2] + 1), c("row", "col"))
    }

    ind <- rbind(move_this)
    tmp <- dat[ind]
    dat[ind] <- "."
    dat[rbind(go_here)] <- tmp

    ind[1] <-  ind[1] + 1
    self$dat <- dat
    self$moved <- tmp
    self$move <- dat[rbind(ind)]
    invisible(self)
  }
))

find_positions <- function(dat) {
  As <- cbind.data.frame(type = "A", which("A" == dat, arr.ind = TRUE))
  Bs <- cbind.data.frame(type = "B", which("B" == dat, arr.ind = TRUE))
  Cs <- cbind.data.frame(type = "C", which("C" == dat, arr.ind = TRUE))
  Ds <- cbind.data.frame(type = "D", which("D" == dat, arr.ind = TRUE))
  dplyr::bind_rows(As, Bs, Cs, Ds) |>
    dplyr::mutate(row = as.numeric(row),
                  col = as.numeric(col))
}








# Run simulation ----------------------------------------------------------

dat <- tidy_dat23(data)

next_move <- "B"
shell <- amphipod_shell$new(dat)
shell


pos <- find_positions(dat) |>
  dplyr::filter(type == next_move)

shell <- shell$shift(pos)
shell
next_move <- shell$move










# What is the least energy required to organize the amphipods?
