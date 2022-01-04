#' Day 4: Giant Squid
#' @source <https://adventofcode.com/2021/day/4>
#' @name day4
#'
NULL

#' @rdname day4
#' @param path file path
#' @export
#'
get_numbers <- function(path) {
  path |>
    scan(what = "character", n = 1) |>
    strsplit(",") |>
    unlist() |>
    as.numeric()
}

#' @rdname day4
#' @export
#'
get_boards <- function(path) {
  raw_boards <- read.table(path, skip = 1)
  number_of_boards <- nrow(raw_boards) / 5

  lapply(seq_len(number_of_boards), function(x) {
    start <- 1 + (5 * (x - 1))
    end <- start + 4
    raw_boards[start:end, ]
  })
}

#' @rdname day4
#' @param numbers numbers
#' @param boards boards
#' @export
#'
play_bingo <- function(numbers, boards) {
  # For each number in the bingo call
  for (this_number in numbers) {
    # Check each board
    for (i in seq_along(boards)) {
      # If the number is on the board, mark it off (as NA)
      boards[[i]] <- update_board(this_number, boards[[i]])
      # Check for win
      has_won <- check_for_win(boards[[i]])
      # If this board has won, calculate the winning score
      if (has_won)
        return(sum(boards[[i]], na.rm = TRUE) * this_number)
    }
  }
}

check_for_win <- function(board) {
  check_board <- is.na(board)
  row_win <- any(rowSums(check_board) == 5)
  column_win <- any(colSums(check_board) == 5)
  dplyr::if_else(row_win | column_win, TRUE, FALSE)
}

update_board <- function(number, board) {
  check_number <- board == number
  if (any(check_number, na.rm = TRUE)) {
    # Mark the number
    find_number <- which(check_number, arr.ind = TRUE)
    board[find_number] <- NA
  }
  board
}
