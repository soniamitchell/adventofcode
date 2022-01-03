# Read in data ------------------------------------------------------------
path <- here("inst", "2021", "day4-test.txt")
raw_numbers <- scan(path, what = "character", n = 1)
raw_boards <- read.table(path, skip = 1)

# Tidy up data
numbers <- as.numeric(strsplit(raw_numbers, ",")[[1]])

number_of_boards <- nrow(raw_boards) / 5
boards <- lapply(seq_len(number_of_boards), function(x) {
  start <- 1 + (5 * (x - 1))
  end <- start + 4
  raw_boards[start:end, ]
})

# Define functions --------------------------------------------------------

update_board <- function(number, board) {
  check_number <- board == number
  if (any(check_number, na.rm = TRUE)) {
    # Mark the number
    find_number <- which(check_number, arr.ind = TRUE)
    board[find_number] <- NA
  }
  board
}

check_for_win <- function(board) {
  check_board <- is.na(board)
  row_win <- any(rowSums(check_board) == 5)
  column_win <- any(colSums(check_board) == 5)
  if_else(row_win | column_win, TRUE, FALSE)
}

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

# Play bingo --------------------------------------------------------------

play_bingo(numbers, boards)

lose_bingo <- function(numbers, boards) {
  # Initialise objects
  results <- data.frame(number = numeric(), board = numeric(), score = numeric())
  remaining_boards <- seq_along(boards)

  # For each number in the bingo call
  for (this_number in numbers) {
    # Check each board
    for (i in remaining_boards) {
      # If the number is on the board, mark it off (as NA)
      boards[[i]] <- update_board(this_number, boards[[i]])
      # Check for win
      has_won <- check_for_win(boards[[i]])
      # If this board has won, calculate the winning score
      if (has_won) {
        score <- sum(boards[[i]], na.rm = TRUE) * this_number
        results <- rbind(results,
                         data.frame(number = this_number,
                                    board = i,
                                    score = score))
        remaining_boards <- remaining_boards[-which(remaining_boards == i)]
      }
    }
  }
  results
}

# Play bingo
losers <- lose_bingo(numbers, boards)
tail(losers)
