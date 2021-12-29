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
