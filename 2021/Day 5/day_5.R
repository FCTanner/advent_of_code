rm(list = ls())
.libPaths("C:/R-packages2/")

# Parse called numbers ----------------------------------------------------

called_numbers <- read.csv("input.txt", header = FALSE, nrows = 1)
called_numbers <- as.numeric(matrix(called_numbers[1,]))


# Parse boards ------------------------------------------------------------

boards <- read.table(file= "input.txt", sep = "" , header = F, skip= 2)

board_list <- list()

for(i in 1:nrow(boards)){
  if(i %% 5 == 0){
    matrix_start = i - 4
    board_matrix = boards[matrix_start:i,] 
    matrix_index= i/5
    board_list[[matrix_index]] = board_matrix
    
  }
}

# Part 1 ------------------------------------------------------------------

# Iterate over boards and "eliminate" numbers -----------------------------

replace_winners <- function(board, number){
  board[board==number] <- 0
  board
}

check_if_bingo <- function(board){
  row_sums = rowSums(board)
  col_sums = colSums(board)
  if(sum(row_sums == 0) > 0 |
     sum(col_sums == 0) > 0){
    stop("Bingo!")
  }
}

board_list_played <- board_list

for(w in 1:length(called_numbers)){
  winning_number <- called_numbers[w]
  for(b in 1:length(board_list_played)){
    board_list_played[[b]] <- replace_winners(board = board_list_played[[b]], 
                                       number = winning_number)
    check_if_bingo(board_list_played[[b]])
  }
}


# Sum of all unmarked numbers ---------------------------------------------

sum_unmarked <- sum(rowSums(board_list_played[[b]])) 

# Solve 1 -------------------------------------------------------------------

sum_unmarked * winning_number


# Part 2 ------------------------------------------------------------------

# Add all winning boards to list, choose last one

winning_boards <- list()
winning_board_index <- 1

check_if_won <- function(board){
  row_sums = rowSums(board)
  col_sums = colSums(board)
  if(sum(row_sums == 0) > 0 |
     sum(col_sums == 0) > 0){
    return(TRUE)}
  else{
    return(FALSE)
  }
}


board_list_played <- board_list

for(w in 1:length(called_numbers)){
  winning_number <- called_numbers[w]
  for(b in 1:length(board_list_played)){
    board_list_played[[b]] <- replace_winners(board = board_list_played[[b]], 
                                       number = winning_number)
    if(check_if_won(board_list_played[[b]]) ==FALSE){
      last_board_to_win_index <- as.numeric(b)
    }
  }
}


# Just eliminate on board 85

last_board_to_win <- board_list[[last_board_to_win_index]]

for(w in 1:length(called_numbers)){
  winning_number <- called_numbers[w]
  last_board_to_win <- replace_winners(last_board_to_win, winning_number)
  check_if_bingo(last_board_to_win)
}


# Solve P2 ----------------------------------------------------------------


sum_unmarked_last <- sum(rowSums(last_board_to_win))

sum_unmarked_last * last_board_to_win_index
