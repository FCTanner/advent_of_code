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
    # print("Bingo!")
    stop("Bingo!")
    return(TRUE)
  }else{
    # print("No winner yet")
    return(FALSE)
  }
}


for(w in 1:length(called_numbers)){
  winning_number <- called_numbers[w]
  # print(winning_number)
  for(b in 1:length(board_list)){
    board_list[[b]] <- replace_winners(board = board_list[[b]], 
                                       number = winning_number)
    last_number <- winning_number
    check_if_bingo(board_list[[b]])
  }
  
}


# Sum of all unmarked numbers ---------------------------------------------

sum_unmarked <- sum(rowSums(board_list[[b]])) 

# Solve 1 -------------------------------------------------------------------

sum_unmarked * last_number



# Part 2 ------------------------------------------------------------------


