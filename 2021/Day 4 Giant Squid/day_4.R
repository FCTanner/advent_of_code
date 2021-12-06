rm(list = ls())
.libPaths("C:/R-packages2/")

library(tidyverse)

# Read first line line for called numbers
# Create list of matrices for the boards


# Parse called numbers ----------------------------------------------------

called_numbers <- read.csv("input.txt", header = FALSE, nrows = 1)
called_numbers <- as.numeric(matrix(called_numbers[1,]))


# Parse boards ------------------------------------------------------------

board_list <- list()


full_data <- read_fwf("input.txt",  fwf_widths(rep(3, 5)), skip_empty_rows = TRUE)
boards <- full_data[1:nrow(full_data),] |> 
  filter(!is.na(X1))

for(i in 1:nrow(boards)){
  if(i %% 5 == 0){
    matrix_start = i - 4
    board_matrix = boards[matrix_start:i,] 
    matrix_index= i/5
    board_list[[matrix_index]] = board_matrix
    
  }
}


# Iterate over boards and "eliminate" numbers -----------------------------
length(board_list)

for(i in 1:length(board_list)){
  # print(board_list[[i]])
  
}

