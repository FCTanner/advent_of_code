rm(list = ls())

submarine_depths <- read.delim("input.txt", header= FALSE)
colnames(submarine_depths) <- "depth"


# Part 1 ------------------------------------------------------------------

count <- 0

part_1_iterator <- nrow(submarine_depths) -1

for(i in 1:part_1_iterator){
  if(submarine_depths$depth[i + 1] > submarine_depths$depth[i]){
    count <- count + 1
    }
}



# Part 2 Sliding window ---------------------------------------------------

count_window <- 0

part_2_iterator <- nrow(submarine_depths) - 3

for(i in 1:part_2_iterator){
  first_window_sum <- submarine_depths$depth[i] + submarine_depths$depth[i + 1] + submarine_depths$depth[i + 2]
  second_window_sum <- submarine_depths$depth[i + 1] + submarine_depths$depth[i + 2] + submarine_depths$depth[i + 3]
  print(first_window_sum)
  print(second_window_sum)
  if(second_window_sum > first_window_sum){
    count_window <- count_window + 1
  }
}
