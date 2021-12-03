rm(list = ls())

library(dplyr)

instructions <- read.delim("input.txt", header= FALSE, col.names  = "instruction")



# Part one ----------------------------------------------------------------


for (i in 1:nrow(instructions)){
  instructions$direction[i] <- strsplit(instructions$instruction, split = " ")[[i]][1]
  instructions$units[i] <- as.numeric(strsplit(instructions$instruction, split = " ")[[i]][2])
}

instructions |> 
  group_by(direction) |> 
  summarise(total = sum(units)) 

part_one_result <- (2039-1006) * 2053


# Part two ----------------------------------------------------------------

aim <- 0
depth_pos <- 0
forward_pos <- 0

for(i in 1:nrow(instructions)){
  if(instructions$direction[i] == "down"){
    aim <- aim + instructions$units[i]
  }else if(instructions$direction[i] == "up"){
    aim <- aim - instructions$units[i]
  }
  else if(instructions$direction[i] == "forward"){
    forward_pos <- forward_pos + instructions$units[i]
    depth_pos <- depth_pos + (aim * instructions$units[i])
  }
}

part_two_result <- depth_pos * forward_pos
