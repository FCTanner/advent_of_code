rm(list = ls())
.libPaths("C:/R-packages2/")

library(tidyverse)

diagnostics <- read_fwf("input.txt", fwf_widths(rep(1, 12)))

# Part one  ---------------------------------------------------------------

gamma_bin <- diagnostics |> 
  summarise(across(.fns = median)) |> 
  unite(., sep = "") |> 
  pull(.) 

epsilon_bin <- diagnostics |> 
  summarise(across(.fns = median)) |> 
  mutate(across(.fns = ~abs(. -1))) |> 
  unite(., sep = "") |> 
  pull(.) 
  

gamma <- strtoi(gamma_bin, base = 2)
epsilon <- strtoi(epsilon_bin, base = 2)

gamma * epsilon


# Part two ----------------------------------------------------------------

# From https://github.com/alex-raw/adventofcode_2021/blob/main/aoc_3.R

parse_string <- function(x) {
  x <- chartr("01", "FT", x) |> strsplit("")
  x <- do.call(rbind, x)
  mode(x) <- "logical"
  x
}

vec2dec <- function(x){
  as.integer(x) |>
  paste(collapse = "") |>
  strtoi(base = 2)
}

solve_1 <- function(x) {
  counts <- colSums(x)
  half <- nrow(x) / 2
  vec2dec(counts >= half) * vec2dec(counts < half)
}

find_hidden <- function(x, scrub = FALSE) {
  for (i in seq_len(ncol(x))) {
    ids <- x[, i]
    is_greater <- sum(ids) >= (nrow(x) / 2)
    flip <- xor(scrub, is_greater) # additional flip for <=
    x <- x[xor(flip, ids), ]       # flip if is greater
    if (!is.matrix(x)) break       # have vector when one is left
  }
  x
}

solve_2 <- function(x) {
  ogr <- find_hidden(x) |> vec2dec()
  co2 <- find_hidden(x, scrub = TRUE) |> vec2dec()
  ogr * co2
}

solve_1(diagnostics)
solve_2(diagnostics)



# Deprecated --------------------------------------------------------------


oxygen_bin  <- diagnostics

for(n in 1:ncol(diagnostics)){
  col_index <- names(diagnostics)[n]
  if(nrow(oxygen_bin) > 2){
    vals <- oxygen_bin[,n] |> pull()
    most_common <- round(median(vals))
    print(col_index)
    oxygen_bin <- oxygen_bin[which(oxygen_bin[[col_index]] == most_common),]
  }
  # else if(nrow(oxygen_bin) == 2){
  #   if(mean(oxygen_bin[[col_index]])>=0.5){
  #     oxygen_bin[[col_index]] <- 1
  #     oxygen_bin <- oxygen_bin |> distinct()
  #   }else{
  #     oxygen_bin <- oxygen_bin |> distinct()
  #   }
  #   
  # }
  else if(nrow(oxygen_bin) < 2){
    oxygen_bin <- oxygen_bin
  }
}

oxygen <- oxygen_bin |> unite(., sep = "") |>   pull(.) |> strtoi(base = 2)



co2_bin  <- diagnostics

for(n in 1:ncol(diagnostics)){
  col_index <- names(diagnostics)[n]
  if(nrow(co2_bin) > 2){
    vals <- co2_bin[,n] |> pull()
    most_common <- round(median(vals))
    least_common <- abs(most_common-1)
    co2_bin <- co2_bin[which(co2_bin[[col_index]] == least_common),]
  }
  # else if(nrow(co2_bin) == 2){
  #   if(mean(co2_bin[[col_index]]) <= 0.5){
  #     co2_bin[[col_index]] <- 0
  #     co2_bin <- co2_bin |> distinct()
  #   }else{
  #     co2_bin <- co2_bin |> distinct()
  #   }
  #   
  # }
  else if(nrow(co2_bin) <2){
    co2_bin <- co2_bin
  }
}

co2 <- co2_bin |> unite(., sep = "") |>   pull(.) |> strtoi(base = 2)


oxygen * co2
