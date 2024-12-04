input <- readLines("2024/2024-04/input", warn = FALSE)



# Part 1 ------------------------------------------------------------------


count_xmas <- function(x) {
  stringr::str_count(x, "XMAS") |> 
    sum()
}

verticalize_input <- function(x) {
  
  height <- length(x)
  width <- nchar(x[1])
  stopifnot(height == width)
  
  vertical <- rep(strrep("z", width), height)
  
  for (w in 1:width) {
    for (h in 1:height) {
      original_char <- substr(x[h], w, w) 
      stringr::str_sub(vertical[w], h, h) <- original_char
    }
  }
  return(vertical)
}


reverse_input <- function(x) {
  purrr::map_chr(stringr::str_split(x, "", simplify = FALSE), ~ stringr::str_c(rev(.x), collapse = ""))
}



diagonalize_input <- function(x) {
  
  height <- length(x)
  width <- nchar(x[1])
  stopifnot(height == width)
  
  scaffold_increasing <- purrr::map_chr(1:height, \(y) strrep("z", y))
  scaffold_decreasing <- rev(scaffold_increasing[-length(scaffold_increasing)])
  diagonal_scaffold <- c(scaffold_increasing, scaffold_decreasing)
  
  h_increasing_idx <- purrr::map(height:1, \(h) seq(h, height, 1))
  h_decreasing_idx <- purrr::map((height-1):1, \(h) seq(1, h, 1))
  
  h_idx <- c(h_increasing_idx, h_decreasing_idx) 
  
  w_increasing_idx <- rev(h_decreasing_idx)
  w_decreasing_idx <- rev(h_increasing_idx)
  
  w_idx <- c(w_increasing_idx, w_decreasing_idx) 
  
  purrr::iwalk(diagonal_scaffold, \(diag_line, idx) {
    
    for (i in 1:nchar(diag_line)) {
      
      x_h_index <- h_idx[[idx]][i]
      x_w_index <- w_idx[[idx]][i]
      
      original_char <- substr(x[x_h_index], x_w_index, x_w_index) 
      stringr::str_sub(diagonal_scaffold[idx], i, i) <<- original_char
    } 
    
  })
  
  return(diagonal_scaffold)
}


left_to_right <- input |> 
  count_xmas()

right_to_left <- input |> 
  reverse_input() |> 
  count_xmas() 


top_to_bottom <- input |> 
  verticalize_input() |> 
  count_xmas()

bottom_to_top <- input |> 
  verticalize_input() |> 
  reverse_input() |> 
  count_xmas() 


top_left_to_bottom_right <- input |> 
  diagonalize_input() |> 
  count_xmas() 


bottom_right_to_top_left <- input |> 
  diagonalize_input() |> 
  reverse_input() |> 
  count_xmas() 


bottom_left_to_top_right <- input |> 
  reverse_input() |> 
  diagonalize_input() |> 
  count_xmas() 


top_right_to_bottom_left <- input |> 
  reverse_input() |> 
  diagonalize_input() |> 
  reverse_input() |> 
  count_xmas() 


left_to_right +
  right_to_left +
  top_to_bottom +
  bottom_to_top +
  top_left_to_bottom_right +
  bottom_right_to_top_left +
  bottom_left_to_top_right +
  top_right_to_bottom_left



# Tests -------------------------------------------------------------------

testthat::test_that("Count xmas works", {
  
  xmas_input <- "XMASASDXMASXXMASMAS"
  
  testthat::expect_equal(count_xmas(xmas_input), 3)
})


testthat::test_that("Vertical works", {
  
  test_input <-c(
    "ABCD",
    "BCDE",
    "CDEF",
    "1234"
  )
  
  vertical_expected <- c(
    "ABC1",
    "BCD2",
    "CDE3",
    "DEF4"
  )
  
  testthat::expect_identical(verticalize_input(test_input), vertical_expected)
})


testthat::test_that("Reverse works", {
  
  test_input <-c(
    "ABCD",
    "BCDE",
    "CDEF",
    "1234"
  )
  
  reverse_expected <- c(
    "DCBA",
    "EDCB",
    "FEDC",
    "4321"
  )
  
  testthat::expect_identical(reverse_input(test_input), reverse_expected)
})



testthat::test_that("Diagonalize works", {
  
  test_input <-c(
    "ABCD",
    "BCDE",
    "CDEF",
    "1234"
  )
  
  diagonal_expected <- c(
    "1",
    "C2",
    "BD3",
    "ACE4",
    "BDF",
    "CE",
    "D"
  )
  
  testthat::expect_identical(diagonalize_input(test_input), diagonal_expected)
})




