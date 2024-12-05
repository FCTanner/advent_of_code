path <- "2024/2024-05/input"
input <- readChar(path, nchars = file.info(path)$size)

separated <- input |> 
  stringr::str_split("\r\n\r\n") |> 
  unlist()

rules <- separated[1] |> 
  stringr::str_extract_all("\\d{2}\\|\\d{2}") |> 
  unlist() |> 
  stringr::str_split("\\|") |> 
  purrr::map(as.integer)

updates <- separated[2] |> 
  stringr::str_split("\\r\\n") |> 
  unlist() |> 
  purrr::map(\(x) {
    line <- stringr::str_split(x, ",") |> 
      unlist()
    as.integer(line)
  })

rm(list = c("input", "path", "separated"))


# Part 1 ------------------------------------------------------------------


# in an update, if both pages that are included in a rule need to be printed, 
# they must be in the order of the rule 

# if correct, add up page number of middle page

#' Gets rules that apply for an update
get_rules <- function(update, rules) {
  stopifnot(length(update) > 0)
  rules |> 
    purrr::keep(\(x) all(x %in% update))
}


#' Check if rule applies
check_rule <- function(update, rule) {
  
  id_p1 <- which(update == rule[1])
  id_p2 <- which(update == rule[2])
  
  id_p1 < id_p2
}


#' Get middle number
get_middle_page_number <- function(update) {
  n_pages <- length(update)
  idx <- (n_pages + 1) / 2
  stopifnot(rlang::is_integerish(idx))
  update[idx]
}


is_correct_update <- updates |> 
  purrr::map_lgl(\(update) {
    matching_rules <- get_rules(update, rules)
    
    rules_passed <- matching_rules |> 
      purrr::map_lgl(\(rule) check_rule(update, rule))
    
    all(rules_passed == TRUE)
    
  }) 


correct_updates <- updates[is_correct_update]

correct_updates_middle_numbers <- correct_updates |> 
  purrr::map(\(update) {
    matching_rules <- get_rules(update, rules)
    
    rules_passed <- matching_rules |> 
      purrr::map_lgl(\(rule) check_rule(update, rule))
    
    if (all(rules_passed == TRUE)) {
      get_middle_page_number(update)
    }
  }) 


p1_solution <- correct_updates |> 
  purrr::map(get_middle_page_number) |> 
  unlist() |> 
  sum()

p1_solution


# Part 2 ------------------------------------------------------------------

incorrect_updates <- updates[!is_correct_update]

apply_rule <- function(update, rule) {
  
  r1 <- rule[1]
  r2 <- rule[2]
  i_r1 <- which(update == r1)
  i_r2 <- which(update == r2)
  
  if (i_r1 > i_r2) {
    
    if (i_r2 == 1) { # Extra case for moving r1 to front of vector
      update <- c(r1, update[-i_r1])
      return(update)
    }
    
    update <- append(update[-i_r1], r1, i_r2 - 1)
  }
  
  return(update)
}


order_update_according_to_rules <- function(update, rules) {
  
  relevant_rules <- get_rules(update, rules)
  relevant_rules |> 
    purrr::walk(\(rule) {
      update <<- apply_rule(update, rule)
    }) |> 
    purrr::walk(\(rule) {
      update <<- apply_rule(update, rule)
    })
  
  return(update)
}


p2_solution <- incorrect_updates |> 
  purrr::map(\(x) order_update_according_to_rules(x, rules))  |> 
  purrr::map(get_middle_page_number) |> 
  unlist() |> 
  sum()

p2_solution


# Tests -------------------------------------------------------------------

testthat::test_that("Get rules works",{
  update <- c(1,4,5,6,7)
  rules <- list(
    c(1,4),
    c(6,7),
    c(5,1),
    c(10,5)
  )
  
  get_rules(update, rules) |> 
    testthat::expect_identical(list(
      c(1,4),
      c(6,7),
      c(5,1)
    ))
  
  get_rules(update = c(), rules) |> 
    testthat::expect_error()
})

testthat::test_that("Check rules works",{
  
  update <- c(1,4,5,6,7)
  rule1  <- c(1,4)
  rule2  <- c(4,1)
  
  check_rule(update, rule1) |> 
    testthat::expect_true()
  
  check_rule(update, rule2) |> 
    testthat::expect_false()
})


testthat::test_that("Get middle page works",{
  
  update1 <- c(1,4,5,6,7)
  update2 <- c(1,2,3)
  update3 <- c(1,2,3,4)
  
  get_middle_page_number(update1) |> 
    testthat::expect_equal(5)
  
  get_middle_page_number(update2) |> 
    testthat::expect_equal(2)
  
  get_middle_page_number(update3) |> 
    testthat::expect_error()
})



testthat::test_that("Rules are applied",{
  
  update1 <- c(1,4,5,6,7)
  rule1 <- c(6, 4)
  expected1 <- c(1,6,4,5,7)
  
  
  update2 <- c(1,4,5,6,7)
  rule2 <- c(6, 1)
  expected2 <- c(6, 1, 4, 5, 7)
  
  
  update3 <- c(95, 82, 85, 12, 32)
  rule3 <- c(95, 12)
  expected3 <- update3
  
  update1 |> 
    apply_rule(rule1) |> 
    testthat::expect_identical(expected1)
  
  update2 |> 
    apply_rule(rule2) |> 
    testthat::expect_identical(expected2)
  
  update3 |> 
    apply_rule(rule3) |> 
    testthat::expect_identical(expected3)
})



testthat::test_that("Multiple rules are applied",{
  
  update1 <- c(12L, 32L, 85L, 95L, 82L)
  rules1 <- list(
    c(82L, 12L), 
    c(85L, 12L), 
    c(95L, 85L), 
    c(95L, 82L), 
    c(95L, 12L), 
    c(95L, 32L), 
    c(32L, 12L), 
    c(32L, 85L), 
    c(82L, 32L), 
    c(82L, 85L)
  )
  
  expected1 <- c(95L, 82L, 32L, 85L, 12L)
  
  update1 |> 
    order_update_according_to_rules(rules1) |> 
    testthat::expect_identical(expected1)
  
  update2 <- c(65L, 83L, 88L, 39L, 11L, 38L, 82L)  
  expected2 <- c(65L, 88L, 38L, 82L, 11L, 83L, 39L)
  
  rules2 <- list(c(82L, 11L), c(11L, 39L), c(11L, 83L), c(88L, 39L), 
                 c(88L, 38L), c(88L, 82L), c(88L, 11L), c(88L, 83L), 
                 c(65L, 11L), c(65L, 83L), c(65L, 39L), c(65L, 82), 
                 c(65L, 88L), c(65L, 38L), c(38L, 82L), c(38L, 83L), 38:39, 
                 c(38L, 11L), c(83L, 39L), 82:83, c(82L, 9L))
  
  
  update2 |> 
    order_update_according_to_rules(rules2) |> 
    testthat::expect_identical(expected2)
})

