# Red-Nosed Reports


# Conditions --------------------------------------------------------------

dat <- readLines("2024/2024-02/input") |> 
  strsplit("\\s+") |> 
  purrr::map(as.integer)

# Part 1 ------------------------------------------------------------------

is_all_one_sign <- function(lst) {
  signs <- sign(lst)
  all(signs == signs[1])
}

is_between_one_and_three <- function(lst) {
  lst |> 
    purrr::map_lgl(\(x) 1 <= abs(x) && abs(x) <= 3) |> 
    all()
}

safe_p1 <- dat |> 
  purrr::map(diff) |> 
  purrr::keep(is_between_one_and_three) |> 
  purrr::keep(is_all_one_sign)

length(safe_p1)


# Part 2 ------------------------------------------------------------------

# remove a level at a time

generate_dampened <- function(values) {
  variants <- list(original = values)
  
  # Add variants with each individual value removed
  for (i in seq_along(values)) {
    variants[[paste0("dampened_", i)]] <- values[-i]
  }
  
  return(variants)
}

safe_p2 <- dat |> 
  purrr::map(\(x) {
    generate_dampened(x) |> 
      purrr::map(diff) |> 
      purrr::keep(is_between_one_and_three) |> 
      purrr::keep(is_all_one_sign) 
}) |> 
  purrr::keep(rlang::has_length)

length(safe_p2)

