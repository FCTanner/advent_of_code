path <- "2024/2024-03/input"
dat <- readChar(path, nchars = file.info(path)$size)


# Part 1 ------------------------------------------------------------------

get_list_of_chars <- function(dat) {
  
  regex <- "mul\\(\\d{1,3}, ?\\d{1,3}\\)"
  
  stringr::str_extract_all(dat, regex) |> 
    purrr::map(\(x) {
      stringr::str_remove(x, "mul\\(") |> 
        stringr::str_remove("\\)") |> 
        stringr::str_split(",") 
    }) |> 
    purrr::list_flatten() 
}

cast_multiply_and_add <- function(list_of_chars) {
  list_of_chars |> 
    purrr::map(\(x) {
      x <- as.integer(x)
      x[[1]] * x[[2]]
    }) |> 
    unlist() |> 
    sum()
}
  
dat |> 
  get_list_of_chars() |> 
  cast_multiply_and_add()


# Part 2 ------------------------------------------------------------------

# first remove all sections enframed by "don't()" "do()", then rerun 

dont_do_regex <- "don't\\(\\).*?do\\(\\)"

dat |> 
  stringr::str_remove_all(dont_do_regex) |> 
  paste0() |> 
  get_list_of_chars() |> 
  cast_multiply_and_add()


dont_regex <- "don't\\(\\)"
do_regex <- "do\\(\\)"


dat |> 
  stringr::str_view(dont_do_regex) 

dat |> 
  stringr::str_extract_all(dont_do_regex) |> 
  paste0() |> 
  stringr::str_view(do_regex) 


text <- "sdfsdf ?don't() sdf{don't()something do() +don't()ßß?do()"
stringr::str_remove_all(text, dont_do_regex)
