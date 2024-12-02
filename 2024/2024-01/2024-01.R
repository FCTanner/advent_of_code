dat <- read.delim("2024/2024-01/input", sep = " ", header = FALSE) |> 
  as.data.frame() |>
  dplyr::select(V1, V4)


# Part 1 ------------------------------------------------------------------

l1 <- dat$V1 |> sort()
l2 <- dat$V4 |> sort()

abs(l1 - l2) |> sum()


# Part 2 ------------------------------------------------------------------

# Calculate counts

dat_as_char <- dat |> 
  dplyr::mutate(across(.cols = dplyr::everything(), .fns= as.character)) 

count_right <- dat_as_char |> 
  dplyr::group_by(V4) |> 
  dplyr::count() |> 
  dplyr::rename(number = V4)

# Multiply number by count + Sum

dat_as_char |> 
  dplyr::select("V1") |> 
  dplyr::rename(number = V1) |> 
  dplyr::left_join(count_right) |> 
  dplyr::mutate(
    number = as.numeric(number),
    similarity = number * n) |> 
  dplyr::summarise(
    score = sum(similarity,na.rm = TRUE)
  )

