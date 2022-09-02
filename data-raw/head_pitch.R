xraw <- vroom::vroom(
  "Z:/DAPPeR/OpenFace/csv/Raw/1002_2001_01_P.csv",
  delim = ",",
  col_select = c(time, frame, xc = confidence, x = pose_Ry),
  show_col_types = FALSE
)

yraw <- vroom::vroom(
  "Z:/DAPPeR/OpenFace/csv/Raw/1002_2001_01_C.csv",
  delim = ",",
  col_select = c(time, frame, yc = confidence, y = pose_Ry),
  show_col_types = FALSE
)

df <- dplyr::inner_join(xraw, yraw, by = c("time", "frame"))


head_pitch <-
  df |>
  # Replace low confidence observations with NA
  dplyr::mutate(
    x = dplyr::case_when(xc >= 0.5 ~ x, TRUE ~ NA_real_),
    y = dplyr::case_when(yc >= 0.5 ~ y, TRUE ~ NA_real_)
  ) |>
  # Apply rolling mean to smooth
  dplyr::mutate(
    xs = zoo::rollmean(x, k = 9, fill = NA),
    ys = zoo::rollmean(y, k = 9, fill = NA)
  ) |>
  # Finalize
  dplyr::select(time, frame, x = xs, y = ys) |>
  tidyr::drop_na()


usethis::use_data(head_pitch, overwrite = TRUE)
