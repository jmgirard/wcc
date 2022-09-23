xraw <- vroom::vroom(
  "Z:/DAPPeR/OpenFace/Phase 1/Raw/1002_2001_01_P.csv",
  delim = ",",
  col_select = c(time, frame, xc = confidence,
                 xRx = pose_Rx, xRy = pose_Ry, xRz = pose_Rz),
  show_col_types = FALSE
)

yraw <- vroom::vroom(
  "Z:/DAPPeR/OpenFace/Phase 1/Raw/1002_2001_01_C.csv",
  delim = ",",
  col_select = c(time, frame, yc = confidence,
                 yRx = pose_Rx, yRy = pose_Ry, yRz = pose_Rz),
  show_col_types = FALSE
)

df <- dplyr::inner_join(xraw, yraw, by = c("time", "frame"))


head_velocity <-
  df |>
  # Replace low confidence observations with NA
  dplyr::mutate(
    xRx = dplyr::case_when(xc >= 0.5 ~ xRx, TRUE ~ NA_real_),
    xRy = dplyr::case_when(xc >= 0.5 ~ xRy, TRUE ~ NA_real_),
    xRz = dplyr::case_when(xc >= 0.5 ~ xRz, TRUE ~ NA_real_),
    yRx = dplyr::case_when(yc >= 0.5 ~ yRx, TRUE ~ NA_real_),
    yRy = dplyr::case_when(yc >= 0.5 ~ yRy, TRUE ~ NA_real_),
    yRz = dplyr::case_when(yc >= 0.5 ~ yRz, TRUE ~ NA_real_)
  ) |>
  # Apply rolling mean to smooth
  dplyr::mutate(
    xSx = zoo::rollmean(xRx, k = 9, fill = NA),
    xSy = zoo::rollmean(xRy, k = 9, fill = NA),
    xSz = zoo::rollmean(xRz, k = 9, fill = NA),
    ySx = zoo::rollmean(yRx, k = 9, fill = NA),
    ySy = zoo::rollmean(yRy, k = 9, fill = NA),
    ySz = zoo::rollmean(yRz, k = 9, fill = NA),
  ) |>
  dplyr::mutate(
    xV = v_xyz(t = time, x = xSx, y = xSy, z = xSz),
    yV = v_xyz(t = time, x = ySx, y = ySy, z = ySz)
  ) |>
  # # Finalize
  dplyr::select(time, frame, x = xV, y = yV) |>
  tidyr::drop_na()


usethis::use_data(head_velocity, overwrite = TRUE)
