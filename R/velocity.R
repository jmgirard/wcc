#' @export
v_xyz <- function(t, x, y, z) {
  df <- data.frame(t, x, y, z)

  df$tm1 <- Hmisc::Lag(df$t, -1)
  df$tp1 <- Hmisc::Lag(df$t, +1)
  df$xm1 <- Hmisc::Lag(df$x, -1)
  df$xp1 <- Hmisc::Lag(df$x, +1)
  df$ym1 <- Hmisc::Lag(df$y, -1)
  df$yp1 <- Hmisc::Lag(df$y, +1)
  df$zm1 <- Hmisc::Lag(df$z, -1)
  df$zp1 <- Hmisc::Lag(df$z, +1)

  v <- sqrt(
    ( (df$xp1 - df$xm1) / (2*(df$tp1 - df$tm1)) )^2 +
      ( (df$yp1 - df$ym1) / (2*(df$tp1 - df$tm1)) )^2 +
        ( (df$zp1 - df$zm1) / (2*(df$tp1 - df$tm1)) )^2
  )

  v
}

#' @export
v_y <- function(t, y) {
  df <- data.frame(t, y)

  df$tm1 <- Hmisc::Lag(df$t, -1)
  df$tp1 <- Hmisc::Lag(df$t, +1)
  df$ym1 <- Hmisc::Lag(df$y, -1)
  df$yp1 <- Hmisc::Lag(df$y, +1)

  v <- sqrt(((df$yp1 - df$ym1) / (2*(df$tp1 - df$tm1)))^2)

  v
}

#' @export
v_x <- function(t, x) {
  df <- data.frame(t, x)

  df$tm1 <- Hmisc::Lag(df$t, -1)
  df$tp1 <- Hmisc::Lag(df$t, +1)
  df$xm1 <- Hmisc::Lag(df$x, -1)
  df$xp1 <- Hmisc::Lag(df$x, +1)

  v <- sqrt(((df$xp1 - df$xm1) / (2*(df$tp1 - df$tm1)))^2)

  v
}

#' @export
v_z <- function(t, z) {
  df <- data.frame(t, z)

  df$tm1 <- Hmisc::Lag(df$t, -1)
  df$tp1 <- Hmisc::Lag(df$t, +1)
  df$zm1 <- Hmisc::Lag(df$z, -1)
  df$zp1 <- Hmisc::Lag(df$z, +1)

  v <- sqrt(((df$zp1 - df$zm1) / (2*(df$tp1 - df$tm1)))^2)

  v
}
