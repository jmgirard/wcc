# Still in development

calc_peak <- function(rvec, tvec, L_size, sdeg = 2, sspan = 0.25) {

  # rvec <- economics[1:81,]$uempmed
  # tvec <- -40:40

  # Smooth the row vector using loess
  fit <- stats::loess(rvec ~ tvec, span = sspan, degree = sdeg)

  # Step 1: Calculate search region size
  R_size <- ceiling(L_size / 2)

  c <- 0

    ## Step 2: Define search region R
    R <- (c - R_size):(c + R_size)

    ## Step 3: Define local region L1
    L1 <- min(R):(min(R) + L_size)
    L1_v <- predict(fit, newdata = L1)

    ## Step 4: Calculate maximum value in L1 region
    L1_v_max <- max(L1_v, na.rm = TRUE)
    L1_max <- L1[which.max(L1_v)]

    ## Step 5a: Check if L1_max is the center element of L1
    check1 <- L1_max == median(L1)

    ## Step 5b: Check if the values from L1_v on either side of L1_max are monotonically decreasing
    #check2 <-

    #exit if done

    ## Step 6: Define local region L2
    L2 <- (max(R) - L_size):max(R)
    L2_v <- predict(fit, newdata = L2)
    L2_v_max <- max(L2_v, na.rm = TRUE)
    L2_max <- L2[which.max(L2_v)]

    ## Step 7a: Check if L2_max is the center element of L2
    check3 <- L2_max == median(L2)

    ## Step 7b: Check if the values from L2_v on either side of L2_max are monotonically decreasing
    #check4 <-

    # exit if done

    ## Step 8: Increment R_size by one, then repeat from Step 2 if R_size <= tau_max

    ## Step 9: If R_size > tau_max, then no viable peak was found, return NAs

}
