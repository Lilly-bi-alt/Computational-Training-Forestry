#Coefficient of Variation (CV%)
"CV% = (sd / mean)*100"
"mean = sum(x)/length(n)"
"sd = sqrt(sum((x - mean)^2)/(length(n)-1))"

#First (Mean) function 
get_mean <- function(x) {
    x <- x[!is.na(x)]  # Remove NA values
    n <- length(x)
    total <- sum(x)
    mean_value <- total / n 
    return(mean_value)
}

#second (SD) function 
get_sd <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  n <- length(x)
  if (n < 2) {
    stop("At least two non-NA values are required to calculate standard deviation.")
  }

  mean_value <- get_mean(x)
  deviation <- x - mean_value
  squared_diffs <- deviation^2
  sum_squared_diffs <- sum(squared_diffs)
  variance <- sum_squared_diffs / (n - 1)  # Sample variance
  sd_value <- sqrt(variance)
  return(sd_value)
}

#CV function
calc_cv <- function(x) {
sd_result <- get_sd(x)
mean_result <- get_mean(x)

if(mean_result == 0) {
  stop("Mean is zero, So, CV is undefined.")
}
total_cv <- (sd_result / mean_result)
return(total_cv)
}

cv_to_percent <- function(total_cv) {
  cv_percent <- total_cv * 100
  return(cv_percent)
}

Final_cv <- function(x) {
  mean_value <- get_mean(x)
  sd_value <- get_sd(x)
  total_cv <- calc_cv(x)
  Final_cv <- cv_to_percent(total_cv)
  return(Final_cv)
    }

#GUARD FUNCTION
check_positive <- function(x) {
  if (!is.numeric(x)) {
    stop("Guard Error: Data must be numeric.")
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    stop("Guard Error: No valid values to check.")
  }
  n_negative <- sum(x < 0)
  n_zero <- sum(x == 0)
  if (n_negative > 0) {
    stop(paste("Guard Error: Found", n_negative, "negative value(s)."))
  }
  if (n_zero > 0) {
    stop(paste("Guard Error: Found", n_zero, "zero value(s)."))
  }
  return(TRUE)
}
x<- c(12, 28, 30, 34, 38)
Final_cv(x)
