

#' Get the period of an vector object.
#'
#' This is a function to find the period of an vector object.
#' The vector object is representing a time series.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param x A vector object representing a time series.
#' @return The period of \code{x}.
#' If the input is not a vector, an error message is returned.
#' @examples
#' ts_vector = c(1,4,6,1,24,5,1)
#' get_ts_frequency(x = ts_vector)
get_ts_frequency <- function(x){
  # --------------------------------------------------------------------
  # The code is adopted from Hyndman, Rob J. which provides him under:
  # https://robjhyndman.com/hyndsight/tscharacteristics/
  # Decomposition of the ts object
  # If else clause to check the input
  if (is.vector(x)) {
    x_ts <- as.ts(x)
    # Remove trend from data
    x_ts <- residuals(forecast::tslm(x_ts ~ trend))
    # Compute spectrum by fitting ar model to largest section of x
    n_freq <- 500
    spec <- spec.ar(c(na.contiguous(x_ts)), plot = FALSE, n.freq = n_freq)
    # Arbitrary threshold chosen by trial and error.
    if (max(spec[["spec"]]) > 10) {
      period <- floor(1 / spec[["freq"]][which.max(spec[["spec"]])] + 0.5)
      if (period == Inf) {  # Find next local maximum
        j <- which(diff(spec[["spec"]]) > 0)
        if (length(j) > 0) {
          nextmax <- j[1] + which.max(spec[["spec"]][(j[1] + 1):n_freq])
          if (nextmax < length(spec[["freq"]])){
            period <- floor(1/spec[["freq"]][nextmax] + 0.5)
          } else {
            period <- 1L
          }
        } else {
          period <- 1L
        }
      }
    } else {
      period <- 1L
    }
    return(as.integer(period))
  } else {
    print("A vector object is required as input!")
  }

}

#' Decompose a time series object into trend, seasonal and remainder.
#'
#' This is a function to decompose a time series object.
#' The time series object is decomposed into trend, seasonal and remainder.
#' Remainder is the ts minus the trend and seasonal.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param transform A boolean to enable BoxCox transformation.
#' @return A list of the decomposed \code{ts}. If the input is not a ts,
#' an error message is returned. The list consist of the ts, the trend of
#' the ts, the seasonal of the ts and the remainder of the ts.
#' @examples
#' decompose_ts(ts = datasets::BJsales, transform = TRUE)
decompose_ts <- function(ts, transform = TRUE){
  # If else clause to check the input
  if (is.ts(ts)) {
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Transform ts
    if (transform & min(ts, na.rm = TRUE) >= 0) {
      lambda <- BoxCox.lambda(na.contiguous(ts))
      ts <- BoxCox(ts, lambda)
    } else {
      lambda <- NULL
      transform <- FALSE
    }

    # If seasonal data
    if(stats::frequency(as.numeric(ts)) > 1)
    {
      ts_stl <- stl(ts, s.window = "periodic", na.action = na.contiguous)
      trend <- ts_stl[["time.series"]][,2]
      season <- ts_stl[["time.series"]][,1]
      remainder <- ts - trend - season
    }
    else  # If nonseasonal data
    {
      tt <- 1:length(ts)
      trend <- rep(NA, length(ts))
      trend[!is.na(ts)] <- fitted(gam(ts ~ s(tt)))
      season <- NULL
      remainder <- ts - trend
    }
    return(list(ts = ts, trend = trend, season = season, remainder = remainder,
                transform = transform, lambda = lambda))
  } else {
    print("A time series object is required as input!")
  }

}

#' Scale a numeric value into a standardized interval [0,1].
#'
#' This function scales a numeric value into a standardized interval [0,1].
#' A linear transformation method is used to map values of
#' (-infinity,infinity) to [0,1].
#' As input for \code{x}, \code{min}, \code{max} is only required an numeric
#' value. Otherwise the function returns an error message.
#'
#' @param x A numeric value that should be normalized.
#' @param min A numeric value.
#' @param max A numeric value.
#' @return A scaled value between [0,1] of \code{x}.
#' If the input \code{x},\code{min} or \code{max} is not a numeric value
#' or min is not smaller than max (min < max), an error message is returned.
#' @examples
#' scale_feature(x = 2.5, min = 1, max = 5)
scale_feature <- function(x, min, max) {

  if (is.numeric(x) & is.numeric(min) & is.numeric(max) & min < max) {
    if (min < max) {
      # Maps (-infinity,infinity) to [0,1]
      scaled_x = (x - min) / (max - min)
      return(scaled_x)
    } else {
      print("min must be smaller than max (min<max)!")
    }
  } else {
    print("Only numeric values for x, min and max are allowed!")
  }

}

#' Replace all missing observations of a time series or vector object
#'
#' This is a function to Replace all missing observations of a time series or
#' vector object. For \code{na_option} is only required the string 'mean' or
#' 'kalman' allowed. This means, that all na values are either replaced by the
#' mean, or kalman imputation of the ts. The standard value of \code{na_option}
#' is 'mean'. Otherwise the function returns an error message.
#'
#' @param ts A time series or vector object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The na imputated \code{ts}. If the input is not a ts or
#' vector, an error message is returned.
#' @examples
#' predict_missing_observations(ts = datasets::BJsales)
predict_missing_observations <- function(ts, na_option = "mean") {
  # If else clause to check the 'na_option' input parameter
  if (is.ts(ts) | is.vector(ts)) {
    if (na_option == "mean") {
      # Replace all NA by mean
      adjusted_ts <- na.mean(ts, option = na_option)
      return(adjusted_ts)
    } else if (na_option == "kalman") {
      if ((length(as.numeric(ts)) - length(na.omit(as.numeric(ts)))) >= 3){
        # Replace all NA by kalman
        adjusted_ts <- na.kalman(ts)
        return(adjusted_ts)
      } else {
        print("For kalman imputation are at least 3 NA observations
              + required, thus the simple 'mean' imputation is used")
        # Replace all NA by mean
        adjusted_ts <- na.mean(ts, option = na_option)
        return(adjusted_ts)
      }

    } else {
      stop("ERROR: input parameter 'na_option' has to contain either
           'mean' or 'kalman'")
    }

  } else {
    stop("An time series or vector object is required!")
  }
}
