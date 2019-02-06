
#' Generates the skewness of a ts object.
#'
#' This is a function to generate the skewness of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param type integer of 1 to 3 defining one of the three skewness algorithms
#' of the skewness function from the e1071 R package.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The skewness of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_skewness(ts = datasets::BJsales)
#' @export
calculate_skewness <- function(ts, na_option = "mean", type = 3){
  # If else clause to check the input
  if(is.ts(ts) & type %in% c(1, 2, 3) & na_option %in% c("mean", "kalman")){
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    return(e1071::skewness(ts, type = type))
  } else {
    stop("A time series object is required as input! For type only an integer
         between 1 to 3 and for na_option only 'mean' or 'kalman'")
  }
}

#' Generates the kurtosis of a ts object.
#'
#' This is a function to generate the kurtosis of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param type integer of 1 to 3 defining one of the three skewness algorithms
#' of the skewness function from the e1071 R package.
#' @return The skewness of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_kurtosis(ts = datasets::BJsales)
#' @export
calculate_kurtosis <- function(ts, na_option = "mean", type = 3){
  # If else clause to check the input
  if(is.ts(ts) & type %in% c(1, 2, 3) & na_option %in% c("mean", "kalman")){
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    return(e1071::kurtosis(ts, na.rm = FALSE, type = type))
  } else {
    stop("A time series object is required as input! For type only an integer
         between 1 to 3 and for na_option only 'mean' or 'kalman'")
  }
}

#' Generates the trend of a ts object.
#'
#' This is a function to generate the trend of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The trend of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_trend(ts = datasets::BJsales)
#' @export
calculate_trend <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    freq <- get_ts_frequency(as.numeric(ts))
    decomposed_ts <- decompose_ts(ts)

    # Adjust the data
    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {
      fits <- decomposed_ts[["trend"]] + decomposed_ts[["season"]]
    } else { # Nonseasonal data
      fits <- decomposed_ts[["trend"]]
    }
    adjusted_ts <- decomposed_ts[["ts"]] - fits +
      mean(decomposed_ts[["trend"]], na.rm = TRUE)

    variance_adj_ts <- var(adjusted_ts, na.rm = TRUE)

    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {  # If seasonal data
      deseason <- decomposed_ts[["ts"]] - decomposed_ts[["season"]]
      trend <- ifelse (var(deseason, na.rm = TRUE) < 1e-10, 0,
                       max(0, min(1, 1 - variance_adj_ts /
                                    var(deseason, na.rm = TRUE))))
    } else {  # If non-seasonal data
      trend <- ifelse(var(decomposed_ts[["ts"]],na.rm = TRUE) < 1e-10, 0,
                      max(0,min(1,1-variance_adj_ts /
                                  var(decomposed_ts[["ts"]], na.rm = TRUE))))
    }
    return(trend)
  } else {
    stop("A time series object is required as input! For na_option only
         'mean' or 'kalman' are allowed!")
  }

}

#' Generates the mean of the autocorrelationfunction (ACF) of a ts object.
#'
#' This is a function to generate the mean of the ACF of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param type String giving the type of acf to be computed. Allowed
#' values are "correlation" (the default), "covariance" or "partial".
#' Will be partially matched.
#' @param demean True/False boolean. Should the covariances be about the
#' sample means?
#' @return The mean of the ACF of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_autocorrelation(ts = datasets::BJsales)
#' @export
calculate_autocorrelation <- function(ts, na_option = "mean",
                                      type = "covariance", demean = TRUE){
  # If else clause to check the input
  if (is.ts(ts) & type %in% c("correlation", "covariance", "partial")
      & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    acf = stats::acf(ts, plot = FALSE, demean = demean, type = type)
    return(mean(acf$acf))
  } else {
    stop("A time series object is required as input! For type only
         'correlation' or 'covariance' or 'partial' is allowed. For na_option
         only 'mean' or 'kalman' are allowed!")
  }
}

#' Generates the mean of the normalized values [0,1] of a ts object.
#'
#' This is a function to generate the mean of the normalized values [0,1]
#' of a time series object. As input is only required an object from
#' the class time series. Otherwise the function returns an error message.
#'  Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The normalized mean of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_mean(ts = datasets::BJsales)
#' @export
calculate_mean <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    x = (ts - min(ts)) / (max(ts) - min(ts))
    return(mean(x))
  } else {
    stop("A time series object is required as input! For na_option only
         'mean' or 'kalman' are allowed!")
  }

}

#' Generates the standard deviation of a ts object.
#'
#' This function generates the standard deviation of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The standard deviation of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_sd(ts = datasets::BJsales)
#' @export
calculate_sd <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    return(sd(ts))
  } else {
    stop("A time series object is required as input! For na_option only
         'mean' or 'kalman' are allowed!")
  }

}

#' Generates the number of observations of a ts object.
#'
#' This is a function to generate the number of observations of an
#' time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The number of observations of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_observationnumber(ts = datasets::BJsales)
#' @export
calculate_observationnumber <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    return(length(as.numeric(ts)))
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the non linearity factor of a ts object.
#'
#' This function generates the non linearity factor of a time series object.
#' The Teraesvirta's neural network test for neglected nonlinearity is applied.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param scale Boolean, whether the data should be scaled before computing
#' the test statistic. Default value is 'TRUE'.
#' @param type String indicating whether the Chi-Squared test or the
#' F-test is computed. Thus, valid are "Chisq" (default) and "F".
#' @return The non linearity factor of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_non_linearity(ts = datasets::BJsales)
#' @export
calculate_non_linearity <- function(ts, na_option = "mean", scale = TRUE,
                                    type = "Chisq"){
  # If else clause to check the input
  if (is.ts(ts) & type %in% c("Chisq","F")
      & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    # use the terasvirta.test for non linearity check
    p <- tseries::terasvirta.test(na.contiguous(ts), scale = scale,
                                  type = type)[["statistic"]]
    return(p)
  } else {
    stop("A time series object is required as input! For type only
         'Chisq' or 'F' is allowed. For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the seasonality factor of a ts object.
#'
#' This function generates the seasonality factor of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The seasonality factor of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_seasonality(ts = datasets::BJsales)
#' @export
calculate_seasonality <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    # Decomposition
    freq <- get_ts_frequency(as.numeric(ts))
    decomposed_ts <- decompose_ts(ts)

    # Adjust data
    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {
      fits <- decomposed_ts[["trend"]] + decomposed_ts[["season"]]
    } else { # Nonseasonal data
      fits <- decomposed_ts[["trend"]]
    }
    adjusted_ts <- decomposed_ts[["ts"]] - fits +
      mean(decomposed_ts[["trend"]], na.rm=TRUE)

    variance_adj_ts <- var(adjusted_ts, na.rm=TRUE)

    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {
      detrend <- decomposed_ts[["ts"]] - decomposed_ts[["trend"]]
      season <- ifelse (var(detrend, na.rm = TRUE) < 1e-10, 0,
                       max(0, min(1, 1 - variance_adj_ts /
                                    var(detrend, na.rm=TRUE))))
    } else { # non-seasonal data
      season <- 0
    }
    return(season)
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the periodicity/frequency factor of a ts object.
#'
#' This function generates the periodicity/frequency factor of an
#' time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#' Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The periodicity/frequency factor of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_periodicity(ts = datasets::BJsales)
#' @export
calculate_periodicity <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    freq <- get_ts_frequency(as.numeric(ts))
    return((exp((freq - 1) / 50) - 1) / (1 + exp((freq - 1) / 50)))
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the maximum Lyapunov exponent (chaos) of a ts object.
#'
#' This is a function to generate the maximum Lyapunov exponent (chaos)
#' of a time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#' Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The maximum Lyapunov exponent (chaos)  of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_chaos(ts = datasets::BJsales)
#' @export
calculate_chaos <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    length <- calculate_observationnumber(ts)
    freq <- get_ts_frequency(as.numeric(ts))

    if (freq > length - 10) {
      return(as.numeric(0)) # Insufficient ts data
    } else {
      Ly <- numeric(length - freq)
      for (i in 1:(length - freq)) {
        idx <- order(abs(ts[i] - ts))
        idx <- idx[idx < (length - freq)]
        j <- idx[2]
        Ly[i] <- log(abs((ts[i + freq] - ts[j + freq]) /
                           (ts[i] - ts[j]))) / freq
        if(is.na(Ly[i]) | Ly[i] == Inf | Ly[i] == -Inf) Ly[i] <- NA
      }
      Lyap <- mean(Ly, na.rm=TRUE)
      fLyap <- exp(Lyap) / (1 + exp(Lyap))
      return(fLyap)
    }
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the approximate entropy of a ts object.
#'
#' This function generates the approximate entropy of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#' Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param edim The embedding dimension, as for chaotic time series;
#' a preferred value is 2.
#' @param r Filter factor; work on heart rate variability has suggested
#' setting r to be 0.2 times the standard deviation of the data.
#' @return The approximate entropy of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_entropy(ts = datasets::BJsales)
#' @export
calculate_entropy <- function(ts, na_option = "mean", edim = 2,
                              r = 0.2*sd(ts)){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # default settings: approx_entropy(ts, edim = 2, r = 0.2*sd(ts), elag = 1)
    return(pracma::approx_entropy(ts, edim = edim, r = r))
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the Hurst exponent of a ts object.
#'
#' This is a function to measure the self similarity by the Hurst exponent
#' of a time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#' Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#' The code is adopted from Hyndman, Rob J. which provides him under:
#' https://robjhyndman.com/hyndsight/tscharacteristics/
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param nar Number of autoregressive parameters p.
#' @param nma Number of moving average parameters q.
#' @return The Hurst exponent of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_selfsimilarity(ts = datasets::BJsales)
#' @export
calculate_selfsimilarity <- function(ts, na_option = "mean", nar = 0, nma = 0){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # --------------------------------------------------------------------
    # The code is adopted from Hyndman, Rob J. which provides him under:
    # https://robjhyndman.com/hyndsight/tscharacteristics/
    # Decomposition of the ts object
    hurst_exponent <- fracdiff::fracdiff(na.contiguous(ts),
                                         nar = nar, nma = nma)[["d"]] + 0.5
    return(hurst_exponent)
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the dynamic time warping (DTW) for a ts object to the 1000 ts
#' from the list in /data.
#'
#' This is a function to generate the DTW for a ts to the 13 blocks from the
#' 1000 ts in the un_ts_list and multi_ts_list in /data. Each block contains
#' 100 ts. As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param window_type Windowing function. Character: "none", "itakura",
#' "sakoechiba", "slantedband", or a function
#' @param window_size Integer window size for the window type.
#' @param dist_method Distance function to use.
#'
#' @return The DTW of \code{ts} to the 13 different blocks. Thus an vector
#' with 13 DTW distances is returned. The first number is the DTW to
#' block 1,..., the last number is the DTW to block 13.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_dtw_blockdistance(ts = datasets::BJsales)
#' @export
calculate_dtw_blockdistance <- function(ts, na_option = "mean",
                                        window_type = "slantedband",
                                        window_size = 100,
                                        dist_method = "Euclidean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    blocks <- tstaxonomyr::matrix_block_list
    # Distance vector for containing dtw distance for ts to all blocks
    distance_vector <- c()

    # Generate a transposed matrix of ts, a row contains a ts object
    temp_df <- as.data.frame(as.numeric(ts))
    temp_df <- data.table::transpose(temp_df)
    matrix_ts <- as.matrix.data.frame(temp_df)

    # Generate for each of the 13 blocks the DTW distance and store it
    for (elem in blocks) {
      # Get the DTW distance matrix
      alignment <- dtw::dtwDist(matrix_ts, elem,
                           window.type = window_type,
                           window.size = window_size,
                           dist.method = dist_method)

      # Mean over all the distances to the block elements
      dtw <- mean(alignment)

      # Append the block DTW distance to final vector
      distance_vector <- append(distance_vector, dtw)
    }
    return(distance_vector)
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the percentage of turning points of a ts object.
#'
#' This is a function to generate the percentage of turning points of
#' a time series object. Points are significant turning points if their
#' probability is smaller the significance level 5%.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The percentage of turning points of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_turningpoint_percentage(ts = datasets::BJsales)
#' @export
calculate_turningpoint_percentage <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }

    # Create turningpoint object
    tp <- pastecs::turnpoints(ts)
    # if number of tp is 0, return directly 0
    if (tp$nturns > 0) {
      # Get all turning points
      allTP <- summary(tp)
      # Indicate which turnpoints are significant with sign level (5%)
      # Get the number of significant tp
      sigTP <- sum(allTP$proba < 0.05)
    } else {
      return(0)
    }
    # if number of significant tp is 0, return directly 0
    if (length(sigTP) == 0){
      return(0)
    } else {
      return(sigTP / calculate_observationnumber(ts))
    }
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the mean of the partial autocorrelationfunction (PACF)
#' of a ts object.
#'
#' This is a function to generate the mean of the PACF of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The mean of the PACF of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_partial_autocorrelation(ts = datasets::BJsales)
#' @export
calculate_partial_autocorrelation <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    p_acf <- stats::pacf(ts, plot = FALSE)
    return(mean(p_acf$acf))
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the variance of a ts object.
#'
#' This is a function to generate the variance of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The variance of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_variance(ts = datasets::BJsales)
#' @export
calculate_variance <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    return(stats::var(ts))
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the percentage of outliers of a ts object.
#'
#' This is a function to generate the percentage of outliers of an
#' time series object. Points are significant outliers if there probability is
#' smaller the significance level 5%. A z_test, chisq_test and t_test is
#' applied to identify number of outliers. Then the mean of the three test
#' results is used as final number of outliers. As input is only required an
#' object from the class time series. Otherwise the function returns
#' an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The percentage of outliers of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_outlier_percentage(ts = datasets::BJsales)
#' @export
calculate_outlier_percentage <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    # All tests with significance level 5%
    z_test <- sum(outliers::scores(ts, type="z", prob = 0.95))
    chisq_test <- sum(outliers::scores(ts, type="chisq", prob = 0.95))
    t_test <- sum(outliers::scores(ts, type="t", prob = 0.95))
    # Get the mean over the 3 test results
    outlier_mean <- mean(c(z_test, chisq_test, t_test))
    # Return the outlier percentage of the series
    if (length(outlier_mean) == 0){
      return(0)
    } else {
      return(outlier_mean / calculate_observationnumber(ts))
    }
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the percentage of step changes of a ts object.
#'
#' This is a function to generate the percentage of step changes of
#' a time series object. As input is only required an object from the class
#' time series. Otherwise the function returns an error message.
#' Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The percentage of step changes of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_stepchange_percentage(ts = datasets::BJsales)
#' @export
calculate_stepchange_percentage <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    number_of_stepchanges = 0
    for(i in 5:calculate_observationnumber(ts)){
      if (base::abs(as.numeric(ts)[i] - base::mean(as.numeric(ts)[1:(i - 1)])) >
          2 * stats::sd(as.numeric(ts)[1:(i - 1)])) {
        number_of_stepchanges <- number_of_stepchanges + 1
      }
    }
    if (length(number_of_stepchanges) == 0){
      return(0)
    } else {
      return(number_of_stepchanges / calculate_observationnumber(ts))
    }

  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the percentage of peaks of a ts object.
#'
#' This function generates the percentage of peaks of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param minpeakheight Numeric, the minimum height a peak has to have
#' to be recognized as such
#' @param minpeakdistance Integer, the minimum distance (in indices) peaks
#' have to have to be counted
#' @param threshold Integer, minimum peak number
#'
#' @return The percentage of peaks of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_peak_percentage(ts = datasets::BJsales)
#' @export
calculate_peak_percentage <- function(ts, na_option = "mean",
                                      minpeakheight = 0.6,
                                      minpeakdistance = 1,
                                      threshold = 0){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }
    percentage_peaks <- nrow(pracma::findpeaks(as.numeric(ts),
                                          minpeakheight = minpeakheight,
                                          minpeakdistance = minpeakdistance,
                                          threshold = threshold)) /
                                          calculate_observationnumber(ts)
    if (length(percentage_peaks) == 0){
      return(0)
    } else {
      return(percentage_peaks)
    }
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the autocorrelation measure of a data.frame object.
#'
#' This function generates the autocorrelation measure of a data.frame object.
#' It is based on the durbin watson test. The test is based on detrended data.
#' The data.frame object should represent an multivariate ts. As input is
#' only required an object from the class data.frame.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param df A data.frame object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param targetcol A character containing a column name of the \code{df}.
#' @return The autocorrelation measure of \code{df}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_durbin_watson_test(
#' df = multi_ts_list$`M-TS-1`$data,
#' targetcol = colnames(multi_ts_list$`M-TS-1`$data)[1])
#' @export
calculate_durbin_watson_test <- function(df, targetcol, na_option = "mean"){
  # If else clause to check the input
  if (is.data.frame(df) & na_option %in% c("mean", "kalman") &
      targetcol %in% colnames(df)) {
    df_colnames <- colnames(df)[which(colnames(df) != "date")]
    df <- df[,df_colnames]
    colnames(df)[which(colnames(df) == targetcol)] <- "target"

    for(col in colnames(df)){
      # Handle missing observations
      if (length(na.omit(as.numeric(df[, col]))) !=
          (length(as.numeric(df[, col])))) {
        df[, col] <- predict_missing_observations(as.numeric(df[, col]),
                                                  na_option = na_option)
      }
    }

    # Transform the df into detrended data
    transform_df <- df
    for(col in colnames(df)){

      freq = get_ts_frequency(as.numeric(unlist(transform_df[col])))
      decomp <- decompose_ts(ts(as.numeric(unlist(transform_df[col])),
                                f = freq))
      deTREND <- decomp[["ts"]] - decomp[["trend"]]
      transform_df[col] <- deTREND
    }
    # Calculate DW on detrended df
    dw <- car::durbinWatsonTest(lm(target ~ ., data = df))
    return(dw$dw)
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed! The targetcol has to be an
         attribute of the inputed df!")
  }

}

#' Generates the percentage of the values in the 4 quartiles of a ts object.
#'
#' This is a function to generate the percentage of the values in the 4
#' quartiles of a time series object. As input is only required an object from
#' the class time series. Otherwise the function returns an error message.
#' Also, for \code{na_option} is only required the string 'mean' or'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param ts A time series object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @return The percentage of the values in the 4 quartiles of \code{ts}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_quartile_distribution(ts = datasets::BJsales)
#' @export
calculate_quartile_distribution <- function(ts, na_option = "mean"){
  # If else clause to check the input
  if (is.ts(ts) & na_option %in% c("mean", "kalman")) {
    # Handle missing observations
    if (length(na.omit(as.numeric(ts))) != (length(as.numeric(ts)))) {
      ts <- predict_missing_observations(ts, na_option = na_option)
    }

    observations <- calculate_observationnumber(ts)
    quartiles <- round(calculate_observationnumber(ts) / 4)
    distribution <- c()
    distribution <- append(distribution,
                           sum(abs(ts[1:quartiles])))
    distribution <- append(distribution,
                           sum(abs(ts[(quartiles + 1):(quartiles * 2)])))
    distribution <- append(distribution,
                           sum(abs(ts[((quartiles * 2) + 1):(quartiles * 3)])))
    distribution <- append(distribution,
                           sum(abs(ts[((quartiles * 3) + 1):observations])))
    return(distribution / sum(abs(ts)))
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed!")
  }

}

#' Generates the coefficient of determination (R2) of a data.frame object.
#'
#' This is a function to generate the R2 of a data.frame object.
#' As input is only required an object from the class data.frame.
#' The data.frame object should represent an multivariate ts.
#' Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean' or 'kalman'
#' allowed. This means, that all na values are either replaced by the mean,
#' or kalman imputation of the ts.
#' The standard value of \code{na_option} is 'mean'.
#'
#' @param df A data.frame object.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'mean'.
#' @param targetcol A character containing a column name of the \code{df}.
#' @return The R2 of \code{df}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_determination_coefficient(
#' df = multi_ts_list$`M-TS-1`$data,
#' targetcol = colnames(multi_ts_list$`M-TS-1`$data)[1])
#' @export
calculate_determination_coefficient <- function(df, targetcol,
                                                na_option = "mean"){
  # If else clause to check the input
  if (is.data.frame(df) & na_option %in% c("mean", "kalman") &
      targetcol %in% colnames(df)) {

    df_colnames <- colnames(df)[which(colnames(df) != "date")]
    df <- df[,df_colnames]
    colnames(df)[which(colnames(df) == targetcol)] <- "target"

    for(col in colnames(df)){
      # Handle missing observations
      if (length(na.omit(as.numeric(df[, col]))) !=
          (length(as.numeric(df[, col])))) {
        df[, col] <- predict_missing_observations(as.numeric(df[, col]),
                                                  na_option = na_option)
      }
    }

    lm = stats::lm(target ~ ., data = df)
    return(summary(lm)$r.squared)
  } else {
    stop("A time series object is required as input! For na_option
         only 'mean' or 'kalman' is allowed! The targetcol has to be an
         attribute of the inputed df!")
  }

}


#' Generates the number of attributes of a data.frame object.
#'
#' This function generates the number of attributes of a data.frame object.
#' As input is only required an object from the class data.frame.
#' The data.frame object should represent an multivariate ts.
#' Otherwise the function returns an error message.
#'
#' @param df A data.frame object.
#' @return The number of attributes of \code{df}.
#' If the above input params are wrong, an error message is returned.
#' @examples
#' calculate_attributenumber(df = multi_ts_list$`M-TS-1`$data)
#' @export
calculate_attributenumber <- function(df){
  # If else clause to check the input
  if (is.data.frame(df)) {
    if (length(colnames(df)[str_detect(colnames(df),
                                  regex("date", ignore_case = TRUE))]) > 0) {
      # -1 because of the date col
      return(ncol(df) - 1)
    }else{
      return(ncol(df))
    }
  } else {
    stop("A data.frame object is required as input!")
  }

}
