
#' Generates the skewness of a ts object.
#'
#' This is a function to generate the skewness of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The skewness of \code{ts}.
#' If the input is not a ts object, an error message is returned.
#' @examples
#' calculate_skewness(ts = datasets::BJsales)
#' @export
calculate_skewness <- function(ts){
  # If else clause to check the input
  if(is.ts(ts)){
    return(skewness(ts, na.rm = FALSE, type = 3))
  } else {
    stop("A time series object is required as input!")
  }
}

#' Generates the kurtosis of a ts object.
#'
#' This is a function to generate the kurtosis of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The skewness of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_kurtosis(ts = datasets::BJsales)
#' @export
calculate_kurtosis <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    return(kurtosis(ts, na.rm = FALSE, type = 3))
  } else {
    stop("A time series object is required as input!")
  }
}

#' Generates the trend of a ts object.
#'
#' This is a function to generate the trend of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The trend of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_trend(ts = datasets::BJsales)
#' @export
calculate_trend <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
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
    stop("A time series object is required as input!")
  }

}

#' Generates the mean of the autocorrelationfunction (ACF) of a ts object.
#'
#' This is a function to generate the mean of the ACF of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The mean of the ACF of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_autocorrelation(ts = datasets::BJsales)
#' @export
calculate_autocorrelation <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    acf = acf(ts, plot = FALSE)
    return(mean(acf$acf))
  } else {
    stop("A time series object is required as input!")
  }
}

#' Generates the mean of the normalized values [0,1] of a ts object.
#'
#' This is a function to generate the mean of the normalized values [0,1]
#' of a time series object. As input is only required an object from
#' the class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The normalized mean of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_mean(ts = datasets::BJsales)
#' @export
calculate_mean <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    x = (ts - min(ts)) / (max(ts) - min(ts))
    return(mean(x))
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the standard deviation of a ts object.
#'
#' This function generates the standard deviation of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The standard deviation of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_sd(ts = datasets::BJsales)
#' @export
calculate_sd <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    return(sd(ts))
  } else {
    stop("A time series object is required as input!")
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
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The non linearity factor of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_non_linearity(ts = datasets::BJsales)
#' @export
calculate_non_linearity <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    # use the terasvirta.test for non linearity check
    p <- tseries::terasvirta.test(na.contiguous(ts))[["statistic"]]
    return(p)
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the seasonality factor of a ts object.
#'
#' This function generates the seasonality factor of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The seasonality factor of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_seasonality(ts = datasets::BJsales)
#' @export
calculate_seasonality <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    # Decomposition
    freq <- get_ts_frequency(as.numeric(ts))
    decomposed_ts <- decompose_ts(ts)

    # Adjust data
    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {
      fits <- decomposed_ts[["trend"]] + decomposed_ts[["season"]]
    } else { # Nonseasonal data
      fits <- decomposed_ts[["trend"]]
    }
    adjusted_ts <- decomposed_ts[["ts"]] - fits + mean(decomposed_ts[["trend"]], na.rm=TRUE)

    variance_adj_ts <- var(adjusted_ts, na.rm=TRUE)

    if (freq > 1 & !is.null(decomposed_ts[["season"]])) {
      detrend <- decomposed_ts[["ts"]] - decomposed_ts[["trend"]]
      season <- ifelse (var(detrend, na.rm = TRUE) < 1e-10, 0,
                       max(0, min(1, 1 - variance_adj_ts / var(detrend, na.rm=TRUE))))
    } else { # non-seasonal data
      season <- 0
    }
    return(season)
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the periodicity/frequency factor of a ts object.
#'
#' This function generates the periodicity/frequency factor of an
#' time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The periodicity/frequency factor of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_periodicity(ts = datasets::BJsales)
#' @export
calculate_periodicity <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    freq <- get_ts_frequency(as.numeric(ts))
    return((exp((freq - 1) / 50) - 1) / (1 + exp((freq - 1) / 50)))
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the maximum Lyapunov exponent (chaos) of a ts object.
#'
#' This is a function to generate the maximum Lyapunov exponent (chaos)
#' of a time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The maximum Lyapunov exponent (chaos)  of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_chaos(ts = datasets::BJsales)
#' @export
calculate_chaos <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
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
    stop("A time series object is required as input!")
  }

}

#' Generates the approximate entropy of a ts object.
#'
#' This function generates the approximate entropy of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The approximate entropy of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_entropy(ts = datasets::BJsales)
#' @export
calculate_entropy <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    # default settings: approx_entropy(ts, edim = 2, r = 0.2*sd(ts), elag = 1)
    return(approx_entropy(ts))
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the Hurst exponent of a ts object.
#'
#' This is a function to measure the self similarity by the Hurst exponent
#' of a time series object. As input is only required an object from the
#' class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The Hurst exponent of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_selfsimilarity(ts = datasets::BJsales)
#' @export
calculate_selfsimilarity <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    hurst_exponent <- fracdiff::fracdiff(na.contiguous(ts), 0, 0)[["d"]] + 0.5
    return(hurst_exponent)
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the dynamic time warping (DTW) for a ts object to the 1000 ts
#' from the list in /data.
#'
#' This is a function to generate the DTW for a ts to the 13 blocks from the
#' 1000 ts in the un_ts_list and multi_ts_list in /data. Each block contains 100 ts. As input is only
#' required an object from the class time series. Otherwise the function
#' returns an error message.
#'
#' @param ts A time series object.
#' @return The DTW of \code{ts} to the 13 different blocks. Thus an vector
#' with 13 DTW distances is returned. The first number is the DTW to
#' block 1,..., the last number is the DTW to block 13. If the input is not
#' a ts object, an error message is returned.
#' @examples
#' calculate_dtw_blockdistance(ts = datasets::BJsales)
#' @export
calculate_dtw_blockdistance <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    blocks <- tstaxonomyr::matrix_block_list
    # Distance vector for containing dtw distance for ts to all blocks
    distance_vector <- c()

    # Generate a transposed matrix of ts, a row contains a ts object
    temp_df <- as.data.frame(as.numeric(ts))
    temp_df <- transpose(temp_df)
    matrix_ts <- as.matrix.data.frame(temp_df)

    # Generate for each of the 13 blocks the DTW distance and store it
    for (elem in blocks) {
      # Get the DTW distance matrix
      alignment<-dtwDist(matrix_ts, elem)

      # Sum the results for the block
      dtw <- sum(alignment)

      # Append the block DTW distance to final vector
      distance_vector <- append(distance_vector,dtw)
    }
    return(distance_vector)
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the percentage of turning points of a ts object.
#'
#' This is a function to generate the percentage of turning points of
#' a time series object. Points are significant turning points if their
#' probability is smaller the significance level 5%.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The percentage of turning points of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_turningpoint_percentage(ts = datasets::BJsales)
#' @export
calculate_turningpoint_percentage <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    # Create turningpoint object
    tp <- turnpoints(ts)
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
    stop("A time series object is required as input!")
  }

}

#' Generates the mean of the partial autocorrelationfunction (PACF)
#' of a ts object.
#'
#' This is a function to generate the mean of the PACF of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The mean of the PACF of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_partial_autocorrelation(ts = datasets::BJsales)
#' @export
calculate_partial_autocorrelation <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    pacf <- pacf(ts, plot = FALSE)
    return(mean(pacf$acf))
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the variance of a ts object.
#'
#' This is a function to generate the variance of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The variance of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_variance(ts = datasets::BJsales)
#' @export
calculate_variance <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    return(var(ts))
  } else {
    stop("A time series object is required as input!")
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
#' an error message.
#'
#' @param ts A time series object.
#' @return The percentage of outliers of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_outlier_percentage(ts = datasets::BJsales)
#' @export
calculate_outlier_percentage <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    # All tests with significance level 5%
    z_test <- sum(scores(ts, type="z", prob = 0.95))
    chisq_test <- sum(scores(ts, type="chisq", prob = 0.95))
    t_test <- sum(scores(ts, type="t", prob = 0.95))
    # Get the mean over the 3 test results
    outlier_mean <- mean(c(z_test, chisq_test, t_test))
    # Return the outlier percentage of the series
    if (length(outlier_mean) == 0){
      return(0)
    } else {
      return(outlier_mean / calculate_observationnumber(ts))
    }
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the percentage of step changes of a ts object.
#'
#' This is a function to generate the percentage of step changes of
#' a time series object. As input is only required an object from the class
#' time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The percentage of step changes of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_stepchange_percentage(ts = datasets::BJsales)
#' @export
calculate_stepchange_percentage <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    number_of_stepchanges = 0
    for(i in 5:calculate_observationnumber(ts)){
      if (abs(as.numeric(ts)[i] - mean(as.numeric(ts)[1:(i - 1)])) >
          2 * sd(as.numeric(ts)[1:(i - 1)])) {
        number_of_stepchanges <- number_of_stepchanges + 1
      }
    }
    if (length(number_of_stepchanges) == 0){
      return(0)
    } else {
      return(number_of_stepchanges / calculate_observationnumber(ts))
    }

  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the percentage of peaks of a ts object.
#'
#' This function generates the percentage of peaks of a time series object.
#' As input is only required an object from the class time series.
#' Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The percentage of peaks of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_peak_percentage(ts = datasets::BJsales)
#' @export
calculate_peak_percentage <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
    percentage_peaks <- nrow(findpeaks(as.numeric(ts), minpeakheight = 0.6)) /
      calculate_observationnumber(ts)
    if (length(percentage_peaks) == 0){
      return(0)
    } else {
      return(percentage_peaks)
    }
  } else {
    stop("A time series object is required as input!")
  }

}

#' Generates the autocorrelation measure of a data.frame object.
#'
#' This function generates the autocorrelation measure of a data.frame object.
#' It is based on the durbin watson test. The test is based on detrended data.
#' The data.frame object should represent an multivariate ts. As input is
#' only required an object from the class data.frame.
#' Otherwise the function returns an error message.
#'
#' @param df A data.frame object.
#' @param targetcol A character containing a column name of the \code{df}.
#' @return The autocorrelation measure of \code{df}.
#' If the input is not a data.frame, an error message is returned.
#' @examples
#' calculate_determination_coefficient(
#' df = multi_ts_list$`M-TS-1`$data,
#' targetcol = colnames(multi_ts_list$`M-TS-1`$data)[1])
#' @export
calculate_durbin_watson_test <- function(df, targetcol){
  # If else clause to check the input
  if (is.data.frame(df)) {
    dw <- 0
    df_colnames <- colnames(df)
    df_colnames <- df_colnames[which(df_colnames !=
                                       targetcol & df_colnames != "date")]
    # Transform the df into detrended data
    transform_df <- df
    for(col in df_colnames){
      freq = get_ts_frequency(as.numeric(unlist(transform_df[col])))
      decomp <- decompose_ts(ts(as.numeric(unlist(transform_df[col])),
                                f = freq))
      deTREND <- decomp[["ts"]] - decomp[["trend"]]
      transform_df[col] <- deTREND
    }
    # Calculate DW on detrended df
    dw_colnames = df_colnames[which(df_colnames != targetcol)]
    for(col in dw_colnames){
      dw_temp <- durbinWatsonTest(
        lm(as.numeric(unlist(transform_df[targetcol])) ~
             as.numeric(unlist(transform_df[col])), data = faithful))
      dw <- dw + dw_temp$r
    }
    return(dw / length(dw_colnames))
  } else {
    stop("A data.frame object is required as input!")
  }

}

#' Generates the percentage of the values in the 4 quartiles of a ts object.
#'
#' This is a function to generate the percentage of the values in the 4
#' quartiles of a time series object. As input is only required an object from
#' the class time series. Otherwise the function returns an error message.
#'
#' @param ts A time series object.
#' @return The percentage of the values in the 4 quartiles of \code{ts}.
#' If the input is not a ts, an error message is returned.
#' @examples
#' calculate_quartile_distribution(ts = datasets::BJsales)
#' @export
calculate_quartile_distribution <- function(ts){
  # If else clause to check the input
  if (is.ts(ts)) {
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
    stop("A time series object is required as input!")
  }

}

#' Generates the coefficient of determination (R2) of a data.frame object.
#'
#' This is a function to generate the R2 of a data.frame object.
#' As input is only required an object from the class data.frame.
#' The data.frame object should represent an multivariate ts.
#' Otherwise the function returns an error message.
#'
#' @param df A data.frame object.
#' @param targetcol A character containing a column name of the \code{df}.
#' @return The R2 of \code{df}.
#' If the input is not a data.frame, an error message is returned.
#' @examples
#' calculate_determination_coefficient(
#' df = multi_ts_list$`M-TS-1`$data,
#' targetcol = colnames(multi_ts_list$`M-TS-1`$data)[1])
#' @export
calculate_determination_coefficient <- function(df, targetcol){
  # If else clause to check the input
  if (is.data.frame(df)) {
    R2 <- 0
    df_colnames <- colnames(df)
    df_colnames <- df_colnames[which(df_colnames !=
                                       targetcol & df_colnames != "date")]
    for(col in df_colnames){
      lm = lm(as.numeric(unlist(df[targetcol])) ~
                as.numeric(unlist(df[col])), data = faithful)
      R2 = R2 + summary(lm)$r.squared
    }
    return(R2)
  } else {
    stop("A data.frame object is required as input!")
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
#' If the input is not a data.frame, an error message is returned.
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
