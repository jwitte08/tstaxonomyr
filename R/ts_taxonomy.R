
#' Classifies a time series based on the defined ts taxonomy features.
#'
#' This is a function to calculate the different (statistical) features
#' from the defined time series taxonomy. The taxonomy can be applied to
#' classify univariate and multivariate time series based on it.
#' As input for \code{ts} is only required an object from the classes
#' time series, data.frame or vector. Each of them should represent an
#' time series. Otherwise the function returns an error message. Also, for
#' \code{na_option} is only required the string 'mean', 'median' or 'random'
#' allowed. This means, that all na values are either replaced by the mean,
#' median or a random number between the min and max of the ts.
#' The standard value of \code{na_option} is 'random'. The following 24
#' features: "number_of_observations", "number_of_attributes",
#' "coefficient of determination", "durbin watson test", "mean", "periodicity",
#' "chaos", "entropy", "selfsimilarity", "dynamic time warping (DTW) distance ",
#' "percentage of turning points", "variance", "percentage of outliers",
#' "percentage of step changes", "quartile distribition", "standard deviation",
#' "percentage of peaks", "trend", "seasonality", "autocorrelation",
#' "partial autocorrelation", "skewness", "kurtosis", "non linearity"
#' are calculated, scaled and then assigned to the taxonomy values.
#'
#'
#' @param ts Either a vector, time series or data.frame object representing
#' a time series is allowed.
#' @param na_option A string value containing either 'mean'
#' or'kalman'; Standard values is 'kalman'.
#' @return The final list, containing all scaled feature factor values.
#' If the input \code{ts} is not a vector, ts or data.frame object
#' an error message is returned.
#' @examples
#' ts_vector = c(1,4,6,1,24,5,1)
#' classify_ts(ts = ts_vector_object)
#' classify_ts(ts = ts_vector_object, na_option = "kalman")
#' classify_ts(ts = dataframe_object, na_option = "mean")
#' classify_ts(ts = datasets::BJsales, na_option = "kalman")
#' @export
classify_ts <- function(ts, na_option = "kalman"){

  # Check input data and transform it into ts object ---------------------------

  list_ts <- list()
  # If else clause to check the 'ts' input parameter
  if (is.ts(ts)) {
  # If ts is a ts object, add it to the list_ts list
    list_ts[["ts"]] <- ts
  } else if (is.vector(ts)) {
  # If ts is a vector object, transfrom it to ts & add it to the list_ts list
    # character and factor cols are transformed to numeric cols
    if (is.character(ts) || is.factor(ts)) {
      ts = as.factor(ts)
      ts = as.numeric(ts)
    }
    # Find out the frequency of the data
    freq <- get_ts_frequency(ts)
    # Transform ts into a ts
    ts <- ts(ts, f = freq)
    list_ts[["ts"]] <- ts
  } else if (is.data.frame(ts)) {
  # If ts is a data.frane object, transfrom each data col to a ts object
  # and add each ts object to the list_ts list
    if (ncol(ts) > 1){
      for (col in 1:ncol(ts)) {
        # The date col is cutted out
        if ((!is.Date(ts[,col]) || !is.POSIXt(ts[,col])) &
            length(unique(ts[,col])) > 1 & !str_detect(colnames(ts)[col],
                                        regex("date", ignore_case = TRUE))) {
          # character and factor cols are transformed to numeric cols
          if (is.character(ts[,col]) || is.factor(ts[,col])) {
            ts[,col] = as.factor(ts[,col])
            ts[,col] = as.numeric(ts[,col])
          }
          # Find out the frequency of the data
          freq <- get_ts_frequency(ts[,col])
          # Transform tsinto a ts object
          transform_ts <- ts(ts[,col], f = freq)
          list_ts[[as.character(col)]] = transform_ts
        }
      }
    } else {
      # Find out the frequency of the data
      freq = get_ts_frequency(ts)
      # Transform ts into a time series
      ts <- ts(ts, f = freq)
      list_ts[["ts"]] <- ts
    }
  } else {
    # if the input data has a wrong class, print a error message
    stop("ERROR: input parameter 'ts' has to be of class
              vector, ts or data.frame")
  }

  # If else clause to check the 'na_option' input parameter
    if (na_option == "mean") {
      count_list_ts <- 1
      for (elem in list_ts) {
        if (length(na.omit(as.numeric(elem))) != length(elem)) {
          # Replace all NA by mean
          adjusted_ts <- na.mean(elem, option = "mean")
          list_ts[[names(list_ts)[count_list_ts]]] <- adjusted_ts
          count_list_ts <- count_list_ts + 1
        }
      }
    } else if (na_option == "kalman") {
      count_list_ts <- 1
      for(elem in list_ts){
        if (length(na.omit(as.numeric(elem))) != (length(elem))) {
          if ((length(elem) - length(na.omit(as.numeric(elem)))) >= 3){
            # Replace all NA by kalman
            adjusted_ts <- na.kalman(elem)
            list_ts[[names(list_ts)[count_list_ts]]] <- adjusted_ts
            count_list_ts <- count_list_ts + 1
          } else {
            print("For kalman imputation are at least 3 NA observations
                  + required, thus the simple 'mean' imputation is used")
            # Replace all NA by mean
            adjusted_ts <- na.mean(elem, option = "mean")
            list_ts[[names(list_ts)[count_list_ts]]] <- adjusted_ts
            count_list_ts <- count_list_ts + 1
          }
        }
      }
    } else {
      stop("ERROR: input parameter 'na_option' has to contain either
           'mean' or 'kalman'")
    }

  # Generate the feature values and scale [0,1] them ---------------------------

  # Vector to store the feature results
  ts_vector <- c()

  # Feature NumberOfObservations
  number_of_observations <-
    calculate_observationnumber(list_ts[[names(list_ts)[1]]])
  # Scaling
  #number_of_observations <- scale_feature(number_of_observations,a,b,type)
  # Add scaled result to the ts_vector
  ts_vector <- append(ts_vector, number_of_observations)

  # Feature NumberOfAttributes
  if (is.data.frame(ts)) {
    number_of_attributes = calculate_attributenumber(as.data.frame(list_ts))
  } else {
    number_of_attributes <- 1
  }
  # Add result to vector
  ts_vector <- append(ts_vector, number_of_attributes)

  # Feature R2 fits only for multivaritae ts, else the value is -1
  if (number_of_attributes > 1) {
    ts_colnames <- colnames(as.data.frame(list_ts))
    r2 <-
      calculate_determination_coefficient(df = as.data.frame(list_ts),
                                          targetcol = ts_colnames[1])
  } else {
    r2 <- -1
  }
  # Add result to the ts_vector
  ts_vector <- append(ts_vector, r2)

  # DurbinWatsonTest fits only for multi ts, else the value -1
  if (number_of_attributes > 1) {
    ts_colnames <- colnames(as.data.frame(list_ts))
    dw = calculate_durbin_watson_test(df = as.data.frame(list_ts),
                                      targetcol = ts_colnames[1])
  } else {
    dw = -1
  }
  # Add result to the ts_vector
  ts_vector <- append(ts_vector, dw)

  # Assigning the feature names to the above generated values
  names(ts_vector) <- c("number_of_observations", "number_of_attributes"
                        , "r2", "dw")


  # Generate a feature matrix to store the results for the following for loop
  temp_taxonomy <- matrix(NA, nrow = length(list_ts), ncol = 35)
  colnames(temp_taxonomy) <- c("feature_mean", "feature_periodicity",
                               "feature_chaos", "feature_entropy",
                               "feature_selfsimilarity", "feature_dtw1",
                               "feature_dtw2", "feature_dtw3", "feature_dtw4",
                               "feature_dtw5", "feature_dtw6", "feature_dtw7",
                               "feature_dtw8", "feature_dtw9", "feature_dtw10",
                               "feature_dtw11", "feature_dtw12",
                               "feature_dtw13", "feature_tp",
                               "feature_variance",
                               "feature_outlier", "feature_stepchange",
                               "QUARTILE1", "QUARTILE2", "QUARTILE3",
                               "QUARTILE4", "feature_sd", "feature_peak",
                               "feature_trend", "feature_season",
                               "feature_autocor", "feature_partial_autocor",
                               "feature_skewness", "feature_kurtosis",
                               "feature_non_linearity")
  # Count for the following for-loop
  ts_count <- 1

  # For each ts object in the list_ts list generate all following features
  for(elem in list_ts){

    # A vector that contains all feature values within the for loop
    temp_vector <- c()

    # The following features are generated on raw data -----------------------

    # Feature mean
    feature_mean <- calculate_mean(elem)
    # Scaling
    #feature_mean <- scale_feature(feature_mean,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_mean)

    # Feature periodicity
    feature_periodicity <- calculate_periodicity(elem)
    # Scaling
    #feature_periodicity <- scale_feature(feature_periodicity,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_periodicity)

    # Feature chaos
    feature_chaos <- calculate_chaos(elem)
    # Scaling
    #feature_chaos <- scale_feature(feature_chaos,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_chaos)

    # Feature entropy
    feature_entropy <- calculate_entropy(elem)
    # Scaling
    #feature_entropy <- scale_feature(feature_entropy,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_entropy)

    # Feature self-similarity
    feature_selfsimilarity <- calculate_selfsimilarity(elem)
    # Scaling
    #feature_selfsimilarity <- scale_feature(feature_selfsimilarity,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_selfsimilarity)

    # Feature dtw
    feature_dtw <- calculate_dtw_blockdistance(elem)
    # Scaling
    #tmp_feature_dtw <- c()
    #for(d in feature_dtw){
    #  tmpfeature_dtw <- append(tmp_feature_dtw,scale_feature(d,a,b,type))
    #}
    #feature_dtw <- tmp_feature_dtw
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_dtw)

    # Feature turning points
    feature_tp <- calculate_turningpoint_percentage(elem)
    # Scaling
    #feature_tp <- scale_feature(feature_tp,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_tp)

    # Feature variance
    feature_variance <- calculate_variance(elem)
    # Scaling
    #feature_variance <- scale_feature(feature_variance,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_variance)

    # Feature outliers
    feature_outlier <- calculate_outlier_percentage(elem)
    # Scaling
    #feature_outlier <- scale_feature(feature_outlier,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_outlier)

    # Feature step changes
    feature_stepchange <- calculate_stepchange_percentage(elem)
    # Scaling
    # feature_stepchange <- scale_feature(feature_stepchange,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_stepchange)

    # Feature quartile distribution
    feature_quartiles <- calculate_quartile_distribution(elem)
    # Scaling
    #tmp_quartiles <- c()
    #for(q in feature_quartiles){
    #  tmp_quartiles <- append(tmp_quartiles, scale_feature(q,a,b,type))
    #}
    # feature_quartiles <- tmp_quartiles
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_quartiles)

    # Decompose the raw data for the following features -----------------------
    decomp_ts <- decompose_ts(elem)
    detrended_and_deseason_ts <- decomp_ts[["remainder"]]
    detrended_ts <- decomp_ts[["ts"]] - decomp_ts[["trend"]]

    # The next features are generated on detrended data -----------------------

    # Feature sd
    feature_sd <- calculate_sd(detrended_ts)
    # Scaling
    #feature_sd <- scale_feature(feature_sd,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_sd)

    # Feature peaks
    feature_peak <- calculate_peak_percentage(detrended_ts)
    # Scaling
    #feature_peak <- scale_feature(feature_peak,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_peak)

    # The next features are generated on detrended_and_deseason_ts data
    # but they require a ts object and transfrom it within the fucntion ------

    # Feature trend
    feature_trend <- calculate_trend(elem)
    # Scaling
    #feature_trend <- scale_feature(feature_trend,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_trend)

    # Feature seasonality
    feature_season <- calculate_seasonality(elem)
    # Scaling
    #feature_season <- scale_feature(feature_season,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_season)

    ### Autocor, PartialAutocor, skew, kurto, non-linearity on all 3!

    # The next features are generated on detrended_and_deseason_ts,
    # detrended and deseasonalized data ------------------------------

    decomp_ts_list <- list(detrended_and_deseason_ts,
                           detrended_ts, decomp_ts[["ts"]])

    # Feature autocorrelation
    feature_autocor <- 0
    for(data in decomp_ts_list){
      feature_autocor <- feature_autocor + calculate_autocorrelation(data)
    }
    # Scaling
    #feature_autocor <- scale_feature(feature_autocor/3,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_autocor)

    # Feature partial autocorrelation
    feature_partial_autocor <- 0
    for(data in decomp_ts_list){
      feature_partial_autocor <- feature_partial_autocor +
                                  calculate_partial_autocorrelation(data)
    }
    # Scaling
    #feature_partial_autocor <- scale_feature(feature_partial_autocor/3,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_partial_autocor)

    # Feature skewness
    feature_skewness <- 0
    for(data in decomp_ts_list){
      feature_skewness <- feature_skewness + calculate_skewness(data)
    }
    # Scaling
    #feature_skewness <- scale_feature(feature_skewness/3,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_skewness)

    # Feature kurtosis
    feature_kurtosis <- 0
    for(data in decomp_ts_list){
      feature_kurtosis <- feature_kurtosis + calculate_kurtosis(data)
    }
    # Scaling
    #feature_kurtosis = scale_feature(feature_kurtosis/3,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_kurtosis)

    # Feature non linearity
    feature_non_linearity <- 0
    for(data in decomp_ts_list){
      feature_non_linearity <- feature_non_linearity +
                                  calculate_non_linearity(data)
    }
    # Scaling
    #feature_non_linearity <- scale_feature(feature_non_linearity/3,a,b,type)
    # Add scaled result to the temp_vector
    temp_vector <- append(temp_vector, feature_non_linearity)

    # Add the feature vector to the appropriate row of the temp_taxonomy matrix
    temp_taxonomy[ts_count,] <- temp_vector
    ts_count <- ts_count + 1
  }

  # Collect and combine all feature results -----------------------

  # Generate taxonomy vector for the sum of the temp_taxonomy matrix values
  ts_taxonomy <- c()
  for(i in 1:ncol(temp_taxonomy)){
    ts_taxonomy <- append(ts_taxonomy, mean(temp_taxonomy[,i], na.rm = TRUE))
  }
  names(ts_taxonomy) <- colnames(temp_taxonomy)

  # Add additional features from above the for loop from the ts_vector
  ts_taxonomy <- append(ts_taxonomy, ts_vector)

  # Based on the defined ts taxonomy intervals
  # assign to the feature values from ts_taxonomy factor values --------------

  # final list with all taxonomy values (factor values)
  ts_taxonomy_list <- list()

  # Feature skewness
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_skewness"]] > 0.8) {
    ts_taxonomy_list[["Skewness"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_skewness"]]  > 0.6) {
    ts_taxonomy_list[["Skewness"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_skewness"]]  > 0.4) {
    ts_taxonomy_list[["Skewness"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_skewness"]]  > 0.2) {
    ts_taxonomy_list[["Skewness"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Skewness"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature kurtosis
  factor_levels = c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_kurtosis"]] > 0.8) {
    ts_taxonomy_list[["Kurtosis"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_kurtosis"]]  > 0.6) {
    ts_taxonomy_list[["Kurtosis"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_kurtosis"]]  > 0.4) {
    ts_taxonomy_list[["Kurtosis"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_kurtosis"]]  > 0.2) {
    ts_taxonomy_list[["Kurtosis"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Kurtosis"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature trend
  factor_levels = c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_trend"]] > 0.8) {
    ts_taxonomy_list[["Trend"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_trend"]]  > 0.6) {
    ts_taxonomy_list[["Trend"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_trend"]]  > 0.4) {
    ts_taxonomy_list[["Trend"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_trend"]]  > 0.2) {
    ts_taxonomy_list[["Trend"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Trend"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature autocorrelation
  factor_levels = c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_autocor"]] > 0.8) {
    ts_taxonomy_list[["Autocorrelation"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_autocor"]]  > 0.6) {
    ts_taxonomy_list[["Autocorrelation"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_autocor"]]  > 0.4) {
    ts_taxonomy_list[["Autocorrelation"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_autocor"]]  > 0.2) {
    ts_taxonomy_list[["Autocorrelation"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Autocorrelation"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature mean
  factor_levels <- c("very low", "low", "medium", "high","very high")
  if (ts_taxonomy[["feature_mean"]] > 0.8) {
    ts_taxonomy_list[["Mean"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_mean"]]  > 0.6) {
    ts_taxonomy_list[["Mean"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_mean"]]  > 0.4) {
    ts_taxonomy_list[["Mean"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_mean"]]  > 0.2) {
    ts_taxonomy_list[["Mean"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Mean"]] <-
      factor("very low", levels = factor_levels)
  }

  #Feature sd
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_sd"]] > 0.8) {
    ts_taxonomy_list[["StandardDeviation"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_sd"]]  > 0.6){
    ts_taxonomy_list[["StandardDeviation"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_sd"]]  > 0.4){
    ts_taxonomy_list[["StandardDeviation"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_sd"]]  > 0.2){
    ts_taxonomy_list[["StandardDeviation"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["StandardDeviation"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature number of observations
  factor_levels <- c("very short", "short", "medium", "long", "very long")
  if (ts_taxonomy[["number_of_observations"]] > 0.8) {
    ts_taxonomy_list[["NumberOfObservations"]] <-
      factor("very long", levels = factor_levels)
  } else if (ts_taxonomy[["number_of_observations"]]  > 0.6) {
    ts_taxonomy_list[["NumberOfObservations"]] <-
      factor("long", levels = factor_levels)
  } else if (ts_taxonomy[["number_of_observations"]]  > 0.4) {
    ts_taxonomy_list[["NumberOfObservations"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["number_of_observations"]]  > 0.2) {
    ts_taxonomy_list[["NumberOfObservations"]] <-
      factor("short", levels = factor_levels)
  } else {
    ts_taxonomy_list[["NumberOfObservations"]] <-
      factor("very short", levels = factor_levels)
  }

  #Feature non linearity
  factor_levels <- c("very low", "low", "medium", "high","very high")
  if (ts_taxonomy[["feature_non_linearity"]] > 0.8) {
    ts_taxonomy_list[["NonLinearity"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_non_linearity"]]  > 0.6) {
    ts_taxonomy_list[["NonLinearity"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_non_linearity"]]  > 0.4) {
    ts_taxonomy_list[["NonLinearity"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_non_linearity"]]  > 0.2) {
    ts_taxonomy_list[["NonLinearity"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["NonLinearity"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature seasonality
  factor_levels <- c("none seasonality","very low", "low", "medium",
                     "high", "very high")
  if (ts_taxonomy[["feature_season"]] > 0.8) {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_season"]]  > 0.6) {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_season"]]  > 0.4) {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("medium", levels= factor_levels)
  } else if (ts_taxonomy[["feature_season"]]  > 0.2) {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("low", levels = factor_levels)
  } else if (ts_taxonomy[["feature_season"]] == 0) {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("none seasonality", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Seasonality"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature periodicity
  factor_levels <- c("none periodicity","very low", "low", "medium",
                     "high", "very high")
  if (ts_taxonomy[["feature_periodicity"]] > 0.8) {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_periodicity"]]  > 0.6) {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_periodicity"]]  > 0.4) {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_periodicity"]]  > 0.2) {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("low", levels = factor_levels)
  } else if (ts_taxonomy[["feature_periodicity"]] == 0) {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("none seasonality", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Periodicity"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature chaos
  factor_levels <- c("very low", "low", "medium", "high","very high")
  if (ts_taxonomy[["feature_chaos"]] > 0.8) {
    ts_taxonomy_list[["Chaos"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_chaos"]]  > 0.6) {
    ts_taxonomy_list[["Chaos"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_chaos"]]  > 0.4) {
    ts_taxonomy_list[["Chaos"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_chaos"]]  > 0.2) {
    ts_taxonomy_list[["Chaos"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Chaos"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature entropy
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_entropy"]] > 0.8) {
    ts_taxonomy_list[["Entropy"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_entropy"]]  > 0.6) {
    ts_taxonomy_list[["Entropy"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_entropy"]]  > 0.4) {
    ts_taxonomy_list[["Entropy"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_entropy"]]  > 0.2) {
    ts_taxonomy_list[["Entropy"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Entropy"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature selfsimilarity
  factor_levels <- c("very low", "low", "medium", "high","very high")
  if (ts_taxonomy[["feature_selfsimilarity"]] > 0.8) {
    ts_taxonomy_list[["SelfSimilarity"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_selfsimilarity"]]  > 0.6) {
    ts_taxonomy_list[["SelfSimilarity"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_selfsimilarity"]]  > 0.4) {
    ts_taxonomy_list[["SelfSimilarity"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_selfsimilarity"]]  > 0.2) {
    ts_taxonomy_list[["SelfSimilarity"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["SelfSimilarity"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature dtw distance
  feature_dtwvector <- c()
  factor_levels <- c("Block1 high", "Block1 medium", "Block1 low",
                     "Block2 high", "Block2 medium", "Block2 low",
                     "Block3 high", "Block3 medium", "Block3 low",
                     "Block4 high", "Block4 medium", "Block4 low",
                     "Block5 high", "Block5 medium", "Block5 low",
                     "Block6 high", "Block6 medium", "Block6 low",
                     "Block7 high", "Block7 medium", "Block7 low",
                     "Block8 high", "Block8 medium", "Block8 low",
                     "Block9 high", "Block9 medium", "Block9 low",
                     "Block10 high", "Block10 medium", "Block10 low",
                     "Block11 high", "Block11 medium", "Block11 low",
                     "Block12 high", "Block12 medium", "Block12 low",
                     "Block13 high", "Block13 medium", "Block13 low")
  if (ts_taxonomy[["feature_dtw1"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block1 high")
  } else if (ts_taxonomy[["feature_dtw1"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block1 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block1 low")
  }
  if(ts_taxonomy[["feature_dtw2"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block2 high")
  } else if (ts_taxonomy[["feature_dtw2"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block2 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block2 low")
  }
  if(ts_taxonomy[["feature_dtw3"]] > 0.66){
    feature_dtwvector <- append(feature_dtwvector, "Block3 high")
  } else if (ts_taxonomy[["feature_dtw3"]]  > 0.33){
    feature_dtwvector <- append(feature_dtwvector, "Block3 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block3 low")
  }
  if (ts_taxonomy[["feature_dtw4"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block4 high")
  } else if (ts_taxonomy[["feature_dtw4"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block4 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block4 low")
  }
  if (ts_taxonomy[["feature_dtw5"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block5 high")
  } else if (ts_taxonomy[["feature_dtw5"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block5 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block5 low")
  }
  if (ts_taxonomy[["feature_dtw6"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block6 high")
  } else if (ts_taxonomy[["feature_dtw6"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block6 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block6 low")
  }
  if (ts_taxonomy[["feature_dtw7"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block7 high")
  } else if (ts_taxonomy[["feature_dtw7"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block7 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block7 low")
  }
  if (ts_taxonomy[["feature_dtw8"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block8 high")
  } else if (ts_taxonomy[["feature_dtw8"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block8 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block8 low")
  }
  if (ts_taxonomy[["feature_dtw9"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block9 high")
  } else if (ts_taxonomy[["feature_dtw9"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block9 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block9 low")
  }
  if (ts_taxonomy[["feature_dtw10"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block10 high")
  } else if (ts_taxonomy[["feature_dtw10"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block10 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block10 low")
  }
  if (ts_taxonomy[["feature_dtw11"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block11 high")
  } else if (ts_taxonomy[["feature_dtw10"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block11 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block11 low")
  }
  if (ts_taxonomy[["feature_dtw12"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block12 high")
  } else if (ts_taxonomy[["feature_dtw10"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block12 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block12 low")
  }
  if (ts_taxonomy[["feature_dtw13"]] > 0.66) {
    feature_dtwvector <- append(feature_dtwvector, "Block13 high")
  } else if (ts_taxonomy[["feature_dtw10"]]  > 0.33) {
    feature_dtwvector <- append(feature_dtwvector, "Block13 medium")
  } else {
    feature_dtwvector <- append(feature_dtwvector, "Block13 low")
  }
  # Final feature_dtw factor
  ts_taxonomy_list[["feature_dtwdistance"]] <-
    factor(x = feature_dtwvector, levels = factor_levels)

  # Feature turning points
  factor_levels <- c("very steady", "more steady", "medium",
                     "more oscillating", "very oscillating")
  if (ts_taxonomy[["feature_tp"]] > 0.8) {
    ts_taxonomy_list[["TurningPoints"]] <-
      factor("very oscillating", levels = factor_levels)
  } else if (ts_taxonomy[["feature_tp"]] > 0.6) {
    ts_taxonomy_list[["TurningPoints"]] <-
      factor("more oscillating", levels = factor_levels)
  } else if (ts_taxonomy[["feature_tp"]] > 0.4) {
    ts_taxonomy_list[["TurningPoints"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_tp"]] > 0.2) {
    ts_taxonomy_list[["TurningPoints"]] <-
      factor("more steady", levels = factor_levels)
  } else {
    ts_taxonomy_list[["TurningPoints"]] <-
      factor("very steady", levels = factor_levels)
  }

  # Feature partial autocorrelation
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_partial_autocor"]] > 0.8) {
    ts_taxonomy_list[["PartialAutocorrelation"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_partial_autocor"]]  > 0.6) {
    ts_taxonomy_list[["PartialAutocorrelation"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_partial_autocor"]]  > 0.4) {
    ts_taxonomy_list[["PartialAutocorrelation"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_partial_autocor"]]  > 0.2) {
    ts_taxonomy_list[["PartialAutocorrelation"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["PartialAutocorrelation"]] <-
      factor("very low", levels = factor_levels)
  }

  #Feature variance
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_variance"]] > 0.8) {
    ts_taxonomy_list[["Variance"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_variance"]]  > 0.6) {
    ts_taxonomy_list[["Variance"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_variance"]]  > 0.4) {
    ts_taxonomy_list[["Variance"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_variance"]]  > 0.2) {
    ts_taxonomy_list[["Variance"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Variance"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature outliers
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_outlier"]] > 0.8) {
    ts_taxonomy_list[["Outliers"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_outlier"]]  > 0.6) {
    ts_taxonomy_list[["Outliers"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_outlier"]]  > 0.4) {
    ts_taxonomy_list[["Outliers"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_outlier"]]  > 0.2) {
    ts_taxonomy_list[["Outliers"]] <-
      factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Outliers"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature step changes
  factor_levels <- c("very less abrupt changes", "less abrupt changes",
                     "medium", "many abrupt changes",
                     "great many abrupt changes")
  if (ts_taxonomy[["feature_stepchange"]] > 0.8) {
    ts_taxonomy_list[["StepChanges"]] <-
      factor("great many abrupt changes", levels = factor_levels)
  } else if (ts_taxonomy[["feature_stepchange"]]  > 0.6) {
    ts_taxonomy_list[["StepChanges"]] <-
      factor("many abrupt changes", levels = factor_levels)
  } else if (ts_taxonomy[["feature_stepchange"]]  > 0.4) {
    ts_taxonomy_list[["StepChanges"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_stepchange"]]  > 0.2) {
    ts_taxonomy_list[["StepChanges"]] <-
      factor("less abrupt changes", levels = factor_levels)
  } else {
    ts_taxonomy_list[["StepChanges"]] <-
      factor("very less abrupt changes", levels = factor_levels)
  }

  # Feature peaks
  factor_levels <- c("very low", "low", "medium", "high", "very high")
  if (ts_taxonomy[["feature_peak"]] > 0.8) {
    ts_taxonomy_list[["Peaks"]] <- factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_peak"]]  > 0.6) {
    ts_taxonomy_list[["Peaks"]] <- factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["feature_peak"]]  > 0.4) {
    ts_taxonomy_list[["Peaks"]] <- factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["feature_peak"]]  > 0.2) {
    ts_taxonomy_list[["Peaks"]] <- factor("low", levels = factor_levels)
  } else {
    ts_taxonomy_list[["Peaks"]] <- factor("very low", levels = factor_levels)
  }

  # Feature durbin watson test
  factor_levels <- c("positive autocorrelation", "none autocorrelation",
                     "negative autocorrelation", "none because univariate")
  if(ts_taxonomy[["dw"]] > 2){
    ts_taxonomy_list[["DurbinWatsonTest"]] <-
      factor("negative autocorrelation", levels = factor_levels)
  } else if (ts_taxonomy[["dw"]]  == 2){
    ts_taxonomy_list[["DurbinWatsonTest"]] <-
      factor("none autocorrelation", levels = factor_levels)
  } else if (ts_taxonomy[["dw"]]  == -1){
    ts_taxonomy_list[["DurbinWatsonTest"]] <-
      factor("none because univariate", levels = factor_levels)
  } else {
    ts_taxonomy_list[["DurbinWatsonTest"]] <-
      factor("positive autocorrelation", levels = factor_levels)
  }

  # Feature quartile distribution
  feature_distribution_vector <- c()
  factor_levels <- c("Quartile1 less", "Quartile1 medium", "Quartile1 many",
                     "Quartile2 less", "Quartile2 medium", "Quartile2 many",
                     "Quartile3 less", "Quartile3 medium", "Quartile3 many",
                     "Quartile4 less", "Quartile4 medium", "Quartile4 many")
  if (ts_taxonomy[["QUARTILE1"]] > 0.66) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile1 many")
  } else if (ts_taxonomy[["QUARTILE1"]]  > 0.33) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile1 medium")
  } else {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile1 less")
  }
  if (ts_taxonomy[["QUARTILE2"]] > 0.66) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile2 many")
  } else if (ts_taxonomy[["QUARTILE2"]]  > 0.33) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile2 medium")
  } else {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile2 less")
  }
  if (ts_taxonomy[["QUARTILE3"]] > 0.66) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile3 many")
  } else if (ts_taxonomy[["QUARTILE3"]]  > 0.33) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile3 medium")
  } else {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile3 less")
  }
  if (ts_taxonomy[["QUARTILE4"]] > 0.66) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile4 many")
  } else if (ts_taxonomy[["QUARTILE4"]]  > 0.33) {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile4 medium")
  } else {
    feature_distribution_vector <-
      append(feature_distribution_vector, "Quartile4 less")
  }
  # Final quartile factor
  ts_taxonomy_list[["QuartileDistribution"]] <-
    factor(x = feature_distribution_vector, levels = factor_levels)

  # Feature determination coefficient r2
  factor_levels <- c("none because univariate TS","very low", "low", "medium",
                     "high", "very high")
  if (ts_taxonomy[["r2"]] > 0.8) {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("very high", levels = factor_levels)
  } else if (ts_taxonomy[["r2"]]  > 0.6) {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("high", levels = factor_levels)
  } else if (ts_taxonomy[["r2"]]  > 0.4) {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("medium", levels = factor_levels)
  } else if (ts_taxonomy[["r2"]]  > 0.2) {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("low", levels = factor_levels)
  } else if (ts_taxonomy[["r2"]] == -1) {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("none because univariate TS", levels = factor_levels)
  } else {
    ts_taxonomy_list[["calculate_determination_coefficient"]] <-
      factor("very low", levels = factor_levels)
  }

  # Feature number of attributes
  factor_levels <- c("univariate TS", "multivariate TS")
  if (ts_taxonomy[["number_of_attributes"]] > 1) {
    ts_taxonomy_list[["NumberOfAttributes"]] <-
      factor("multivariate TS", levels = factor_levels)
  } else {
    ts_taxonomy_list[["NumberOfAttributes"]] <-
      factor("univariate TS", levels = factor_levels)
  }

  # Return final list with all feature factors
  return(ts_taxonomy)

}
