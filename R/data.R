#' A list of 500 different univariate time series data.
#'
#' A list containing 500 different univariate time series data.
#' Each element of the \code{uni_list} represents a time series by a list
#' of three different attributes: name, data and desc. The name attribute
#' contains the name of the time series, the data attribute is a 2 dimensional
#' data frame consisting of the date and the actual time series values. The
#' desc attribute describes the kind of time series data.
#' The first 323 elements of the list are time series data from the
#' M4 competition for forecasting model evaluation. Also, 66 elements of the
#' list are from the NNGC competition. The left 111 elements are time series
#' from the NN3 competition.
#'
#' @format A list with 500 elements.
#' Each element is a list of the following three items:
#' \describe{
#'   \item{name}{The valid name of the ts from its source}
#'   \item{data}{A data.frame which always contains a 'date' column
#'   and then one further column for the ts data}
#'   \item{desc}{A brief information about the kind of the data}
#' }
#' @source
#' M4 competition \url{https://www.mcompetitions.unic.ac.cy/the-dataset/}
#' NNGC competition \url{http://www.neural-forecasting-competition.com/downloads/NNGC1/datasets/download.htm}
#' NN3 competition \url{http://www.neural-forecasting-competition.com/NN3/datasets.htm}
"uni_ts_list"



#' A list of 500 different multivariate time series data.
#'
#' A list containing 500 different multivariate time series data.
#' Each element of the \code{uni_list} represents a time series by a list
#' of three different attributes: name, data and desc. The name attribute
#' contains the name of the time series, the data attribute is a 2 to n
#' dimensional data frame consisting of the date and several actual time
#' series values/features. The desc attribute describes the kind of ts data.
#' 458 elements of the list are multivariate time series data from the
#' kaggle data science platform. It consist mainly of stock, weather,
#' currency and crime data. The left 42 elemnts are from the University of
#' Florida and the UCI Machine Learning Repository.
#'
#' @format A list with 500 elements.
#' Each element is a list of the following three items:
#' \describe{
#'   \item{name}{The valid name of the ts from its source}
#'   \item{data}{A data.frame which always contains a 'date' column and
#'   then one further column for the ts data}
#'   \item{desc}{A brief information about the kind of the data}
#' }
#' @source
#' Kaggle TS data \url{https://www.kaggle.com/datasets?tagids=6618}
#' University of Florida \url{http://users.stat.ufl.edu/~winner/datasets.html}
#' UCI Machine Learning Repository \url{https://archive.ics.uci.edu/ml/datasets.html?format=&task=reg&att=&area=&numAtt=&numIns=&type=ts&sort=attUp&view=table}
"multi_ts_list"



#' A list of 13 elements, containing time series matrices.
#'
#' A list of 13 elements, containing time series matrices for the dynamic time
#' warping distance calculation. The 1000 time series data from the
#' multi_ts_list.RDS and uni_ts_list.RDS files are divided into 13 blocks by
#' their data type.
#'
#' @format A list of 13 elements, each element is a matrix:
#' \describe{
#'   \item{stock}{A matrix, each row represents multivariate stock
#'   time series data from kaggle}
#'   \item{weather}{A matrix, each row represents multivariate weather
#'   time series data from kaggle}
#'   \item{crime}{A matrix, each row represents multivariate crime
#'   time series data from kaggle}
#'   \item{crypto}{A matrix, each row represents crypto currency
#'   time series data from kaggle}
#'   \item{multi-other}{A matrix, each row represents multivariate
#'   time series data of different kind}
#'   \item{finance}{A matrix, each row represents univariate finance
#'   time series data from the M4 competition}
#'   \item{micro}{A matrix, each row represents univariate micro
#'   time series data from the M4 competition}
#'   \item{demographic}{A matrix, each row represents univariate demographic
#'   time series data from the M4 competition}
#'   \item{macro}{A matrix, each row represents univariate macro
#'   time series data from the M4 competition}
#'   \item{industry}{A matrix, each row represents univariate industry
#'   time series data from the M4 competition}
#'   \item{other}{A matrix, each row represents univariate time series data
#'   of different kind from the M4 competition}
#'   \item{NN3}{A matrix, each row represents univariate time series data
#'   from the NN3 competition}
#'   \item{NNGC}{A matrix, each row represents univariate time series data from
#'   the NNGC competition}
#' }
"matrix_block_list"
