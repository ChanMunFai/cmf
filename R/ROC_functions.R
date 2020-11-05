#library(tidyverse)
#library(readr)
#library(dplyr)
#library(janitor)


#' Ratio of Counts Function
#'
#' This function allows you to determine the Ratio of Counts(ROC) between 2 datasets, whereby the
#' ratio of counts is an indication of the overlap between 2 datasets.
#' Specifically, it calculates the ROC between 2 datasets for each variable. The overall ROC is the arithmetic mean of all the ROC scores.
#' @param data1 1st dataset
#' @param data2 2nd dataset
#' @param num_cat Number of categories that can be evaluated. Defaults to 2000
#' @param min_items Minimum mumber of items in a category. Defaults to 5. If the number falls below this threshold, this particular variable will be omitted from the calculation of the ROC score.
#' @keywords ROC, Ratio of Counts
#' @export
#' ROC_score
#'@importFrom magrittr %>%

ROC_score <- function (data1, data2, num_cat = 2000, min_items = 5){
  ### Read in 2 dataframes to evaluate the Ratio of Counts per variable
  ### num_cat is the number of categories that can be evaluated
  ### min_items is the minimum number of items in a category, otherwise it will be omitted from calculations
  ROC = 0
  num_col = 0 ## counter for the number of columns to divide ROC by
  for (x in colnames(data1)){
    df_freq <- janitor::tabyl(data1, (x))
    if (nrow(df_freq) <= num_cat){
    df_freq[[x]] <- as.character(df_freq[[x]])
    df_freq[[x]][is.na(df_freq[[x]])] = 0 ## Replaced NA values with 0 - have to ensure that there arent any 0 values in real dataset
    df_freq <- data.frame(df_freq, row.names = df_freq[[x]])
    dfsyn_freq <- janitor::tabyl(data2, (x))
    dfsyn_freq[[x]] <- as.character(dfsyn_freq[[x]])
    dfsyn_freq[[x]][is.na(dfsyn_freq[[x]])] = 0
    dfsyn_freq <- data.frame(dfsyn_freq,row.names = dfsyn_freq[[x]])


    df_combined <- merge(df_freq, dfsyn_freq, by = 0, all = TRUE) ## merge by row name which is every single individual value of a row
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}

      y <- which(df_combined$ratio == 0 & (df_combined$n.x <= min_items & df_combined$n.y <= min_items))
      if (is.integer0(y) == FALSE){next}

      num_col = num_col + 1
    ROC = ROC + as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}}}

  ROC/num_col

}

#' Ratio of Counts (Individual) Function
#'
#' This function allows you to determine the Ratio of Counts(ROC) for a particular variable between 2 datasets, whereby the
#' ratio of counts is an indication of the overlap between 2 datasets.
#' @param data1 1st dataset
#' @param data2 2nd dataset
#' @param x column name that we want to compare ROC on
#' @param num_cat Number of categories that can be evaluated. Defaults to 2000
#' @keywords ROC, Ratio of Counts
#' @export
#' ROC_indiv

ROC_indiv <- function (data1, data2, x,  num_cat = 2000){

  df_freq <- janitor::tabyl(data1, (x))
  if (nrow(df_freq) <= num_cat){
    df_freq[[x]] <- as.character(df_freq[[x]])
    df_freq[[x]][is.na(df_freq[[x]])] = 0 ## Replaced NA values with 0 - have to ensure that there arent any 0 values in real dataset
    df_freq <- data.frame(df_freq, row.names = df_freq[[x]])
    dfsyn_freq <- janitor::tabyl(data2, (x))
    dfsyn_freq[[x]] <- as.character(dfsyn_freq[[x]])
    dfsyn_freq[[x]][is.na(dfsyn_freq[[x]])] = 0
    dfsyn_freq <- data.frame(dfsyn_freq,row.names = dfsyn_freq[[x]])

    df_combined <- merge(df_freq, dfsyn_freq, by = 0, all = TRUE) ## merge by row name which is every single individual value of a row
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}

      ROC = as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}
    print(paste0(x, " : ",  ROC))}
  else {print (paste0(x, " : not used in calculation as number of categories exceed num_cat."))}
}

is.integer0 <- function(x){
  is.integer(x) && length(x) ==0L
}

#' Ratio of Counts Numeric Function
#'
#' This function allows you to determine the Ratio of Counts(ROC) for a particular numeric variable between 2 datasets, whereby the
#' ratio of counts is an indication of the overlap between 2 datasets.
#' This function bins continuous numerical variables into discrete categories before calculating the ROC score.
#' @param data1 1st dataset
#' @param data2 2nd dataset
#' @param x column name that we want to compare ROC on
#' @param y All values in y will be rounded off to the nearest y. Defaults to 2500. Should be adjusted according to the data structure.
#' @param num_cat Number of categories that can be evaluated. Defaults to 2000
#' @keywords ROC, Ratio of Counts
#' @export
#' ROC_numeric

ROC_numeric <- function(data1, data2, x, y = 2500 , num_cat=2000){

  df_freq <- janitor::tabyl(data1, (x))
  df_freq[[x]] <- plyr::round_any(as.numeric(df_freq[[x]]), y)
  df_freq <- data.frame(aggregate(cbind(df_freq$n, df_freq$percent), by = list(df_freq[[x]]), FUN = sum))
  colnames(df_freq) <- c(paste(x), "n", "percent")

  dfsyn_freq <- janitor::tabyl(data2, (x))
  dfsyn_freq[[x]] <- plyr::round_any(as.numeric(dfsyn_freq[[x]]), y)
  dfsyn_freq <- data.frame(aggregate(cbind(dfsyn_freq$n, dfsyn_freq$percent), by = list(dfsyn_freq[[x]]), FUN = sum))
  colnames(dfsyn_freq) <- c(paste(x), "n", "percent")

  if (nrow(df_freq) <= num_cat){
    df_combined <- merge(df_freq, dfsyn_freq, by = 0, all = TRUE) ## merge by row name which is every single individual value of a row
    if(nrow(df_combined) <= num_cat){
      df_combined$min <- pmin(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$max <- pmax(df_combined[c('percent.x')], df_combined[c('percent.y')])
      df_combined$ratio <- df_combined$min / df_combined$max
      df_combined$ratio[is.na(df_combined$ratio)] <- 0
      w <- which(df_combined$n.x ==0 & df_combined$n.y ==0)
      if (is.integer0(w) == FALSE){df_combined <- df_combined[-c(w),]}

      ROC = as.numeric(lapply(df_combined[,ncol(df_combined)], mean))}}
  print(ROC) }


#' Ratio of Counts List Function
#'
#' This function allows you to determine the Ratio of Counts(ROC) for all variables between 2 datasets, whereby the
#' ratio of counts is an indication of the overlap between 2 datasets.
#' @param data1 1st dataset
#' @param data2 2nd dataset
#' @param num_cat Number of categories that can be evaluated. Defaults to 2000
#' @keywords ROC, Ratio of Counts
#' @export
#' ROC_list
ROC_list <- function(data1, data2, num_cat = 10000){
  for (x in colnames(data1)){
    ROC_indiv(data1, data2, x)
  }
}


