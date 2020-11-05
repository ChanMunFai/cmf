#library(readr)
#library(tidyverse)

#' Correct Attribution Probability Original
#'
#' This function allows you to determine the Correct Attribution Probability (CAP) of a given dataset.
#' @param dataframe dataset
#' @param key_variable key variable
#' @param target_variable target_variable
#' @export
#' CAP_original

CAP_original <- function(dataframe, key_variable, target_variable) {
  ### return the CAP score for an original dataset
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    dplyr::inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    dplyr::inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  dcap_original <- mean(dataframe$DCAP_original)
  print(paste0("df_original mean DCAP = ", dcap_original))
}

#' Correct Attribution Probability Baseline
#'
#' This function allows you to determine the baseline Correct Attribution Probability (CAP) of a given dataset.
#' @param dataframe dataset
#' @export
#' CAP_baseline

CAP_baseline <- function(dataframe, target_variable) {
  ### return the baseline CAP score for an original dataset
  ### generate univariate distribution of target variables ###
  target_freq <- plyr::count(dataframe, target_variable)
  t_freq_total <- sum(target_freq$freq)
  target_freq$prob <- target_freq$freq/t_freq_total
  dcap_univariate <- mean(target_freq$prob)
  print(paste0("df_original baseline CAP = ", dcap_univariate))
}

#' Correct Attribution Probability for Synthetic Dataset
#'
#' This function allows you to determine the baseline Correct Attribution Probability (CAP) of a synthetic dataset, relative to an original dataset.
#' @param dataframe dataset
#' @param synthetic_dataframe synthetic dataset
#' @param key_variable key variable
#' @param target_variable target variable
#' @export
#' CAP_synthetic

CAP_synthetic <- function(dataframe, synthetic_dataframe, key_variable, target_variable){
  ### return the CAP score for a synthetic dataset
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    dplyr::inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    dplyr::inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  # dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  # dcap_original <- mean(dataframe$DCAP_original)

  key_freq_synt <- plyr::count(synthetic_dataframe, c(key_variable))
  colnames(key_freq_synt)[ncol(key_freq_synt)] <- "syn_key_freq"
  denominator <- dplyr::inner_join(key_freq, key_freq_synt)
  denominator$dm_count <- pmin(denominator$k_freq, denominator$syn_key_freq)

  synth_target_key_freq <- plyr::count(synthetic_dataframe, c(key_variable, target_variable))
  colnames(synth_target_key_freq)[ncol(synth_target_key_freq)] <- "synth_tk_freq"
  numerator <- dplyr::inner_join(synth_target_key_freq, target_key_freq)
  numerator$nm_count <- pmin(numerator$tk_freq, numerator$synth_tk_freq)

  df_synthetic <- dplyr::left_join(dataframe, numerator)
  df_synthetic <- dplyr::left_join(df_synthetic, denominator)
  df_synthetic$DCAP_syn <- df_synthetic$nm_count / df_synthetic$dm_count

  dcap_synthetic1 <- mean(df_synthetic$DCAP_syn, na.rm=TRUE)
  print(paste0("df_synthetic mean DCAP when NAs are undefined = ", dcap_synthetic1))

  df_synthetic$DCAP_syn2 <- df_synthetic$DCAP_syn
  df_synthetic$DCAP_syn2[is.na(df_synthetic$DCAP_syn2)] <- 0
  dcap_synthetic2 <- mean(df_synthetic$DCAP_syn2)
  print(paste0("df_synthetic mean DCAP when NAs are 0 = ", dcap_synthetic2))
}

#' Correct Attribution Probability Original (numeric)
#'
#' This function allows you to determine the Correct Attribution Probability (CAP) of a given dataset where the target variable is numeric.
#' @param dataframe dataset
#' @param key_variable key variable
#' @param target_variable target_variable
#' @param y Continuous numerical values of the target variable are rounded to the nearest value of y. Default y = 2500.
#' @export
#' CAP_original_num

CAP_original_num <- function(dataframe, key_variable, target_variable, y = 2500) {
  ### return the CAP score for an original dataset where target_variable is numeric
  ## select y to round the target variable to the nearest y
  dataframe[[target_variable]] <- plyr::round_any(as.numeric(dataframe[[target_variable]]), y)
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    dplyr::inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    dplyr::inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))
  dataframe$DCAP_original <- dataframe$tk_freq/dataframe$k_freq
  dcap_original <- mean(dataframe$DCAP_original)
  print(paste0("df_original mean DCAP = ", dcap_original))
}

#' Correct Attribution Probability for Synthetic Dataset (Numeric)
#'
#' This function allows you to determine the baseline Correct Attribution Probability (CAP) of a synthetic dataset, relative to an original dataset. The target variable here is numeric.
#' @param dataframe dataset
#' @param synthetic_dataframe synthetic dataset
#' @param key_variable key variable
#' @param target_variable target variable
#' @param y Continuous numerical values of the target variable are rounded to the nearest value of y. Default y = 2500.
#' @export
#' CAP_synthetic_num

CAP_synthetic_num <- function(dataframe, synthetic_dataframe, key_variable, target_variable, y = 2500){
  ### return the CAP score for a synthetic dataset
  ## select y to round the target variable to the nearest y
  dataframe[[target_variable]] <- plyr::round_any(as.numeric(dataframe[[target_variable]]), y)
  key_freq <- plyr::count(dataframe, key_variable)
  target_key_freq <- plyr::count(dataframe, c(key_variable, target_variable))
  colnames(target_key_freq)[ncol(target_key_freq)] <- "tk_freq"
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  dataframe %<>%
    dplyr::inner_join(target_key_freq, by = grep("tk_freq", colnames(target_key_freq), value = TRUE, invert = TRUE)) %>%
    dplyr::inner_join(key_freq, by =  grep("k_freq", colnames(key_freq), value = TRUE, invert = TRUE))

  synthetic_dataframe[[target_variable]] <- plyr::round_any(as.numeric(synthetic_dataframe[[target_variable]]), y)
  key_freq_synt <- plyr::count(synthetic_dataframe, c(key_variable))
  colnames(key_freq_synt)[ncol(key_freq_synt)] <- "syn_key_freq"
  denominator <- dplyr::inner_join(key_freq, key_freq_synt)
  denominator$dm_count <- pmin(denominator$k_freq, denominator$syn_key_freq)

  synth_target_key_freq <- plyr::count(synthetic_dataframe, c(key_variable, target_variable))
  colnames(synth_target_key_freq)[ncol(synth_target_key_freq)] <- "synth_tk_freq"
  numerator <- dplyr::inner_join(synth_target_key_freq, target_key_freq)
  numerator$nm_count <- pmin(numerator$tk_freq, numerator$synth_tk_freq)

  df_synthetic <- dplyr::left_join(dataframe, numerator)
  df_synthetic <- dplyr::left_join(df_synthetic, denominator)
  df_synthetic$DCAP_syn <- df_synthetic$nm_count / df_synthetic$dm_count

  dcap_synthetic1 <- mean(df_synthetic$DCAP_syn, na.rm=TRUE)
  print(paste0("df_synthetic mean DCAP when NAs are undefined = ", dcap_synthetic1))

  df_synthetic$DCAP_syn2 <- df_synthetic$DCAP_syn
  df_synthetic$DCAP_syn2[is.na(df_synthetic$DCAP_syn2)] <- 0
  dcap_synthetic2 <- mean(df_synthetic$DCAP_syn2)
  print(paste0("df_synthetic mean DCAP when NAs are 0 = ", dcap_synthetic2))
}

#' Correct Attribution Probability Baseline(Numeric)
#'
#' This function allows you to determine the baseline Correct Attribution Probability (CAP) of a given dataset.
#' @param dataframe dataset
#' @param y Continuous numerical values of the target variable are rounded to the nearest value of y. Default y = 2500.
#' @export
#' CAP_baseline_num

CAP_baseline_num <- function(dataframe, target_variable, y = 2500) {
  ### return the baseline CAP score for an original dataset
  ### generate univariate distribution of target variables ###
  dataframe[[target_variable]] <- plyr::round_any(as.numeric(dataframe[[target_variable]]), y)
  target_freq <- plyr::count(dataframe, target_variable)
  t_freq_total <- sum(target_freq$freq)
  target_freq$prob <- target_freq$freq/t_freq_total
  dcap_univariate <- mean(target_freq$prob)
  print(paste0("df_original baseline CAP = ", dcap_univariate))
}
