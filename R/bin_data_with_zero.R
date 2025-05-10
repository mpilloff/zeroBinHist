#' Create Dataframe for Binned Data with a Dedicated Zero Bin
#'
#' This function creates bins for continuous values to visualize into a histogram-like format
#' with a dedicated bin for zero. It is an input into visualization functions to display
#' the shape of a continuous variable with a dedicated option for values that are 0.
#'
#' @param df A data frame containing the data to be binned.
#' @param value_col The name of the column with numeric values to bin.
#' @param group_col The name of the columns to group by (e.g., for comparison). This is defaulted to NULL
#' @param bins Number of bins to create (default is 50).
#' @param cutoff_low Optional lower number cutoff (numeric). Default is 5th percentile
#' @param cutoff_high Optional upper number cutoff (numeric). Defailt is 95th percentile
#'
#' @return A binned data frame with columns for the bin, counts, proportions, and each grouping column
#'
#' @examples
#' binned <- bin_data_with_zero(data_frame, value_col = "price", group_col = "group")
#' @export

bin_data_with_zero <- function(
    df,
    value_col = "value",
    group_col = NULL,
    bins = 50,
    cutoff_low = NULL,
    cutoff_high = NULL) {
  
  library(dplyr)
  library(rlang)

  value_sym <- sym(value_col)
  
  df_filtered <- df %>%
    filter(!is.na(!!value_sym))
  
  # Calculate cutoffs
  cutoff_low_val <- coalesce(cutoff_low, quantile(df_filtered[[value_col]], 0.05, na.rm = TRUE))
  cutoff_high_val <- coalesce(cutoff_high, quantile(df_filtered[[value_col]], 0.95, na.rm = TRUE))
  
  # Find bin width
  bin_width <- (cutoff_high_val - cutoff_low_val) / (bins - 1)
  
  # Create bin assignment
  # 0 bin is is value is rounded to 0 at the 6th decimal due to float precision
  df_binned <- df_filtered %>%
    mutate(
      bin = case_when(
        round(!!value_sym, 6) == 0 ~ 0,
        ceiling(!!value_sym / bin_width) <= 0 ~ bin_width * floor(!!value_sym / bin_width),
        TRUE ~ bin_width * ceiling(!!value_sym / bin_width)
      )
    )
  
  # Count values in each bin per group
  df_summary <- df_binned %>%
    group_by(across(all_of(group_col)), bin) %>%
    summarise(
      obs_num = n(),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    group_by(across(all_of(group_col))) %>%
    mutate(
      total = sum(obs_num),
      prop = obs_num / total,
      bin_width = bin_width,
      cutoff_low = cutoff_low_val,
      cutoff_high = cutoff_high_val,
      length = total
    ) %>%
    ungroup() %>% 
    select(all_of(group_col), bin, length, bin_width, cutoff_high, cutoff_low, obs_num, prop)
  
  return(df_summary)
  
}