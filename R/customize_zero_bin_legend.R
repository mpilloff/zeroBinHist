#' Format Legend Names
#'
#' Applies custom names and color values to a column for use in legend formatting.
#'
#' @param p The histogram created by `plot_zero_bin_histogram`
#' @param binned_data The data used to create p, made by `bin_data_with_zero`.
#' @param group_col A character vector of the grouping variables for the legend.
#' @param legend_labels A character vector of new names for the legend. # Named vector: c("old_name1" = "New Label 1", ...)
#' @param fill_colors A vector of colors to map to each group. # Named vector: c("New Label 1" = "#color1", ...)
#'
#' @return A data frame with an updated `name_legend` column and color mapping.
#'
#' @examples
#' df <- format_legend_names(df, c("Real", "Model"), c("Real World", "Model"), c("blue", "orange"))
#' @export

customize_zero_bin_legend <- function(
    p,
    binned_data,
    group_col = "name",
    legend_labels = NULL,
    fill_colors = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(rlang)
  
  group_sym <- sym(group_col)
  
  # Re-map group labels in the data if legend_labels provided
  if (!is.null(legend_labels)) {
    binned_data <- binned_data %>%
      mutate(legend_label = recode(!!group_sym, !!!legend_labels))
  } else {
    binned_data <- binned_data %>%
      mutate(legend_label = !!group_sym)
  }
  
  # Rebuild the plot with new legend labels
  bin_width <- unique(binned_data$bin_width)
  
  p <- p %+% binned_data + 
    aes(fill = legend_label, group = legend_label)
  
  # Apply color scheme if provided
  if (!is.null(fill_colors)) {
    p <- p + scale_fill_manual(values = fill_colors)
  }
  
  return(p)
}
