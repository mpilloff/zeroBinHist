#' Plot Zero Bin Histogram
#'
#' Generates a ggplot histogram with a zero-centered bin and adjusted axis labels.
#'
#' @param binned_data A data frame created by `bin_data_with_zero`.
#' @param group_col Column name for grouping (fill aesthetic).
#' @param counter Numeric value to control x-axis label spacing.
#' @param y_var Column to use on the y-axis, such as "prop" or "obs_num". Default is "prop" which gives the proportion
#' @param y_format Format for all numeric values on the y-axis (function). Default is percent_format()
#' @param x_format Format for all numeric values on the x-axis (function). Default is numeric_format()
#' @param drop_line Flag to create a dashed drop line at 0 on the x-axis to indicate "No change"
#'
#' @return A `ggplot` object representing the histogram.
#'
#' @examples
#' p <- plot_zero_bin_histogram(binned_data)
#' print(p)
#' @export

plot_zero_bin_histogram <- function(
    binned_data,
    group_col = NULL,
    counter = 0.5,
    y_var = "prop",
    y_format = percent_format(),
    x_format = number_format(),
    drop_line = F) {

  library(ggplot2)
  library(dplyr)
  library(scales)

  bin_width <- unique(binned_data$bin_width)

  # Get axis range from cutoff values
  # Add/subtract half a bin to include the cutoff bins
  cutoff_low <- unique(binned_data$cutoff_low) - .5*bin_width
  cutoff_high <- unique(binned_data$cutoff_high) + .5*bin_width

  # Compute label positions and text
  x_min <- floor(cutoff_low/counter)*counter
  x_max <- ceiling(cutoff_high/counter)*counter

  .labels <- seq(x_min, x_max, by = counter)

  ## Manually change x-axis to move all ticks by 1/2 bin width
  .breaks <- sapply(.labels, function(x) {
    if (x > 0) {
      x + 0.5 * bin_width
    } else if (x == 0) {
      0
    } else {
      x - 0.5 * bin_width
    }
  })

  # Build plot but add in redundancy if grouping column does not exist
  if (!is.null(group_col)) {
    group_sym <- sym(group_col)
    p <- ggplot(binned_data, aes(x = bin, y = !!sym(y_var), fill = !!group_sym, group = !!group_sym))
  } else {
    p <- ggplot(binned_data, aes(x = bin, y = !!sym(y_var)))
  }

  p <- p +
    geom_col(position = "identity", alpha = 0.5, width = bin_width) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(x_min  - .5*bin_width , x_max + .5*bin_width),
      breaks = .breaks,
      labels = x_format(.labels)
    )+
    scale_y_continuous(
      labels = y_format
      , expand = c(0, 0)
    )

  if (drop_line) {
    max_y <- binned_data %>% summarise(max_val = max(!!sym(y_var))) %>% pull(max_val)

    p <- p +
      annotate("text", x = 0, y = max_y, label = "No Change", vjust = 3, hjust = -0.08,
               size = 10 / ggplot2::.pt, color = "black", fontface = "plain") +
      geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "black")
  }

  return(p)
}

