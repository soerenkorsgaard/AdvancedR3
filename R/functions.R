# Descriptive statistics --------------------------------------------------


#' Descriptive statistics - obtain mean and sd of numeric variables by a grouping variable
#'
#' @param data A dataset
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd, median = median, iqr = IQR))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 1)))
}



# Distribution plot -------------------------------------------------------

#' Plot for distribution of metabolites
#'
#' @param data Lipidomics dataset
#'
#' @return ggplot object, histogram of metabolite distribution
plot_distribution <- function(data) {
  ggplot2::ggplot(
    data,
    aes
    (x = value)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite),
      scales = "free"
    )
}
