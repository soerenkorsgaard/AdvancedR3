
# Descriptive statistics --------------------------------------------------


#' Descriptive statistics - obtain mean and sd of numeric variables by a grouping variable
#'
#' @param data A dataset
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 1)))
}
