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
    ggplot2::aes
    (x = value)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite),
      scales = "free"
    )
}


# Change a character columns values to snakecase  -------------------------------------------------------

#' Change character column values to snakecase
#'
#' @param Dataset
#' @param Column to snakecase
#'
#' @return Dataset with altered columns

column_values_to_snake_case <- function(data, columns) {
  data |>
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}

# Dataset from long to wider  -------------------------------------------------------

#' Convert data from long format to wider
#'
#' @param dataset in long format
#'
#' @return dataset in wide format
metabolites_to_wider <- function(data) {
  data |>
    tidyr::pivot_wider(
      names_from = metabolite,
      values_from = value,
      values_fn = mean,
      names_prefix = "metabolite_"
    )
}

# Transformation -------------------------------------------------------

#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return a recipe object
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) |>
    recipes::update_role({{ metabolite_variable }},
      age,
      gender,
      new_role = "predictor"
    ) |>
    recipes::update_role(class,
      new_role = "outcome"
    ) |>
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}

# Model workflow -------------------------------------------------------


#' Create a workflow object of the model and transformations.
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() |>
    workflows::add_model(model_specs) |>
    workflows::add_recipe(recipe_specs)
}

# Tidy output from workflow models -------------------------------------------------------

#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model The model workflow object that has been fitted
#'
#' @return A tidy dataframe
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model |>
    workflows::extract_fit_parsnip() |>
    broom::tidy(exponentiate = TRUE)
}

# Convert long to wide, split by metabolite data frames -------------------------------------------------------

#' Convert the long form dataset into a list of wide form data frames.
#'
#' @param data The lipidomics dataset.
#'
#' @return A list of data frames.
#'
split_by_metabolite <- function(data) {
  data |>
    column_values_to_snake_case(metabolite) |>
    dplyr::group_split(metabolite) |>
    purrr::map(metabolites_to_wider)
}


# Generate model for obtaining results -------------------------------------------------------

#' Generate the results of a model
#'
#' @param data The lipidomics dataset
#'
#' @return A dataframe
generate_model_results <- function(data) {
  create_model_workflow(
    model_specs = parsnip::logistic_reg() |> parsnip::set_engine("glm"),
    recipe_specs = data |> create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) |>
    parsnip::fit(data) |>
    tidy_model_output()
}


# Calculate estimates for each metabolite -------------------------------------------------------

#' Calculate the estimates for the model for each metabolite.
#'
#' @param data The lipidomics dataset
#'
#' @return A dataframe
calculate_estimates <- function(data) {
  model_estimates <- data |>
    split_by_metabolite() |>
    purrr::map(generate_model_results) |>
    purrr::list_rbind() |>
    dplyr::filter(stringr::str_detect(term, "metabolite_"))

  data |>
    dplyr::select(metabolite) |>
    dplyr::mutate(term = metabolite) |>
    column_values_to_snake_case(term) |>
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) |>
    dplyr::distinct(metabolite, term) |>
    dplyr::right_join(model_estimates, by = "term")
}
