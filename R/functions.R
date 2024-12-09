#' Finds mean and standard deviation of value by group
#'
#' @param data Input data set
#' @param groupvar Variable for grouping
#' @param valuevar Variable for mean and SD calculation
#'
#' @return A data.frame/tibble.
descriptive_stats <- function(data, groupvar, valuevar) {
  data |>
    dplyr::group_by({{ groupvar }}) |>
    dplyr::summarise(dplyr::across(
      {{ valuevar }},
      list(
        mean = mean,
        sd = sd
      )
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      ~ round(.x, 1)
    ))
}
#' Function for creating histograms by group
#'
#' @param data Input data
#' @param group Facets plot by this group
#'
#' @return Returns a ggplot histogram
plot_distribution <- function(data, group) {
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars({{ group }}), scales = "free")
}

#' Convert coloumns character values to snake case
#'
#' @param data Input data
#' @param column The column you want to convert to snake case
#'
#' @return A data frame
column_values_to_snake_case <- function(data, column) {
  data |>
    dplyr::mutate(dplyr::across({{ column }}, snakecase::to_snake_case))
}

#' Convert the metabolite long format into a wider one.
#'
#' @param data The lipidomics dataset.
#'
#' @return A wide data frame.
metabolites_to_wider <- function(data) {
  data |>
    tidyr::pivot_wider(
      names_from = metabolite,
      values_from = value,
      values_fn = mean,
      names_prefix = "metabolite_"
    )
}

#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#'
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) |>
    recipes::update_role({{ metabolite_variable }}, age, gender, new_role = "predictor") |>
    recipes::update_role(class, new_role = "outcome") |>
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}

#' Create a workflow object for the model and transformation
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

#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model The model workflow that has been fitted
#'
#' @return Returns a tibble
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model |>
    workflows::extract_fit_parsnip() |>
    broom::tidy(exponentiate = T)
}
