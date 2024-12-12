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
#' @return recipe object
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

#' Convert long form dataset into a list of wide form data frames based on metabolites
#'
#' @param data The lipidomics dataset
#'
#' @return list of tibbles
split_by_metabolite <- function(data) {
  data |>
    column_values_to_snake_case(metabolite) |>
    dplyr::group_split(metabolite) |>
    purrr::map(metabolites_to_wider)
}

#' Generate the results of a model
#'
#' @param data The lipidomics dataset
#'
#' @return A data frame
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() |>
      parsnip::set_engine("glm"),
    data |>
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) |>
    parsnip::fit(data) |>
    tidy_model_output()
}

#' Splits the lipidomics dataset by metabolite and estimate OR
#'
#' @param data Input data
#'
#' @return returns a tibble with model estimates for each metabolite
calculate_estimates <- function(data) {
  df_estiamtes <-
    split_by_metabolite(data) |>
    purrr::map(generate_model_results) |>
    purrr::list_rbind() |>
    dplyr::filter(stringr::str_detect(term, "metabolite_"))

  data |>
    dplyr::select(metabolite) |>
    dplyr::mutate(term = metabolite) |>
    column_values_to_snake_case(term) |>
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) |>
    dplyr::distinct(metabolite, term) |>
    dplyr::right_join(df_estiamtes, by = "term")
}
