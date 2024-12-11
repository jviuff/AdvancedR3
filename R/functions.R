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
metabolite_distribution_plot  <- function(data, group){
    ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars({{group}}), scales = "free")
}
