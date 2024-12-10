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
        dplyr::summarise(across(
            {{ valuevar }},
            list(
                mean = mean,
                sd = sd
            )
        )) |>
        dplyr::mutate(across(
            where(is.numeric),
            ~ round(.x, 1)
        ))
}
