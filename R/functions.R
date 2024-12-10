#' Descriptive Statistics
#'
#' @param data Input file
#' @param variable Name of the column with the values
#'
#' @return A data.frame/tibble
#'

descriptive_stats <- function(data, group_variable, value_variable) {
    data %>%
        dplyr::group_by({{group_variable}}) %>%
        dplyr::summarise(across(
            {{value_variable}},
            list(
                mean = mean,
                sd = sd
            )
        )) %>%
        dplyr::mutate(across(
            where(is.numeric),
            ~ round(.x, digits = 1)
        ))
}
