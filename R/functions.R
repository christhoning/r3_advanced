#' Descriptive Statistics
#'
#' @param data Input file
#' @param variable Name of the column with the values
#'
#' @return A data.frame/tibble
#'

descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(across(
            value,
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
