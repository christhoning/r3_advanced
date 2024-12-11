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
                sd = sd,
                median = median,
                iqr = IQR
            )
        )) %>%
        dplyr::mutate(across(
            where(is.numeric),
            ~ round(.x, digits = 1)
        ))
}


#' Metabolite Plot
#'
#' @param data
#'
#' @return A plot object
plot_distributions <- function(data) {
    ggplot2::ggplot(
        data,
        ggplot2::aes(x = value)
    ) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
