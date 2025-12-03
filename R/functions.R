#' Title: Function making descriptive table
#'
#' @param data
#'
#' @returns A data.frame
#'
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(
      dplyr::across(
        value,
        list(
          mean = mean,
          sd = sd
        )
      )
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      \(x) round(x, digits = 2)
    )) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD)
}


#' Title: GGplot of histogram distributions of values
#'
#' @param data
#'
#' @returns A plot object
#'
create_plot_distributions <- function(data) {
  ggplot2::ggplot(
    data,
    aes(x = as.numeric(value))
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free") +
    ggplot2::theme_minimal()
}
