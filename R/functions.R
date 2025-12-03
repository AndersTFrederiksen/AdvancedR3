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
    ggplot2::aes(x = value)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_minimal()
}

#' Title Creating a clean format of the lipidomics dataset
#'
#' @param data
#'
#' @returns A clean dataframe
#'
clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}

#' Title: Preprocessing the data
#'
#' @param data
#'
#' @returns A dataframe
#'
Preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}


#' Title: Fit the model
#'
#' @param data
#' @param metabol
#'
#' @returns
#'
regress <- function(data, model, metabol) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial(link = "logit")
  ) |>
    broom::tidy(exponentiate = T) |>
    dplyr::mutate(
      metabolite = unique(metabol),
      model = format(model),
      .before = everything()
    )
}
