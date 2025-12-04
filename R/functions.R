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
regress <- function(data, model) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial(link = "logit")
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = everything()
    )
}

#' Title Creates table with model results for cholesterol
#'
#' @param data
#'
#' @returns 2x7 table with model info
#'

#' Title: Fit all models
#'
#' @param data
#'
#' @returns¨A datafrane
#'
fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + age + gender
  ) |>
    purrr::map(\(model) regress(data, model = model)) |>
    purrr::list_rbind()
}


#' Title: Split dataframe in 12 subsets based opn metabolite,
#' clean dataframes using Preprocess and fit the two models to all metabolites
#' Finally combine everything in one single dataframe
#'
#' @param data
#'
#' @returns A dataframe
#'

total_models <- function(data, metal) {
  data |>
    dplyr::group_split({{ metal }}) |>
    purrr::map(Preprocess) |>
    purrr::map(fit_all_models) |>
    purrr::list_rbind()
}


#' Title: Create plot of model results
#'
#' @param model_results
#'
#' @returns A plot object
#'
create_plot_model_results <- function(results) {
  results |>
    dplyr::filter(term == "value", std.error <= 2, estimate <= 5) |>
    dplyr::select(metabolite, model, estimate, std.error) |>
    ggplot2::ggplot(ggplot2::aes(
      x = estimate,
      y = metabolite,
      xmin = estimate - std.error,
      xmax = estimate + std.error
    )) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::facet_grid(cols = ggplot2::vars(model))
}
