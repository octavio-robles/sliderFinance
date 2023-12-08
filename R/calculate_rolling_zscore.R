#' Calculate Rolling Zscore
#'
#' @param x A data tibble
#' @param column The column variable to perform calculations
#' @param lookback The lookback period
#' @param column_name The Column's Output Name
#'
#' @return A tibble
#' @export

calculate_rolling_zscore <- function(x, column, lookback = 20, column_name = zscore ) {

  mu <- standard_deviation <-  zscore <- NULL

  x %>%
    dplyr::mutate(mu = slider::slide_dbl(.x = {{column}}, .f = mean, .before = lookback, .complete = T),
                  standard_deviation = slider::slide_dbl(.x = {{column}}, .f = stats::sd, .before = lookback, .complete = T),
                  {{column_name}} := ({{column}}-mu)/standard_deviation
                  ) %>%
    dplyr::select(-mu, -standard_deviation)

}
