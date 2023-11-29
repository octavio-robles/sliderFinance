#' Calculate Rolling Zscore
#'
#' @param x A data tibble
#' @param column The column variable to perform calculations
#' @param lookback The lookback period
#'
#' @return A tibble
#' @export

calculate_rolling_zscore <- function(x, column, lookback = 20) {

  mu <- standard_deviation <- NULL

  x %>%
    dplyr::mutate(mu = slider::slide_dbl(.x = {{column}}, .f = mean, .before = lookback, .complete = T),
                  standard_deviation = slider::slide_dbl(.x = {{column}}, .f = stats::sd, .before = lookback, .complete = T),
                  zscore = ({{column}}-mu)/standard_deviation
                  ) %>%
    dplyr::select(-mu, -standard_deviation)

}
