#' Calculate Rolling Stochastic Oscillator
#'
#' @param x A tibble
#' @param column The column variable to perform calculations
#' @param lookback The lookback period
#'
#' @return A tibble
#' @export
#'

calculate_rolling_stochastic_oscillator <- function(x, column, lookback = 14) {

  low <- high <- NULL

  x %>%
    dplyr::mutate(low = slider::slide_dbl(.x = {{column}}, .f = min, .before = lookback, .complete = T),
                  high = slider::slide_dbl(.x = {{column}}, .f = max, .before = lookback, .complete = T),
                  stochastic_oscillator = ({{column}} - low)/(high - low)
                  ) %>%
    dplyr::select(-low, -high)

  }
