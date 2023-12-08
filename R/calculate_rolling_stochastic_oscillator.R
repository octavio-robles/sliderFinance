#' Calculate Rolling Stochastic Oscillator
#'
#' @param x A tibble
#' @param column The column variable to perform calculations
#' @param lookback The lookback period
#' @param column_name The Column's Output Name
#'
#' @return A tibble
#' @export
#'

calculate_rolling_stochastic_oscillator <- function(x, column, lookback = 14, column_name = stochastic_oscillator) {

  low <- high <- stochastic_oscillator <-  NULL

  x %>%
    dplyr::mutate(low = slider::slide_dbl(.x = {{column}}, .f = min, .before = lookback, .complete = T),
                  high = slider::slide_dbl(.x = {{column}}, .f = max, .before = lookback, .complete = T),
                  {{column_name}} := ({{column}} - low)/(high - low)
                  ) %>%
    dplyr::select(-low, -high)

  }
