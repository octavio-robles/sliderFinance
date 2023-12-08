#' Calculate Rolling Market Regime
#'
#' @param x A tibble
#' @param column The column variable to perform calculations
#' @param lookback The lookback period
#' @param column_name The Column's Output Name
#'
#' @return A tibble
#' @export
#'
#'
#'
calculate_rolling_market_regime <- function(x,column, lookback = 100, column_name = market_regime) {

  daily_returns <- mu <- market_regime <- standard_deviation <-  NULL

    dplyr::mutate(x,
                  daily_returns = log({{column}}/dplyr::lag({{column}})),
                  mu = slider::slide_dbl(.x = daily_returns, .f = mean, .before = lookback, .complete = T ),
                  standard_deviation = slider::slide_dbl(.x = daily_returns, .f = stats::sd, .before = lookback, .complete = T ),
                  sqn = mu/standard_deviation * sqrt(lookback),
                  {{column_name}} := dplyr::case_when(sqn >= 1.7 ~ as.factor("Bull Volatile"),
                                                   sqn >= .6 ~ as.factor("Bull Quiet"),
                                                   sqn >= -.6 ~ as.factor("Neutral"),
                                                   sqn >= -1.7 ~ as.factor("Bear Quiet"),
                                                   sqn < -1.7 ~ as.factor("Bear Volatile")
                                                   )
                  ) %>%
    dplyr::select(-mu, -daily_returns, -standard_deviation)


  }
