
# https://community.rstudio.com/t/spread-with-multiple-value-columns/5378/2

spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
# 
# tdata %>%
#   nest(N, Mean, Median,   SD ,  SE, .key = 'value_col', -ARMA, -TEST) %>%
#   spread(key = TEST, value = value_col) %>%
#   unnest(Amy, Bob, .sep = '_')
# tdata %>% 
#   gather(variable, value, -(ARMA:TEST)) %>%
#   unite(temp, TEST, variable) %>%
#   spread(temp, value)
# 
# tdata %>% select(-Median, -SE) %>% myspread(TEST, c(N, Mean, SD))
# 
# tdata %>%
#   nest(N, Mean, Median,   SD ,  SE, .key = 'value_col', -ARMA, -TEST) %>%
#   spread(key = TEST, value = value_col) %>%
#   unnest(Amy, Bob, .sep = '_')