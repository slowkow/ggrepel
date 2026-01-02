# Reset ggplot2 theme to default for consistent test snapshots
# This prevents ~/.Rprofile custom themes from affecting vdiffr tests
library(ggplot2)
theme_set(theme_gray())

