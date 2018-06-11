library(microbenchmark)
library(ggrepel)

d <- mtcars
d$name <- rownames(mtcars)
d <- as.data.frame(rbind(d, d))
d$wt[33:64] <- d$wt[33:64] + 1
d$mpg[33:64] <- d$mpg[33:64] + 5

microbenchmark(
  geom_text_repel = {
    p <- ggplot(d, aes(wt, mpg, label = name)) +
      geom_point(color = 'red') +
      geom_text_repel() +
      theme_classic(base_size = 16)
    print(p)
  },
  geom_text = {
    p <- ggplot(d, aes(wt, mpg, label = name)) +
      geom_point(color = 'red') +
      geom_text() +
      theme_classic(base_size = 16)
    print(p)
  },
  times = 25L
)

# 2018-02-11 ggrepel-0.7.3 ggplot2-2.2.1
# Unit: milliseconds
#            expr       min        lq      mean    median        uq       max neval
# geom_text_repel 1233.0252 1317.1580 1368.4266 1326.6279 1353.8870 1833.3384    25
#       geom_text  587.5614  601.8036  647.0909  666.5325  686.1348  758.1243    25

# Github:
# Unit: milliseconds
#            expr      min       lq     mean   median       uq      max neval
# geom_text_repel 582.9879 612.3658 633.6304 635.0404 644.7016 713.2744    25
#       geom_text 161.9298 207.8709 215.0703 217.9270 224.5474 292.4633    25

# HEAD:
# Unit: milliseconds
#            expr      min       lq     mean   median       uq      max neval
# geom_text_repel 568.7964 588.8807 607.2520 598.2813 616.9077 702.8651    25
#       geom_text 160.1820 182.4907 202.4225 206.1050 212.1132 247.8276    25
