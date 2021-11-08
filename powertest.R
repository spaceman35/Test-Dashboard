library(pwr) # for power calcs
library(dplyr) # for data manipulation
library(tidyr) # for data manipulation
library(ggplot2) # for plotting power curves
library(plotly) # for interactive power curves

# Generate power calculations
ptab <- cbind(NULL, NULL)       

for (i in seq(0,1, length.out = 200)){
  pwrt1 <- pwr.t2n.test(n1 = 28, n2 = 1406, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  pwrt2 <- pwr.t2n.test(n1 = 144, n2 = 1290, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  pwrt3 <- pwr.t2n.test(n1 = 287, n2 = 1147, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  pwrt4 <- pwr.t2n.test(n1 = 430, n2 = 1004, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  pwrt5 <- pwr.t2n.test(n1 = 574, n2 = 860, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  pwrt6 <- pwr.t2n.test(n1 = 717, n2 = 717, 
                        sig.level = 0.05, power = NULL, 
                        d = i, alternative="two.sided")
  ptab <- rbind(ptab, cbind(pwrt1$d, pwrt1$power,
                            pwrt2$d, pwrt2$power,
                            pwrt3$d, pwrt3$power,
                            pwrt4$d, pwrt4$power,
                            pwrt5$d, pwrt5$power,
                            pwrt6$d, pwrt6$power))
}

ptab <- cbind(seq_len(nrow(ptab)), ptab)

colnames(ptab) <- c("id","n1=28, n2=1406.effect size","n1=28, n2=1406.power",
                    "n1=144, n2=1290.effect size","n1=144, n2=1290.power",
                    "n1=287, n2=1147.effect size","n1=287, n2=1147.power",
                    "n1=430, n2=1004.effect size","n1=430, n2=1004.power",
                    "n1=574, n2=860.effect size","n1=574, n2=860.power",
                    "n1=717, n2=717.effect size","n1=717, n2=717.power")

# get data into right format for ggplot2
temp <- ptab %>%
  as.data.frame() %>%
  gather(key = name, value = val, 2:13) %>%
  separate(col = name, into = c("group", "var"), sep = "\.") %>%
  spread(key = var, value = val)

# factor group
temp$group <- factor(temp$group, 
                     levels = c("n1=28, n2=1406", "n1=144, n2=1290", 
                                "n1=287, n2=1147", "n1=430, n2=1004",
                                "n1=574, n2=860", "n1=717, n2=717"))


# plot
p <- ggplot(temp, aes(x = `effect size`, y = power, color = group))
geom_line(size=2) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  geom_vline(xintercept = .54, linetype = 2) +
  geom_hline(yintercept = 0.80, linetype = 2)

# so simple to make interactive plots
plotly::ggplotly(p)

