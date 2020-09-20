
library(pwr) # to calculate power
library(ggplot2) # plotting
library(dplyr) # to use near() for picking specific values of d in the plots (for some reason I don't understand, not all d-values in the data frame are exact, meaning they can't be indexed with "==")
library(patchwork) # to combine the two power plots into one


############
## SETUP ##
############
# create a data frame which varies n per group from 2 to 10,000 and d from 0.01 to 3 (in .01 increments):
powerdf <- expand.grid(n = c(2:10000), d = seq(.01, 3, .01)) 
# add total N to the data frame by doubling n per group:
powerdf$N <- powerdf$n*2 
# add power for alpha = .05:
powerdf$power.05 <- pwr.t.test(d = powerdf$d, 
                               n = powerdf$n, 
                               sig.level=0.05, 
                               type="two.sample",
                               alternative="two.sided")$power
# calculate power for alpha = .01:
powerdf$power.01 <- pwr.t.test(d = powerdf$d, 
                               n = powerdf$n, 
                               sig.level=0.01, 
                               type="two.sample",
                               alternative="two.sided")$power 


##############
## PLOTTING ##
##############

# First plot: Power for alpha = .01
power01.n <-
  ggplot(powerdf[near(powerdf$d, .01) | near(powerdf$d, .1) |
                   near(powerdf$d, .2) |
                   near(powerdf$d, .3) |
                   near(powerdf$d, .5) |
                   near(powerdf$d, .8) |
                   near(powerdf$d, 1.0) |
                   near(powerdf$d, 1.5), ],
         aes(x = N, y = power.01, colour = as.factor(d)))  +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, .2))) +
  scale_x_continuous(limits = c(0, 1000), breaks = c(seq(0, 1000, 100))) +
  scale_colour_viridis_d(name = "true effect size (\u03B4)") +
  labs(x = NULL,
       y = expression(power["alpha = .01"]),
       subtitle = "alpha = .01") +
  theme(legend.margin = margin(0, 0, 0, 0)) 

# Second plot: Power for alpha = .05
power05.n <-
  ggplot(powerdf[near(powerdf$d, .01) | near(powerdf$d, .1) |
                   near(powerdf$d, .2) |
                   near(powerdf$d, .3) |
                   near(powerdf$d, .5) |
                   near(powerdf$d, .8) |
                   near(powerdf$d, 1.0) |
                   near(powerdf$d, 1.5), ],
         aes(x = N, y = power.05, colour = as.factor(d)))  +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, .2))) +
  scale_x_continuous(limits = c(0, 1000), breaks = c(seq(0, 1000, 100))) +
  scale_colour_viridis_d(name = "true effect size (\u03B4)") +
  labs(x = "total sample size",
       y = expression(power["alpha = .05"]),
       subtitle = "alpha = .05") +
  theme(legend.margin = margin(0, 0, 0, 0)) 

# Combine power01.n and power05.n into one plot (stacked on top of each other):
powerstack <- power01.n/power05.n + plot_layout(guides="collect") + plot_annotation(
  title = 'What proportion of *all* p-values is below alpha (i.e., power)?',
  subtitle = '(in a two-sided independent t-test, group allocation 1:1)')

# Save the combined plot as a high-res PNG file
png(filename = "powerstack01vs05.png",
    width = 1600, height = 1700, units = "px", pointsize = 12,
    bg = "white", res = 300, 
    type = "cairo")
powerstack
dev.off()


# Third plot: Proportion of significant p-values below .01 (i.e., power for alpha = .01 divided by power for alpha = .05) as a function of sample size:
powerprop.n.d <- ggplot(powerdf[near(powerdf$d, .01) | near(powerdf$d, .1) | 
                                  near(powerdf$d, .2) | near(powerdf$d, .3) |
                                  near(powerdf$d, .5) | near(powerdf$d, .8) |
                                  near(powerdf$d, 1.0) | near(powerdf$d, 1.5),], 
                        aes(x=N, y=power.01/power.05, colour = as.factor(d)))  +  
  geom_hline(aes(yintercept=0.5), colour="#999999") +
  geom_line() +  
  annotate(geom = "text", x = -60, y = .57, 
           label = ">50% of sig. results below .01", 
           hjust = 0, angle = 90,
           color = "#999999", size = 2.5) +
  annotate(geom = "segment", x = -60, y = .5, xend = -60, yend = .55,
           color = "#999999", 
           arrow = arrow(length = unit(0.02, "npc"))) +
  annotate(geom = "text", x = -40, y = .43,
           label = "<50% of sig. results below .01",
           hjust = 0, angle = 270,
           color = "#999999", size = 2.5) +
  annotate(geom = "segment", x = -40, y = .5, xend = -40, yend = .45,
           color = "#999999", 
           arrow = arrow(length = unit(0.02, "npc"))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, .2))) +
  scale_x_continuous(limits = c(-60, 1000), breaks = c(seq(0,1000, 100))) +
  scale_colour_viridis_d(name = "true effect size (\u03B4)")+
  labs(x="total sample size",
       subtitle = "(in a two-sided independent t-test, group allocation 1:1)") +
  labs(y=expression(power["alpha = .01"]/power["alpha = .05"]),
       title = "What proportion of significant p-values (p<.05) is below .01?") +
  coord_fixed(expand = TRUE) +
  theme(aspect.ratio = 0.85,
        legend.margin = margin(0, 0, 0, 0))

# Save powerprop.n.d as a high-res PNG file:
png(filename = "powerproportionalpha01vs05_n.png",
    width = 2450, height = 1400, units = "px", pointsize = 12,
    bg = "white", res = 300, type = "cairo")
powerprop.n.d
dev.off()
  
# Fourth plot: Proportion of significant p-values below .01 (i.e., power for alpha = .01 divided by power for alpha = .05) as a function of power for alpha = .05:
powerprop.power.n <- ggplot(powerdf[powerdf$n==10 | powerdf$n==20 | 
                                      powerdf$n==50 | powerdf$n==100 |
                                      powerdf$n==500 | powerdf$n==1000 |
                                      powerdf$n==5000 | powerdf$n==10000,], 
                            aes(x=power.05, y=power.01/power.05, colour = as.factor(N)))+   
  geom_hline(aes(yintercept=0.5), colour="#999999") + 
  geom_line() +  
  scale_colour_viridis_d(name = "total sample size") +
  labs(x=expression(power["alpha = .05"]),
       y=expression(power["alpha = .01"]/power["alpha = .05"]),
       title = "What proportion of significant p-values (p<.05) is below .01?",
       subtitle = "(in a two-sided independent t-test, true effect size \u03B4 = 0.01 to \u03B4 = 3)") +
  annotate(geom = "text", x = .05, y = .57, 
           label = ">50% of sig. results below .01", 
           hjust = 0, angle = 90,
           color = "#999999", size = 2.5) +
  annotate(geom = "segment", x = .05, y = .5, xend = .05, yend = .55,
           color = "#999999", 
           arrow = arrow(length = unit(0.02, "npc"))) +
  annotate(geom = "text", x = .85, y = .43, 
           label = "<50% of sig. results below .01", 
           hjust = 0, angle = 270,
           color = "#999999", size = 2.5) +
  annotate(geom = "segment", x = .85, y = .5, xend = .85, yend = .45,
           color = "#999999", 
           arrow = arrow(length = unit(0.02, "npc"))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, .2))) +
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0,1, .2))) +  
  coord_fixed(expand = TRUE) +
  theme(aspect.ratio = 0.85,
        legend.margin = margin(0, 6, 0, 0))

# Save powerprop.power.n as a high-res PNG file:
png(
  filename = "powerproportionalpha01vs05_power05.png",
  width = 2450, height = 1400,units = "px", pointsize = 12,
  bg = "white", res = 300, type = "cairo")
powerprop.power.n
dev.off()

