### packages

library(stargazer)
library(tidyverse)
library(fixest)
library(devtools)
library(gapminder)
library(xtable)
source("functions/collect_coefs.R")

# Set your wd here
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_1 <- theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16),
        plot.title.position = "plot")

### Importing data
trade <- read.csv(unz('data/trade.zip', 'trade.csv'), 
                    skipNul = T)
gravity <- read.csv(unz('data/gravity.zip', 'gravity.csv'), 
                  skipNul = T)


### Task 2: Data manipulation

## a) collapse

trade_all <- trade %>%
  group_by(year, origin, destination) %>%
  summarize(trade = sum(trade))

## b) merging

tr_merged <- trade_all %>%
  inner_join(gravity, # inner join will only keep obs present in both dtasets
             by = c("year", "origin", "destination"))

## c) descriptive stats

desc_stat2015 <- tr_merged %>%
  filter(year == 2015) %>%
  ungroup() %>%
  select(-c(year, origin, destination)) %>%
  na.omit() %>% 
  gather(Variable, value) %>%
  # Summarize by variable
  group_by(Variable) %>%
  # summarise all columns
  summarise(N = sum(!is.na(value)),
            `Mean` = mean(value),
            `Std. Dev.` = sd(value),
            `Median` = median(value),
            `10th Percentile` = quantile(value, .1),
            `90th Percentile` = quantile(value, .9))

# create a latex table
desc_tab <- xtable(desc_stat2015,
                   digits = 2,
                   caption = 'Summary statistics for year 2015') %>%
  print(type = "latex",
        include.rownames = FALSE,
        format.args = list(big.mark = ","),
        file = "tables/sum2015.tex")

### Task 3: Estimation

## a)

# select 2015 and add logged variables
tr_2015 <- tr_merged %>%
  filter(year == 2015) %>%
  na.omit() %>%
  mutate(log_dist = log(distance),
         log_tr = log(trade))


# create bins
nbins = 50

pdf(file = "fig/binplot.pdf")
tr_2015 %>% 
  # here I just use equally spaced bins for simplicity
  mutate(bin = ntile(log_tr, n = nbins)) %>%
  group_by(bin) %>%
  # I summarize the variables by taking the mean inside the bins
  summarise(log_tr = mean(log_tr), log_dist = mean(log_dist)) %>%
  ggplot(aes(x = log_tr, y = log_dist)) +
  geom_point() +
  theme_1 +
  labs(title = paste0("Distance and trade: binplot, ", nbins, " bins", sep = ""), 
       x = "log(trade), bin mean",
       y = "log(distance), bin mean")
dev.off()

# pearson correlation
print(cor.test(tr_2015$log_tr, tr_2015$log_dist))


## b)

ols_year <- feols(log(trade) ~ log(distance), 
                  data = tr_merged, split = ~year)

# collect coefficients manually
coefs_dst <- collect_coefs(model = ols_year, 
                           variable = "log(distance)", 
                           confIntr = 0.95)
coefs_dst <- as.data.frame(coefs_dst) %>%
  mutate_if(is.character, as.numeric)


# plot
pdf(file = "fig/dist_coef_simple.pdf")
coefs_dst %>%
  ggplot(aes(y = coef, x = smpl)) +
  geom_point(aes(stroke = 1.5)) +
  geom_errorbar(aes(ymin = right, ymax = left, width = .4)) +
  theme_1 +
  scale_y_continuous(limits = c(-1.7, -0.7)) +
  theme(plot.title = element_text(hjust = 0.8),
        plot.margin = margin(1,1,1.5,1.2, "cm"))  + 
  labs(title = TeX("$rho^t$ estimates with 95% conf. bounds"), 
       x = "Year",
       y = "Distance coefficient")
dev.off()


## c)

# since we estimate for each year separately, origin and destination FEs
# will suffice instead of origin x year and destination x year
ols_fe <- feols(log(trade) ~ log(distance) | origin + destination, 
                data = tr_merged, split = ~year)

# collect the coefficients
coefs_dst_fe <- collect_coefs(model = ols_fe, 
                              variable = "log(distance)", 
                              confIntr = 0.95)
coefs_dst_fe <- as.data.frame(coefs_dst_fe) %>%
  mutate_if(is.character, as.numeric)

# plot
pdf(file = "fig/dist_coef_fe.pdf")
coefs_dst_fe %>%
  ggplot(aes(y = coef, x = smpl)) +
  geom_point(aes(stroke = 1.5)) +
  geom_errorbar(aes(ymin = right, ymax = left, width = .4)) +
  theme_1 +
  scale_y_continuous(limits = c(-2, -1.5)) +
  theme(plot.title = element_text(hjust = 0.8),
        plot.margin = margin(1, 1, 1.5, 1.2, "cm"))  + 
  labs(title = TeX("$beta^t$ estimates with 95% conf. bounds"), 
       x = "Year",
       y = "Distance coefficient")
dev.off()



## d)


# estimation
ols_full <- feols(log(trade) ~ log(distance) + 
                    contiguity + language + colonial + rta 
                  | origin + destination, 
                  data = tr_2015)
# table
etable(ols_full, tex = TRUE, file = "tables/full.tex", digits = 3, replace = T)

