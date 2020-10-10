library(abind)
library(purrr)
library(reshape2)
library(ggplot2)
library(dplyr)

### For batch size nb = 1000


load("./james_sims/sim_runs_nb_1000.Rdata")

dftt$CER = NA
dftt$CER[dftt$s == .002] = 'worst'
dftt$CER[dftt$s == .01] = 'base'
dftt$CER[dftt$s == .015] = 'best'
dftt$CER = factor(dftt$CER, levels = c('worst', 'base', 'best'))
dftt$RRR = as.factor(dftt$RRR)
dftt$sup = as.factor(dftt$sup)
dftt$fut = as.factor(dftt$fut)

dft = melt(dftt, measure.vars = c('pvalue1', 'pvalue2', 'pvalue3'))
names(dft)[names(dft) == 'value'] = 'p_value'
dft$outcome = NA
dft$outcome[dft$variable == 'pvalue1'] = 's'
dft$outcome[dft$variable == 'pvalue2'] = 'p1'
dft$outcome[dft$variable == 'pvalue3'] = 'p2'
dft$outcome = factor(dft$outcome, levels = c('s', 'p1', 'p2'))
dft$variable = NULL
dft$RRR_hat = c(dftt$RRR_hat1, dftt$RRR_hat2, dftt$RRR_hat3)
dft$power = c(dftt$pow1, dftt$pow2, dftt$pow3)
dft$event.rate = c(dftt$event1, dftt$event2, dftt$event3)
dft$pvalue = NA
dft$pvalue[dft$p_value < 0.05] = '< 5%'
dft$pvalue[dft$p_value > 0.05 & dft$p_value < 0.1] = '5% - 10%'
dft$pvalue[dft$p_value > 0.1] = '> 10%'
dft$pvalue = factor(dft$pvalue, levels = c('< 5%', '5% - 10%', '> 10%'))


dft0 = dft %>%
  filter(RRR != 0)

dft00 = dft %>%
  filter(RRR == 0)

dft1 = dft %>%
  filter(futstop == 1)

dft2 = dft %>%
  filter(supstop == 1)

dft01 = dft0 %>%
  filter(futstop == 1)

dft02 = dft0 %>%
  filter(supstop == 1)

### P-value plot

ggplot(dft0, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

### P-value stopping for futility of p1

ggplot(dft01, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'pvalue_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

### P-value stopping for superiority of p1

ggplot(dft02, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

### RRR_hat when stopping for p1

#quantile(dft$RRR_hat, 0.01)

ggplot(dft, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  scale_x_continuous(limits = c(-1, 1)) +
  #coord_cartesian(xlim = c(-10, 10)) + ## Added here to get better look
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# There were 2963 out of 112500 rows removed for not staying in range of (-1, 1)
ggsave(file = 'RRRhat_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

### RRR_hat when stopping for futility on p1

ggplot(dft1, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 1653 of 19521 rows not in range of (-1, 1)
ggsave(file = 'RRRhat_futp1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

### RRR_hat when stopping for superiority on p1

ggplot(dft2, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 1185 of 67065 rows not in range (-1, 1)
ggsave(file = 'RRRhat_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

#ggplot(dft, aes(x = event.rate, fill = outcome)) + geom_histogram() + facet_grid(fut ~ sup)
# ggplot(dft, aes(x = Nt, fill = sup)) + geom_bar(position = position_dodge()) +
#   facet_grid(CER ~ RRR, labeller = label_both)

# For two arm trial don't include /3?
ggplot(dft, aes(x = sup, y = Nt, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + ylab('Nt') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  xlab('superiority threshold') + ylab('expected sample size at trial termination') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'exp_sample_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft0, aes(x = outcome, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft00, aes(x = fut, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) + ylab('type I error rate') +
  facet_grid(CER ~ sup, labeller = label_both) + xlab('futility threshold') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'type_1_error_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft, aes(x = sup, y = early, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft, aes(x = sup, y = futstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for futility') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft, aes(x = sup, y = supstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for superiority') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft, aes(x = sup, y = reachmax, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of reaching max sample size') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_reach_max_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 10)

ggplot(dft0, aes(x = sup, y = power, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR*outcome, labeller = label_both) + xlab('superiority threshold') +
  ylab('power') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_all_p1.png', path = 'james_sims/p1_plots/batch_size_nb_1000', width = 15)

#---------------------------------------------------------------------------------------------------------------------------------#

# For batch size nb = 2000

#---------------------------------------------------------------------------------------------------------------------------------#


load("./james_sims/sim_runs_nb_2000.Rdata")

dftt$CER = NA
dftt$CER[dftt$s == .002] = 'worst'
dftt$CER[dftt$s == .01] = 'base'
dftt$CER[dftt$s == .015] = 'best'
dftt$CER = factor(dftt$CER, levels = c('worst', 'base', 'best'))
dftt$RRR = as.factor(dftt$RRR)
dftt$sup = as.factor(dftt$sup)
dftt$fut = as.factor(dftt$fut)

dft = melt(dftt, measure.vars = c('pvalue1', 'pvalue2', 'pvalue3'))
names(dft)[names(dft) == 'value'] = 'p_value'
dft$outcome = NA
dft$outcome[dft$variable == 'pvalue1'] = 's'
dft$outcome[dft$variable == 'pvalue2'] = 'p1'
dft$outcome[dft$variable == 'pvalue3'] = 'p2'
dft$outcome = factor(dft$outcome, levels = c('s', 'p1', 'p2'))
dft$variable = NULL
dft$RRR_hat = c(dftt$RRR_hat1, dftt$RRR_hat2, dftt$RRR_hat3)
dft$power = c(dftt$pow1, dftt$pow2, dftt$pow3)
dft$event.rate = c(dftt$event1, dftt$event2, dftt$event3)
dft$pvalue = NA
dft$pvalue[dft$p_value < 0.05] = '< 5%'
dft$pvalue[dft$p_value > 0.05 & dft$p_value < 0.1] = '5% - 10%'
dft$pvalue[dft$p_value > 0.1] = '> 10%'
dft$pvalue = factor(dft$pvalue, levels = c('< 5%', '5% - 10%', '> 10%'))


dft0 = dft %>%
  filter(RRR != 0)

dft00 = dft %>%
  filter(RRR == 0)

dft1 = dft %>%
  filter(futstop == 1)

dft2 = dft %>%
  filter(supstop == 1)

dft01 = dft0 %>%
  filter(futstop == 1)

dft02 = dft0 %>%
  filter(supstop == 1)

### P-value plot

ggplot(dft0, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

### P-value stopping for futility of p1

ggplot(dft01, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'pvalue_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

### P-value stopping for superiority of p1

ggplot(dft02, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

### RRR_hat when stopping for p1

#quantile(dft$RRR_hat, 0.01)

ggplot(dft, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  scale_x_continuous(limits = c(-1, 1)) +
  #coord_cartesian(xlim = c(-10, 10)) + ## Added here to get better look
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# There were 2003 out of 112500 rows removed for not staying in range of (-1, 1)
ggsave(file = 'RRRhat_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

### RRR_hat when stopping for futility on p1

ggplot(dft1, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 1083 of 19521 rows not in range of (-1, 1)
ggsave(file = 'RRRhat_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

### RRR_hat when stopping for superiority on p1

ggplot(dft2, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 791 of 67065 rows not in range (-1, 1)
ggsave(file = 'RRRhat_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

#ggplot(dft, aes(x = event.rate, fill = outcome)) + geom_histogram() + facet_grid(fut ~ sup)
# ggplot(dft, aes(x = Nt, fill = sup)) + geom_bar(position = position_dodge()) +
#   facet_grid(CER ~ RRR, labeller = label_both)

# For two arm trial don't include /3?
ggplot(dft, aes(x = sup, y = Nt, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + ylab('Nt') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  xlab('superiority threshold') + ylab('expected sample size at trial termination') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'exp_sample_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft0, aes(x = outcome, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft00, aes(x = fut, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) + ylab('type I error rate') +
  facet_grid(CER ~ sup, labeller = label_both) + xlab('futility threshold') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'type_1_error_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft, aes(x = sup, y = early, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft, aes(x = sup, y = futstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for futility') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft, aes(x = sup, y = supstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for superiority') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft, aes(x = sup, y = reachmax, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of reaching max sample size') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_reach_max_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 10)

ggplot(dft0, aes(x = sup, y = power, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR*outcome, labeller = label_both) + xlab('superiority threshold') +
  ylab('power') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_all_p1.png', path = 'james_sims/p1_plots/batch_size_nb_2000', width = 15)

#---------------------------------------------------------------------------------------------------------------------------------#

# For batch size nb = 3000

#---------------------------------------------------------------------------------------------------------------------------------#


load("./james_sims/sim_runs_nb_3000.Rdata")

dftt$CER = NA
dftt$CER[dftt$s == .002] = 'worst'
dftt$CER[dftt$s == .01] = 'base'
dftt$CER[dftt$s == .015] = 'best'
dftt$CER = factor(dftt$CER, levels = c('worst', 'base', 'best'))
dftt$RRR = as.factor(dftt$RRR)
dftt$sup = as.factor(dftt$sup)
dftt$fut = as.factor(dftt$fut)

dft = melt(dftt, measure.vars = c('pvalue1', 'pvalue2', 'pvalue3'))
names(dft)[names(dft) == 'value'] = 'p_value'
dft$outcome = NA
dft$outcome[dft$variable == 'pvalue1'] = 's'
dft$outcome[dft$variable == 'pvalue2'] = 'p1'
dft$outcome[dft$variable == 'pvalue3'] = 'p2'
dft$outcome = factor(dft$outcome, levels = c('s', 'p1', 'p2'))
dft$variable = NULL
dft$RRR_hat = c(dftt$RRR_hat1, dftt$RRR_hat2, dftt$RRR_hat3)
dft$power = c(dftt$pow1, dftt$pow2, dftt$pow3)
dft$event.rate = c(dftt$event1, dftt$event2, dftt$event3)
dft$pvalue = NA
dft$pvalue[dft$p_value < 0.05] = '< 5%'
dft$pvalue[dft$p_value > 0.05 & dft$p_value < 0.1] = '5% - 10%'
dft$pvalue[dft$p_value > 0.1] = '> 10%'
dft$pvalue = factor(dft$pvalue, levels = c('< 5%', '5% - 10%', '> 10%'))


dft0 = dft %>%
  filter(RRR != 0)

dft00 = dft %>%
  filter(RRR == 0)

dft1 = dft %>%
  filter(futstop == 1)

dft2 = dft %>%
  filter(supstop == 1)

dft01 = dft0 %>%
  filter(futstop == 1)

dft02 = dft0 %>%
  filter(supstop == 1)

### P-value plot

ggplot(dft0, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

### P-value stopping for futility of p1

ggplot(dft01, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'pvalue_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

### P-value stopping for superiority of p1

ggplot(dft02, aes(x = pvalue, fill = outcome)) +
  geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  xlab('p-value') + ylab('probability') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))

ggsave(file = 'pvalue_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

### RRR_hat when stopping for p1

#quantile(dft$RRR_hat, 0.01)

ggplot(dft, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  scale_x_continuous(limits = c(-1, 1)) +
  #coord_cartesian(xlim = c(-10, 10)) + ## Added here to get better look
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# There were 1487 out of 112500 rows removed for not staying in range of (-1, 1)
ggsave(file = 'RRRhat_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

### RRR_hat when stopping for futility on p1

ggplot(dft1, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 798 of 19521 rows not in range of (-1, 1)
ggsave(file = 'RRRhat_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

### RRR_hat when stopping for superiority on p1

ggplot(dft2, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_text(size = 14),
        axis.title.x= element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
# Removed 541 of 67065 rows not in range (-1, 1)
ggsave(file = 'RRRhat_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

#ggplot(dft, aes(x = event.rate, fill = outcome)) + geom_histogram() + facet_grid(fut ~ sup)
# ggplot(dft, aes(x = Nt, fill = sup)) + geom_bar(position = position_dodge()) +
#   facet_grid(CER ~ RRR, labeller = label_both)

# For two arm trial don't include /3?
ggplot(dft, aes(x = sup, y = Nt, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + ylab('Nt') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  xlab('superiority threshold') + ylab('expected sample size at trial termination') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'exp_sample_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft0, aes(x = outcome, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft00, aes(x = fut, y = power, fill = outcome)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) + ylab('type I error rate') +
  facet_grid(CER ~ sup, labeller = label_both) + xlab('futility threshold') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'type_1_error_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft, aes(x = sup, y = early, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft, aes(x = sup, y = futstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for futility') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_fut_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft, aes(x = sup, y = supstop, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of stopping early for superiority') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_stop_early_sup_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft, aes(x = sup, y = reachmax, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
  ylab('probability of reaching max sample size') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'prob_reach_max_size_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 10)

ggplot(dft0, aes(x = sup, y = power, fill = fut)) +
  stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
  facet_grid(CER ~ RRR*outcome, labeller = label_both) + xlab('superiority threshold') +
  ylab('power') +
  scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 14, face = 'bold'),
        strip.text.x = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 'bold'))
ggsave(file = 'power_all_p1.png', path = 'james_sims/p1_plots/batch_size_nb_3000', width = 15)





