# load('sim04_p2_res.Rdata')
#
#
# dftt$CER = NA
# dftt$CER[dftt$s == .002] = 'worst'
# dftt$CER[dftt$s == .01] = 'base'
# dftt$CER[dftt$s == .015] = 'best'
# dftt$CER = factor(dftt$CER, levels = c('worst', 'base', 'best'))
# dftt$RRR = as.factor(dftt$RRR)
# dftt$sup = as.factor(dftt$sup)
# dftt$fut = as.factor(dftt$fut)
#
#
# dft = melt(dftt, measure.vars = c('pvalue1', 'pvalue2', 'pvalue3'))
# names(dft)[names(dft) == 'value'] = 'p_value'
# dft$outcome = NA
# dft$outcome[dft$variable == 'pvalue1'] = 's'
# dft$outcome[dft$variable == 'pvalue2'] = 'p1'
# dft$outcome[dft$variable == 'pvalue3'] = 'p2'
# dft$outcome = factor(dft$outcome, levels = c('s', 'p1', 'p2'))
# dft$variable = NULL
# dft$RRR_hat = c(dftt$RRR_hat1, dftt$RRR_hat2, dftt$RRR_hat3)
# dft$power = c(dftt$pow1, dftt$pow2, dftt$pow3)
# dft$event.rate = c(dftt$event1, dftt$event2, dftt$event3)
# dft$pvalue = NA
# dft$pvalue[dft$p_value < 0.05] = '< 5%'
# dft$pvalue[dft$p_value > 0.05 & dft$p_value < 0.1] = '5% - 10%'
# dft$pvalue[dft$p_value > 0.1] = '> 10%'
# dft$pvalue = factor(dft$pvalue, levels = c('< 5%', '5% - 10%', '> 10%'))
#
# dft0 = dft %>%
#   filter(RRR != 0)
#
# dft00 = dft %>%
#   filter(RRR == 0)
#
# dft1 = dft %>%
#   filter(futstop == 1)
#
# dft2 = dft %>%
#   filter(supstop == 1)
#
# dft01 = dft0 %>%
#   filter(futstop == 1)
#
# dft02 = dft0 %>%
#   filter(supstop == 1)
#
# ggplot(dft, aes(x = p_value, color = outcome)) + geom_freqpoly() + facet_grid(CER ~ RRR)
# ggplot(dft0, aes(x = pvalue, fill = outcome)) +
#   geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) +
#   xlab('p-value') + ylab('probability') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'pvaluep2.png', width = 10)
# ggplot(dft01, aes(x = pvalue, fill = outcome)) +
#   geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) +
#   xlab('p-value') + ylab('probability') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'pvalue_futp2.png', width = 10)
#
# ggplot(dft02, aes(x = pvalue, fill = outcome)) +
#   geom_bar(aes(y = (..count..)/4500), position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) +
#   xlab('p-value') + ylab('probability') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'pvalue_supp2.png', width = 10)
# ggplot(dft, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
#   theme(axis.text.y= element_blank(),
#         axis.title.y= element_blank(),
#         axis.text.x= element_text(size = 14),
#         axis.title.x= element_text(size = 14, face = 'bold'),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'RRRhatp2.png', width = 10)
# ggplot(dft1, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
#   theme(axis.text.y= element_blank(),
#         axis.title.y= element_blank(),
#         axis.text.x= element_text(size = 14),
#         axis.title.x= element_text(size = 14, face = 'bold'),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'RRRhat_futp2.png', width = 10)
# ggplot(dft2, aes(x = RRR_hat, color = outcome)) + geom_density(size = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('relative risk reduction') +
#   theme(axis.text.y= element_blank(),
#         axis.title.y= element_blank(),
#         axis.text.x= element_text(size = 14),
#         axis.title.x= element_text(size = 14, face = 'bold'),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'RRRhat_supp2.png', width = 10)
# #ggplot(dft, aes(x = event.rate, fill = outcome)) + geom_histogram() + facet_grid(fut ~ sup)
# #ggplot(dft, aes(x = Nt, fill = sup)) + geom_bar(position = position_dodge()) +
# #  facet_grid(CER ~ RRR, labeller = label_both)
# ggplot(dft, aes(x = sup, y = Nt/3, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + ylab('Nt') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   xlab('superiority threshold') + ylab('expected sample size at trial termination') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'Ntp2.png', width = 10)
#
# ggplot(dft0, aes(x = outcome, y = power, fill = outcome)) +
#   stat_summary(fun.y="mean", geom="bar", alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'powerp2.png', width = 10)
#
# ggplot(dft00, aes(x = fut, y = power, fill = outcome)) +
#   stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) + ylab('type I error rate') +
#   facet_grid(CER ~ sup, labeller = label_both) + xlab('futility threshold') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'alphap2.png', width = 10)
#
# ggplot(dft, aes(x = sup, y = early, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
#   ylab('probability of stopping early') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'earlyp2.png', width = 10)
#
# ggplot(dft, aes(x = sup, y = futstop, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
#   ylab('probability of stopping early for futility') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'early_futp2.png', width = 10)
#
# ggplot(dft, aes(x = sup, y = supstop, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar",position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
#   ylab('probability of stopping early for superiority') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'early_supp2.png', width = 10)
#
# ggplot(dft, aes(x = sup, y = reachmax, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR, labeller = label_both) + xlab('superiority threshold') +
#   ylab('probability of reaching max sample size') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'reachmaxp2.png', width = 10)
#
#
# ggplot(dft0, aes(x = sup, y = power, fill = fut)) +
#   stat_summary(fun.y="mean", geom="bar", position = position_dodge(), alpha = .8) +
#   facet_grid(CER ~ RRR*outcome, labeller = label_both) + xlab('superiority threshold') +
#   ylab('power') +
#   scale_fill_brewer(name = "futility\nthreshold") + theme_dark() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"),
#         strip.text.y = element_text(size = 14, face = 'bold'),
#         strip.text.x = element_text(size = 14, face = 'bold'),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = 'bold'))
# ggsave(file = 'power_allp2.png', width = 15)
#
#
