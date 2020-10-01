#' Probability of superiority plot
#'
#' @param trial An object of class 'trial'
#'
#' @return Probabilities of superiority for each arm evolving through the course of the trial
#' @export


psup_plot_12 = function(trial, upper) {
  psup = trial$psup
  df = melt(psup)
  names(df) = c('treatment', 'interim_look', 'p.best')
  #df$treatment = as.factor(df$treatment)
  df$treatment[df$treatment == 1] = '1:placebo'
  df$treatment[df$treatment == 2] = '2:L.Plantarum'
  df$treatment[df$treatment == 3] = '3:B.Infantis(21d)'
  df$treatment[df$treatment == 4] = '4:B.Infantis(7d)'
  p = ggplot(df, aes(x = interim_look, y = p.best, color = treatment)) +
    geom_line() + geom_hline(yintercept = upper, color = 'darkgrey') +
    scale_color_brewer(palette = 'Set1') +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          strip.text.x = element_text(size = 8, face = "bold"))
  return(p)
}

#' Probability of superiority plot
#'
#' @param trial An object of class 'trial'
#'
#' @return Probabilities of superiority for each arm evolving through the course of the trial
#' @export


psup_plot_34 = function(trial, upper) {
  psup = trial$psup
  df = melt(psup)
  names(df) = c('treatment', 'interim_look', 'p.best')
  #df$treatment = as.factor(df$treatment)
  df$treatment[df$treatment == 1] = '1:placebo'
  df$treatment[df$treatment == 2] = '2:L.Plantarum'
  df$treatment[df$treatment == 3] = '3:B.Infantis'
  df$treatment[df$treatment == 4] = '4:Combo'
  p = ggplot(df, aes(x = interim_look, y = p.best, color = treatment)) +
    geom_line() + geom_hline(yintercept = upper, color = 'darkgrey') +
    scale_color_brewer(palette = 'Set1') +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          strip.text.x = element_text(size = 8, face = "bold"))
  return(p)
}

#' Posterior density plot
#'
#' @param trial An object of class 'trial'
#' @param type a character representing the type of the parameter; "absolute", "OR" (odds ratio),
#' or "RR" (relative risk)
#' @return 5 snapshots of the posterior density plots for the effect sizes through teh trial
#' @export

post_plot = function(trial, type) {
  if (type == "absolute") {
    theta = trial$theta
    np = dim(theta)[3]
    select = floor(seq(1, np, length = 5))
    df = melt(theta)[,-1]
    names(df) = c('treatment', 'interim_look', 'effect')
    df0 = df[df$interim_look %in% select,]
    df0$interim_look = as.factor(df0$interim_look)
    #df1 = data.frame(theta0 = theta0, treatment = as.factor(sort(unique(df0$treatment))))
    p = ggplot(df0, aes(x = effect, fill = interim_look)) +
      geom_density(alpha = .5, color = 'grey') +
      #geom_vline(data = df1, aes(xintercept = theta0)) +
      facet_grid(treatment ~ ., labeller = label_both) +
      scale_fill_brewer() + xlim(-3, 3) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            strip.text.y = element_text(size = 12, face = "bold"))
  }
  if (type == "count") {
    theta = exp(trial$theta)
    np = dim(theta)[3]
    select = floor(seq(1, np, length = 5))
    df = melt(theta)[,-1]
    names(df) = c('treatment', 'interim_look', 'intensity')
    df0 = df[df$interim_look %in% select,]
    df0$interim_look = as.factor(df0$interim_look)
    #df1 = data.frame(theta0 = theta0, treatment = as.factor(sort(unique(df0$treatment))))
    p = ggplot(df0, aes(x = intensity, fill = interim_look)) +
      geom_density(alpha = .5, color = 'grey') +
      #geom_vline(data = df1, aes(xintercept = theta0)) +
      facet_grid(treatment ~ ., labeller = label_both) +
      scale_fill_brewer() + xlim(0, 10) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            strip.text.y = element_text(size = 12, face = "bold"))
  }
  if (type == "rate") {
    theta = exp(trial$theta)/(1 + exp(trial$theta))
    np = dim(theta)[3]
    select = floor(seq(1, np, length = 5))
    df = melt(theta)[,-1]
    names(df) = c('treatment', 'interim_look', 'rate')
    df0 = df[df$interim_look %in% select,]
    df0$interim_look = as.factor(df0$interim_look)
    #df1 = data.frame(theta0 = theta0, treatment = as.factor(sort(unique(df0$treatment))))
    p = ggplot(df0, aes(x = rate, fill = interim_look)) +
      geom_density(alpha = .5, color = 'grey') +
      #geom_vline(data = df1, aes(xintercept = theta0)) +
      facet_grid(treatment ~ ., labeller = label_both) +
      scale_fill_brewer() + xlim(0, 1) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            strip.text.y = element_text(size = 12, face = "bold"))
  }
  return(p)
}

#' Data plot
#'
#' @param trial An object of class 'trial'
#'
#' @return A visual summary of the trial data
#' @export

data_plot_34 = function(trial) {
  y = trial$y
  x = trial$x
  nt = nrow(x)
  treat = 1:nt
  df = data.frame(cbind(y, t(x)%*%treat, 1:length(y)))
  names(df) = c('response', 'treatment', 'patient')
  df$response[df$response == 0] = 'event'
  df$response[df$response == 1] = 'safe'
  df$treatment[df$treatment == 1] = '1:placebo'
  df$treatment[df$treatment == 2] = '2:L.Plant'
  df$treatment[df$treatment == 3] = '3:B.Inf'
  df$treatment[df$treatment == 4] = '4:Combo'
  df$treatment = as.factor(df$treatment)
  if (all(y %in% c(0,1))) df$response = as.factor(df$response)
  p = ggplot(df, aes(x = patient, y = response, color = response)) +
    geom_point(size = 3) + facet_grid(treatment ~ .) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          strip.text.y = element_text(size = 8))
  return(p)
}

#' Data plot
#'
#' @param trial An object of class 'trial'
#'
#' @return A visual summary of the trial data
#' @export

data_plot_12 = function(trial) {
  y = trial$y
  x = trial$x
  nt = nrow(x)
  treat = 1:nt
  df = data.frame(cbind(y, t(x)%*%treat, 1:length(y)))
  names(df) = c('response', 'treatment', 'patient')
  df$response[df$response == 0] = 'event'
  df$response[df$response == 1] = 'safe'
  df$treatment[df$treatment == 1] = '1:placebo'
  df$treatment[df$treatment == 2] = '2:L.Plant'
  df$treatment[df$treatment == 3] = '3:B.Inf(21d)'
  df$treatment[df$treatment == 4] = '4:B.Inf(7d)'
  df$treatment = as.factor(df$treatment)
  if (all(y %in% c(0,1))) df$response = as.factor(df$response)
  p = ggplot(df, aes(x = patient, y = response, color = response)) +
    geom_point(size = 3) + facet_grid(treatment ~ .) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          strip.text.y = element_text(size = 8))
  return(p)
}




estPlot = function(trial) {
  est = trial$est
  nt = nrow(est)
  treat = 1:nt
  est = data.frame(est, treatment = treat)
  est = est[rev(order(est$treatment)),]
  est$treatment = factor(est$treatment, levels = est$treatment[rev(order(est$treatment))])
  p = ggplot() + geom_linerange(data=est, mapping=aes(x=as.factor(treatment), ymin=low, ymax=up), size=1, color="darkblue") +
    geom_point(data=est, mapping=aes(x=as.factor(treatment), y=p.est), size=4, shape=21, fill="#33FFFF", color = 'darkgrey') +
    coord_flip() + ylab('estimate') + xlab('treatment') + theme(axis.text=element_text(size=12),
                                                              axis.title=element_text(size=14,face="bold"),
                                                              strip.text.y = element_text(size = 12, face = "bold"))
  return(p)
}
