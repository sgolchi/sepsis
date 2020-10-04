#' Main simulation function 2-arm
#'
#' Returns simulation results for one iteration
#'
#' @param i simulation iteration index
#' @param cRates vector of control risks
#' @param RRR vector of relative risk reductions - same size as the number of interventions
#' @param nb batch size; interim analysis is done at every \code{nb}patients
#' @param maxN maximum sample size; trial stops when \code{maxN} is reached
#' @param N size of posterior sample used 
#' @param upper1 superiority threshold for LP
#' @param upper2 superiority threshold for BI
#' @param lower futility probability threshold 
#' @param futb futility effect threshold (MID)
#' @param padhere vector of probabilities of adherence for each arm
#' @param adapt logical, indicating if allocation probabilities are adapted
#' @param control_sup logical, indicating if pairwise comparison with control is employed. If FALSE all arms are compared simultaneously
#' @param dos the outcome wrt which superiority decision is made taking values 1(s), 2(p1), or 3(p2)
#' @param dof the outcome wrt which futility decision is made taking values 1(s), 2(p1), or 3(p2)
#' @return a list of simulation outputs including \code{Nt} number of arms, 
#'         \code{early} probability of stopping early, \code{events} propostion of sepsis cases,
#'         \code{supstop_lp} probability of stopping due to superiority of LP,
#'         \code{supstop_bi} probability of stopping due to superiority of BI,
#'         \code{futstop_lp} probability of stopping due to futility of LP,
#'         \code{futstop_bi} probability of stopping due to futility of BI,
#'         \code{reachmax} probability of reaching max sample size,
#'         \code{RRR_hat} relative risk reduction estimates (posterior means),
#'         \code{pvalue_lp} p-values for LP, \code{pbvalue_bi} p-values for BI
#' @export
scn_2arm = function(i, cRates = c(0.02, 0.035, .05), RRR = .4, nb = 1000, maxN = 12000, 
               N = 1000, upper = 0.975, lower = .01, futb = 0.4, padhere = rep(1,nt), adapt = F, control_sup = T,
               dos = c(1), dof = c(1,3)) {
  cr = cRates
  cr[2] = (cRates[2] - cRates[1])/(1 - cRates[1])
  cr[3] = (cRates[3] - cRates[2])/(1 - cRates[2])
  #RRR[2] = RRR[1] - RRR[2]
  theta0 = 1 - cbind(cr, (1 - RRR) * cRates)
  nt = ncol(theta0)
  no = nrow(theta0)
  con = 1
  j = 0
  x = array(0, dim = c(nt, 1))
  x0 = array(0, dim = c(nt, 1))
  y = array(0, dim = c(no, 1))
  y1 = array(0, dim = c(no, 1))
  theta = array(rnorm(N*nt*no, 0, 10), dim = c(N, nt, no,  1))
  post0 = array(1, dim = c(2, nt, no))
  p_new = apply(post0, c(2,3), function(x) rbeta(N, x[1], x[2]))
  theta = array(- log((1 - p_new) / p_new), dim = c(N, nt, no, 1))
  psup1 = array(rep(1/nt,nt*no), dim = c(nt, no, 1))
  prand = c(1,1)
  supstop = 0
  futstop = 0
  reachmax = 0
  repeat {
    j = j + 1
    xb = rmultinom(nb, 1, prob = prand)
    xb0 = xb
    for (k in 1:nb) {
      xb0[which(xb0[,k] == 1), k] = rbinom(1, 1, 1)
    }
    yb = apply(theta0 %*% xb0, c(1,2), function(z) rbinom(1, 1, prob = z))
    yb[2,] = yb[2,]*yb[1,]
    yb[3,] = yb[3,]*yb[2,] 
    x = abind(x, xb, along = 2)
    x0 = abind(x0, xb0, along = 2)
    y = abind(y, yb, along = 2)
    yb1 = t(yb)    #???
    y1 =  c(y1, yb1)
    #post0 = post_beta(yb, xb, post0)
    for (i in 1:3) {
      post0[,,i] = post_beta(yb1[,i], xb, post0[,,i])
    }
    #post0 = post_beta(yb1, xb, post0)
    p_new = apply(post0, c(1,3), function(x) rbeta(N, x[1], x[2]))
    RRR_new =  1 - (1-p_new[,2,])/(1-p_new[,1,])
    theta_new = - log((1 - p_new) / p_new)
    theta = abind(theta, theta_new, along = 4)
    check = apply(theta_new, c(1,3), control_sup_check)
    psup1 = abind(psup1, apply(check, c(1,3), mean), along = 3) 
    fut = NULL
    for (i in dof) fut = c(fut, sum(RRR_new[,i]<futb)/nrow(RRR_new))
    if (max(psup1[,dos,j+1]) > upper) {
      supstop = 1 
      break
    }
    if (sum(fut > 0.975) == length(fut)) {
      futstop = 1
      break
    }
    if (ncol(y) >= maxN) {
      reachmax = 1
      break
    }
  }
  y = y[,-1]
  x = x[,-1]
  psup_last = psup1[,,j+1]
  pbin = ifelse(psup_last > upper, 1, 0)
  pow = apply(pbin, 2, function(z) z[2] ==1)
  early = ncol(y)<maxN
  fail = apply(y, 1, function(z) sum(z == 0)/length(z))
  RRR_hat = apply(RRR_new,2,mean)
  counts = x%*%t(y)
  pval = NULL
  for (i in 1:3) {
    bb = cbind(counts[,i], rowSums(x) - counts[,i])
    ft = fisher.test(bb, alternative = 'less')
    pval = c(pval, ft$p.value)
  }
  #out = list(psup = psup1, psup2 = psup2, pinf = pinf, theta = theta, est = est, y = y, x = x[,-1], x0 = x0[,-1])
  out = data.frame(pow1 = pow[1], pow2 = pow[2], pow3 = pow[3], Nt = ncol(y), early = early, event1 = fail[1], 
                   event2 = fail[2], event3 = fail[3], supstop = supstop, futstop = futstop, 
                   reachmax = reachmax, RRR_hat1 = RRR_hat[1], RRR_hat2 = RRR_hat[2], RRR_hat3 = RRR_hat[3], 
                   pvalue1 = pval[1], pvalue2 = pval[2], pvalue3 = pval[3])
  #class(out) = 'trial'
  return(out)
}


simScn_2arm = function(s, p1, p2, RRR, upper, futb, M = 500, dos = 2, dof = 2) {
  cRates = c(s, p1, p2)
  df = data.frame(t(sapply(1:M, scn_2arm, cRates = cRates, RRR = RRR, upper = upper, futb = futb, dos = dos,
                           simplify = T))) 
  df = unlist_df(df)
  # out = data.frame(power1 = mean(unlist(df$pow1)),
  #                  power2 = mean(unlist(df$pow2)),
  #                  power3 = mean(unlist(df$pow3)),
  #                  pearly = mean(unlist(df$early)),
  #                  Nt = mean(as.numeric(df$Nt)),
  #                  supstop = mean(unlist(df$supstop)), 
  #                  futstop = mean(unlist(df$futstop)), 
  #                  reachmax = mean(unlist(df$reachmax)), 
  #                  E.event.rate1 = mean(unlist(df$event1)),
  #                  E.event.rate2 = mean(unlist(df$event2)),
  #                  E.event.rate3 = mean(unlist(df$event3)),
  #                  RRRhat1 = mean(unlist(df$RRR_hat1)),
  #                  RRRhat2 = mean(unlist(df$RRR_hat2)),
  #                  RRRhat3 = mean(unlist(df$RRR_hat3)),
  #                  pvalue1 = mean(unlist(df$pvalue1)),
  #                  pvalue2 = mean(unlist(df$pvalue2)),
  #                  pvalue3 = mean(unlist(df$pvalue3))
  #                  )
  return(df)
}


sim_exe_2arm = function(cases, M, filename, ...) {
  z = list(...)
  if (!is.null(z$dos)) dos = z$dos
  if (!is.null(z$dof)) dos = z$dof
  cc = cases
  tt = mapply(simScn_2arm, s = cc$s, p1 = cc$p1, p2 = cc$p2, RRR = cc$RRR, upper = cc$sup, 
              futb = cc$fut, dos = dos, M = M)
  dftt = cbind(unlist_df(t(tt)), cases[rep(1:nrow(cases), each = M),])
  save(dftt, file = filename)
}
