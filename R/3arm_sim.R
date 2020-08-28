#' Main simulation function
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
#' @return a list of simulation outputs including \code{Nt} sample size at trial termination,
#'         \code{early} probability of stopping early, \code{events} propostion of sepsis cases,
#'         \code{supstop_lp} probability of stopping due to superiority of LP,
#'         \code{supstop_bi} probability of stopping due to superiority of BI,
#'         \code{futstop_lp} probability of stopping due to futility of LP,
#'         \code{futstop_bi} probability of stopping due to futility of BI,
#'         \code{reachmax} probability of reaching max sample size,
#'         \code{RRR_hat} relative risk reduction estimates (posterior means),
#'         \code{pvalue_lp} p-values for LP, \code{pbvalue_bi} p-values for BI
#' @export
scn = function(i, cRates = c(0.02, 0.035, .05), RRR = c(.4, .3), nb = 1000, maxN = 18000,
               N = 1000, upper1 = 0.99, upper2 = 0.995, lower = .01, futb = 0.2, padhere = rep(1,nt), adapt = F, control_sup = T,
               dos = 2, dof = 2) {
  cr = cRates
  cr[2] = (cRates[2] - cRates[1])/(1 - cRates[1])
  cr[3] = (cRates[3] - cRates[2])/(1 - cRates[2])
  theta0 = 1 - cbind(cr, outer(cRates, (1 - RRR)))
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
  prand = c(1,1,1)
  supstop_lp = 0
  futstop_lp = 0
  supstop_bi = 0
  futstop_bi = 0
  reachmax = 0
  armsdropped = 0
  repeat {
    j = j + 1
    xb = rmultinom(nb, 1, prob = prand)
    xb0 = xb
    for (k in 1:nb) {
      xb0[which(xb0[,k] == 1), k] = rbinom(1, 1, padhere[which(xb0[,k] == 1)])
    }
    yb = t(apply(theta0 %*% xb0, c(1,2), function(z) rbinom(1, 1, prob = z)))
    yb1 = yb
    yb[,2] = yb[,2]*yb[,1]
    yb[,3] = yb[,3]*yb[,2]
    x = abind(x, xb, along = 2)
    x0 = abind(x0, xb0, along = 2)
    y = abind(y, t(yb), along = 2)
    y1 =  c(y1, yb1)
    for (i in 1:3) {
      post0[,,i] = t(post_beta(yb[,i], xb, t(post0[,,i])))
    }
    p_new = apply(post0, c(2,3), function(x) rbeta(N, x[1], x[2]))
    RRR_new = abind(1 - (1-p_new[,2,])/(1-p_new[,1,]), 1 - (1-p_new[,3,])/(1-p_new[,1,]), along = 3)
    theta_new = - log((1 - p_new) / p_new)
    theta = abind(theta, theta_new, along = 4)
    check = apply(theta_new, c(1,3), control_sup_check)
    psup1 = abind(psup1, apply(check, c(1,3), mean), along = 3)
    fut = apply(RRR_new[,dof,]<futb, 2, sum)/nrow(RRR_new[,dof,])
    if (psup1[2,dos,j+1] > upper1) {
      supstop_lp = j+1
      prand[2] = 0
    }
    if (psup1[3,dos,j+1] > upper2) {
      supstop_bi = j+1
      prand[3] = 0
    }
    if (fut[1] > 0.99) {
      futstop_lp = j+1
      prand[2] = 0
    }
    if (fut[2] > 0.99) {
      futstop_bi = j+1
      prand[3] = 0
    }
    if (ncol(y) >= maxN) {
      reachmax = 1
      break
    }
    if (sum(prand>0) <= 1) {
      armsdropped = 1
      break
    }
  }
  y = y[,-1]
  x = x[,-1]
  early = 1 - reachmax
  fail = apply(y, 1, function(z) sum(z == 0)/length(z))
  RRR_hat = apply(RRR_new,c(2,3),mean)
  counts = x%*%t(y)
  pval1 = NULL
  for (i in 1:3) {
    bb = cbind(counts[1:2,i], rowSums(x)[1:2] - counts[1:2,i])
    ft = fisher.test(bb, alternative = 'less')
    pval1 = c(pval1, ft$p.value)
  }
  pval2 = NULL
  for (i in 1:3) {
    bb = cbind(counts[c(1,3),i], rowSums(x)[c(1,3)] - counts[c(1,3),i])
    ft = fisher.test(bb, alternative = 'less')
    pval2 = c(pval2, ft$p.value)
  }
  out = list(Nt = ncol(y), early = early, events = fail, supstop_lp = supstop_lp,
             supstop_bi = supstop_bi, futstop_lp = futstop_lp, futstop_bi = futstop_bi,
             reachmax = reachmax, RRR_hat = RRR_hat, pvalue_lp = pval1, pvalue_bi = pval2)
  #class(out) = 'trial'
  return(out)
}

#' Wrapper for scn
#'
#' returns simulation results for \code{M} iterations
#'
#' @param s ctrl risk for s
#' @param p1 ctrl risk for p1
#' @param p2 ctrl risk for p2
#' @param RRR1 relative risk reduction for LP
#' @param RRR2 relative risk reduction for BI
#' @param upper2 superiority threshold for BI
#' @param M number of simulation iterations
#' @return a data frame with the same output as \code{scn} for \code{M} iterations
#' @export
sim_scn = function(s, p1, p2, RRR1, RRR2, upper2, M = 100) {
  cRates = c(s, p1, p2)
  RRR = c(RRR1, RRR2)
  df = sapply(1:M, scn, cRates = cRates, RRR = RRR, upper2 = upper2, simplify = F)
  df = data.frame(t(sapply(df, unlist)))
  return(df)
}


#' Wrapper for \code{sim_scn}
#'
#' applies \code{sim_scn} to multiple simulation scenarios and saves results in home directory
#'
#' @param cases a data frame containing simulation scenarios
#' @param M number of simulation scenarios
#' @param filename file name under which the results are saved
#' @return saves results ot home directory
#' @export
sim_exe = function(cases, M, filename) {
  cc = cases
  tt = mapply(sim_scn, s = cc$s, p1 = cc$p1, p2 = cc$p2, RRR1 = cc$RRR1, RRR2 = cc$RRR2,
              upper2 = cc$upper2, M = M)
  dftt = cbind(unlist_df(t(tt)), cases[rep(1:nrow(cases), each = M),])
  save(dftt, file = filename)
}

####### definition of simulation scenarios and execution #################

# cRisk = cbind(s = c(.002, .01, .015), p1 = c(.02, .035, .05), p2 = c(.09, .12, .15))
# RRR = cbind(RRR1 = c(0,.2,.4,.6), RRR2 = c(0,.2,.4,.6))
# upper2 = c(0.995, 0.999)
# #exg = expand.grid(RRR = RRR, upper2 = upper2)
#
# cases = cbind(do.call(rbind, rep(list(cRisk), nrow(RRR))), RRR[rep(1:nrow(RRR), each = nrow(cRisk)),])
# cases = cbind(do.call(rbind, rep(list(cases), length(upper2))), upper2[rep(1:length(upper2), each = nrow(cases))])
# row.names(cases) = 1:nrow(cases)
# cases = data.frame(cases)
# names(cases)[6] = 'upper2'
#
# sim_exe(cases, M = 500, filename = '3arm.Rdata')

