source("./R/sim_funs.R")
source("./R/sim_2arm.R")
library(abind)

cRisk <- cbind(s = c(.002, .01, .015), p1 = c(.02, .035, .05), p2 = c(.09, .12, .15))
RRR <- c(0, 0.1, 0.2, 0.4, 0.6)
fut <- c(0.1, 0.2, 0.3, 0.4, 0.6)
case_grid <- cbind(expand.grid(RRR = RRR, fut = fut), ntimes = nrow(cRisk))
case_grid <- as.data.frame(lapply(case_grid, rep, case_grid$ntimes))[-3]
cases <- cbind(case_grid, sup = 0.975, cRisk)

sim_exe_2arm(cases = cases, M = 500, filename = "./james_sims/sim_runs_nb_3000.Rdata", dos = 2, dof = 2, nb = 3000)
