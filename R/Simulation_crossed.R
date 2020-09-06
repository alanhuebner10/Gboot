# library(foreach)
# library(parallel)
# library(doParallel)
#
# # Create two-facet crossed simulated data set called sim_data persons x items x raters (in
# # Brennan and Tong 2004, r = h) set sample sizes
# np <- 100
# ni <- 20
# nr <- 2
# # set true parameter values
# sig_p <- 4
# sig_i <- 2
# sig_r <- 1
# sig_pi <- 8
# sig_pr <- sqrt(2)
# sig_ir <- sqrt(3)
# sig_pir <- 12
# #############
#
# # Construct design matrix (only needs to be done once)
# sim_data <- data.frame(matrix(0, np * ni * nr, ncol = 4))
# colnames(sim_data) <- c("Person", "Item", "Rater", "Score")
# sim_data$Person <- rep(1:np, each = ni * nr)
# sim_data$Item <- rep(1:ni, each = nr, times = np)
# sim_data$Rater <- rep(1:nr, times = np * ni)
#
# results_all <- NULL
# registerDoParallel(cores = 4)
#
#
# #specify the desired number of simulations
# x = 50
# ptm <- proc.time()
# r <- foreach(icount(x), .combine = rbind) %dopar% {
#   # Create random effects (generate new for each replication)
#   Zp <- rnorm(np, 0, 1)
#   Zi <- rnorm(ni, 0, 1)
#   Zr <- rnorm(nr, 0, 1)
#   Zpi <- matrix(rnorm(np * ni, 0, 1), nrow = np, ncol = ni)
#   Zpr <- matrix(rnorm(np * nr, 0, 1), nrow = np, ncol = nr)
#   Zir <- matrix(rnorm(ni * nr, 0, 1), nrow = ni, ncol = nr)
#   # Simulate Scores
#   score_index <- 1
#   for (p in 1:np) {
#     for (i in 1:ni) {
#       for (r in 1:nr) {
#         Score_pir <- sig_p * Zp[p] + sig_i * Zi[i] + sig_r * Zr[r] + sig_pi * Zpi[p, i] +
#           sig_pr * Zpr[p, r] + sig_ir * Zir[i, r] + sig_pir * rnorm(1)
#         sim_data$Score[score_index] <- Score_pir
#         score_index <- score_index + 1
#       }
#     }
#   }
#   result <- c(summaryCI(CalcGTheoryCI(sim_data, 100, type = "p"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "i"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "o"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "pi"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "po"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "io"), 0.8, 4)$Gstudy_Estimates, summaryCI(CalcGTheoryCI(sim_data, 100, type = "pio"), 0.8, 4)$Gstudy_Estimates)
#   result
# }
# results_all <- rbind(results_all, r)
# final <- colMeans(results_all)
# sim_result_mean <- rbind(c(final[1:7]),c(final[15:21]),c(final[29:35]),c(final[43:49]),c(final[57:63]),c(final[71:77]),c(final[85:91]))
# sim_result_SE <- rbind(c(final[8:14]),c(final[22:28]),c(final[36:42]),c(final[50:56]),c(final[64:70]),c(final[78:84]),c(final[92:98]))
#
# colnames(sim_result_mean) <- c("p Var", "i Var", "o Var", "pi Var", "po Var",
#                           "io Var", "ResidVar")
# colnames(sim_result_SE) <- c("p Var_SE", "i Var_SE", "o Var_SE", "pi Var_SE",
# "po Var_SE", "io Var_SE", "ResidVar_SE")
# rownames(sim_result_mean) <- c("boot-p","boot-i","boot-o","boot-p,i","boot-p,o","boot-i,o","boot-p,i,o")
# rownames(sim_result_SE) <- c("boot-p","boot-i","boot-o","boot-p,i","boot-p,o","boot-i,o","boot-p,i,o")
# list(Mean_Estimates = sim_result_mean, SE_Estimates = sim_result_SE)
# proc.time() - ptm
