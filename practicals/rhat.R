# --------------------------------------------------------
# ---------- Compute Gelman/Ruben (Rhat) Statistics ------
# --------------------------------------------------------

# ----- Compute GR Statistic -----
n_dep <- 8
l_RG <- list()

for(m in 1:6) {
  model_list <- list()
  for(chain in 1:Nchain) model_list[[chain]] <- l_Models[[chain]][[m]]
  l_RG[[m]] <- f_GR(model_list) # from: 0_Helpers.R
} # end for: m

# ----- Save -----
saveRDS(l_RG, file="Files/Convergence_GMStat_4chains.RDS")
l_RG <- readRDS(file="Files/Convergence_GMStat_4chains.RDS")

# ----- Summarize -----
# Get means per m and parameter type
RG_agg <- lapply(l_RG, function(x) round(c(mean(x[[1]]), mean(x[[2]])),2) )
m_RG_agg <- do.call(cbind, RG_agg)
colnames(m_RG_agg) <- paste0("M = ", 1:6)
rownames(m_RG_agg) <- c("Transition", "Emission")

xtab <- xtable(m_RG_agg, digits = rep(2, 7))
# Save LateX Table into folder "Files"
print(xtab, type = "latex", file = "Files/table_RG.tex", include.rownames = TRUE)

# --------------------------------------------------------
# ---------- Compute Gelman-Ruben Statistic --------------
# --------------------------------------------------------

f_GR <- function(model_list, digits=2) {
  burnin <- model_list[[1]]$input$burn_in
  # Get m
  nChain <- length(model_list) # number of Chains
  m <- model_list[[1]]$input$m
  n_dep <- model_list[[1]]$input$n_dep
  
  # Transition Matrix
  iter <- m*(m-1)
  v_RG_gamma <- rep(NA, m*(m-1))
  if(m>1) {
    for(j in 1:iter) {
      l_obj <- lapply(model_list, function(x) mcmc(x$gamma_int_bar[-(1:burnin), j]))
      mcmc_chains <- mcmc.list(l_obj) #make this a list input (must be possible)
      gelman_rubin_stat <- gelman.diag(mcmc_chains)
      v_RG_gamma[j] <- gelman_rubin_stat$psrf[1,1]
    }
  }
  m_RG_gamma <- matrix(v_RG_gamma, m, m-1, byrow = TRUE)
  
  # Emission Distribution
  RG_emiss <- matrix(NA, n_dep, m)
  for(p in 1:n_dep) {
    for(ms in 1:m) {
      l_obj <- lapply(model_list, function(x) mcmc(x$emiss_mu_bar[[p]][-(1:burnin), ms]))
      mcmc_chains <- mcmc.list(l_obj)
      gelman_rubin_stat <- gelman.diag(mcmc_chains)
      RG_emiss[p, ms] <- gelman_rubin_stat$psrf[1,1]
    }
  }
  
  # Return
  outlist <- list("m_RG_gamma" = round(m_RG_gamma, digits=digits),
                  "RG_emiss" = round(RG_emiss, digits=digits))
  return(outlist)
  
} # eoF
