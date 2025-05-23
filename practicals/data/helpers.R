insert_nightgap <- function(data, # data frame. First column should be the subject ID, second column should be the day.
                            beeps_per_night,
                            col_id,
                            col_day
){
  cols_data_input <- colnames(data)
  ncol_data <- ncol(data)
  data <- cbind(data[,c(col_id, col_day)], data[,-c(col_id, col_day)])
  cols_data_reordered <- colnames(data)
  # ----- Insert them in the Data -----
  # Unique subjects
  u_subjid <- sort(unique(data[,1]), decreasing = FALSE)
  n_subj <- length(u_subjid)
  ndays <- c()
  for(i in u_subjid){
    ndays <- c(ndays, length(unique(data[data[,1] == i,2])))
  }
  # Storage
  l_data_wN <- list()
  
  # Loop over subjects
  for(i in 1:n_subj) {
    # Subset
    data_i <- as.matrix(data[data[,1]==u_subjid[i], ])
    
    # Define Night block
    night_block <- matrix(NA, beeps_per_night, ncol_data)
    night_block[, 1] <- u_subjid[i]
    
    # Split by day (easy, because we have the full rows for all individuals)
    l_data_i_wN <- list()
    for(day in 1:(ndays[i]-1)) {
      night_block[, 2] <- day
      l_data_i_wN[[day]] <- rbind(data_i[data_i[, 2]==day, ],
                                  night_block)
    }
    l_data_i_wN[[ndays[i]]] <- data_i[data_i[, 2]==ndays[i], ]
    # Combine
    data_i_wN <- do.call(rbind, l_data_i_wN)
    l_data_wN[[i]] <- data_i_wN
  } # end loop
  
  out <- as.data.frame(do.call(rbind, l_data_wN))
  rownames(out) <- NULL
  colnames(out) <- cols_data_reordered
  out <- out[,cols_data_input]
  return(out)
}

# Plot emission densities
plot_dens <- function(model, range_plot, steps = 0.5, level = "group", subject = NULL){
  range_plot <- c(0, 100)
  steps <- 0.5
  model <- out_2st_emotion
  dep_labels <- model$input$dep_labels
  emiss <- mHMMbayes::obtain_emiss(model, level = level)
  if(level == 'subject'){
    emiss <- lapply(emiss, function(x) x[[subject]])
  }
  n_dep <- model$input$n_dep
  m <- model$input$m
  
  emiss_long <- lapply(emiss, function(x) {
    x <- as.data.frame(x)
    x$State <- rownames(x)
    rownames(x) <- NULL
    return(x)
  }) %>%
    dplyr::bind_rows(.id = "Dep") %>%
    dplyr::mutate(Dep = factor(Dep, levels = dep_labels))
  sequence <- seq(from = range_plot[1], to = range_plot[2], by = steps)
  curve_data <- emiss_long %>%
    dplyr::group_by(Dep, State) %>%
    dplyr::do({
      mean <- .data$Mean
      sd <- .data$SD
      y <- dnorm(sequence, mean, sd)
      data.frame(x = sequence, y = y)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Dep = rep(emiss_long$Dep, each = length(sequence)),
                  State = rep(emiss_long$State, each = length(sequence)))
  gg <- ggplot2::ggplot(curve_data, aes(x = x, y = y, color = factor(State))) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(data = emiss_long, aes(xintercept = Mean, colour = as.factor(State)), linetype = 'dashed', linewidth = 0.6) +
    ggplot2::facet_wrap(~ Dep) +
    ggplot2::labs(color = "State", x = "Value", y = "Prob") +
    ggplot2::theme_bw()
  return(gg)
}

# --------------------------------------------------------
# ---------- Compute Gelman-Ruben Statistic --------------
# --------------------------------------------------------

f_GR <- function(model_list, digits=2, burnin=NULL) {
  
  # Get m
  nChain <- length(model_list) # number of Chains
  m <- model_list[[1]]$input$m
  if(is.null(burnin)) {
    burnin <- model_list[[1]]$input$burn_in
  }
  
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

## Function to detect label switching using trace plots
plot_label_switching <- function(model, # output of `mHMM()`
                                 subject, # vector of subjects to plot
                                 vrb = NULL # optional vector of variable names
){
  dep_labels <- model$input$dep_labels
  if(is.null(vrb)){
    vrb <- dep_labels
  }
  dep_index <- which(dep_labels %in% vrb)
  m <- model$input$m
  n_dep <- model$input$n_dep
  mcmc_samps <- lapply(model$PD_subj[subject], "[[", "cont_emiss")
  mcmc_samps <- lapply(mcmc_samps, as.data.frame) %>%
    dplyr::bind_rows(.id = "subj") %>%
    dplyr::mutate(subj = factor(.data$subj, labels = subject)) %>%
    dplyr::group_by(.data$subj) %>%
    dplyr::mutate(iter = 1:(dplyr::n())) %>%
    dplyr::ungroup()
  mcmc_long <- mcmc_samps %>%
    dplyr::select(subj, iter, ends_with("mu")) %>%
    tidyr::pivot_longer(-c(subj, iter),
                        names_to = c("dep", "state"),
                        values_to = c("mu"),
                        names_pattern = "dep(\\d+)_S(\\d+)_mu") %>%
    dplyr::filter(dep %in% dep_index)
  gg <- mcmc_long %>%
    dplyr::mutate(dep = factor(.data$dep, levels = 1:n_dep, labels = dep_labels),
                  state = factor(.data$state, levels = 1:m, labels = paste("State", 1:m))) %>%
    ggplot2::ggplot(ggplot2::aes(x = iter, y = mu, color = state)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(cols = vars(.data$dep), rows = vars(.data$subj)) +
    ggplot2::scale_color_manual(values = c("#E69F00", "#0072B2", "#CC79A7")) +
    ggplot2::theme_bw()
  return(gg)
}

# --------------------------------------------------------
# ---------- Extract Transition Estimates & Plot ---------
# --------------------------------------------------------

PlotTrans <- function(model,
                      subject = NULL) # number of random effects shown in the figure
  
{
  n_dep <- model$input$n_dep
  N <- model$input$n_subj
  m <- model$input$m
  if(is.null(subject)) {
    subject <- 1:N
  }
  # ------ Getting the Parameters ------
  ## Fixed Effects
  gamma_group <- obtain_gamma(model)
  ## Random Effects
  gamma_subj <- obtain_gamma(model, level = "subject")
  a_gamma <- array(NA, dim=c(m, m, N))
  for(i in 1:N) a_gamma[, , i] <- gamma_subj[[i]]
  
  # ------ Plotting ------
  
  ### Right Panel: Barplot with Random effects
  # Generate x_labels
  v_labels <- rep(NA, m^2)
  cnt <- 1
  for(m1 in 1:m) for(m2 in 1:m) {
    v_labels[cnt] <- paste0("S", m1, " to ", "S", m2)
    cnt <- cnt+1
  }
  
  # Barplot
  par(mar=c(4.5,3,3,1))
  bp <- barplot(as.numeric(t(gamma_group)),
                ylim=c(0, 1),
                # names.arg = v_labels,
                cex.names=0.7, axes=FALSE)
  axis(1, v_labels, las=2, at=bp)
  axis(2, las=2)
  
  # Add Random Effects
  cnt <- 1
  for(m1 in 1:m) {
    for(m2 in 1:m) {
      points(rep(bp[cnt], length(subject)), a_gamma[m1, m2, subject],
             pch=21, col="black",, cex=0.75)
      cnt <- cnt + 1
    }
  }
} # eoF

# --------------------------------------------------------
# ---------- Functions to Compute Pseudo-Residuals -------
# --------------------------------------------------------

GetResid <- function(data, # empirical data
                     model, # fitted mHMM model object
                     vrb, # Desired variable
                     subject # Desired subject
) {
  
  # Aux
  v_subj_id <- unique(data$subj_id)
  labels <- model$input$dep_labels
  j <- which(labels == vrb)
  i <- subject
  # --- Emission Distributions -S--
  emiss_subject <- obtain_emiss(model, level = "subject") # For all
  mu_ji <- emiss_subject[[labels[j]]][[i]][, 1] # For specified variable & subject; col=1 for means
  
  # Get most likely state sequence for each subject
  if(model$input$m > 1) {
    model_m_stateSeq <- suppressMessages(vit_mHMM(model, emotion_mHMM))
    State_i <- model_m_stateSeq$state[model_m_stateSeq$subj==v_subj_id[i]] # state seq for person i
  } else {
    State_i <- rep(1, 240+40*8) # if m=1, we know that state is 1 always
  }
  
  # Compute predictions for fixed j & i
  Pred_ji <- as.numeric(mu_ji[State_i])
  
  # Get Empirical data
  X_ji <- data[[labels[j]]][data$subj_id==v_subj_id[i]]
  
  # Compute residual
  res_ji <- X_ji - Pred_ji
  
  # Compute MSE
  RMSE_ji <- sqrt(mean(na.omit(res_ji)^2))
  
  # Return
  outlist <- list("emp" = X_ji,
                  "model" = Pred_ji,
                  "resid" = res_ji,
                  "RMSE" = RMSE_ji)
  return(outlist)
  
} # eoF

# --------------------------------------------------------
# ---------- Functions Visualize Pseudo-Residuals --------
# --------------------------------------------------------

# serves as input for PlotRes (i.e. the right panel)
PlotRes_right <- function(res_ji, layout=TRUE) {
  # Compute AR
  ar_ji <- round(acf(res_ji$resid, plot = FALSE, na.action = na.pass)$acf[2], 3)
  # Fit linear trend
  time <- 1:length(res_ji$resid)
  trend_model <- lm(res_ji$resid ~ time)
  trend_sum <- summary(trend_model)
  trend_coef <- round(trend_sum$coefficients[2, 1], 3)
  trend_pval <- round(trend_sum$coefficients[2, 4], 3)
  
  # Layout
  if(layout) layout(matrix(1:2, ncol=2), widths = c(1, .5))
  # LinePlot
  par(mar=c(4,3,2,1))
  plot.new()
  plot.window(xlim=c(1, 560), ylim=c(-110, 110))
  axis(1)
  axis(2, las=2)
  abline(h=0, lty=1, col="lightgrey", lwd=2)
  points(res_ji$resid, pch=20)
  abline(trend_model, lwd=2, col="steelblue")
  text(5,-70, paste0("Lin Trend: Slope = ", trend_coef, "; pval = ", trend_pval), col="steelblue", adj=0)
  text(5,-90, paste0("Lag-1 AR = ", ar_ji), col="tomato", adj=0)
  text(4, 80, paste0("RMSE = ", round(res_ji$RMSE, 2)), adj=0)
  # Marginal
  par(mar=c(4,0,2,1))
  hist_data <- hist(res_ji$resid, plot = FALSE, breaks=seq(-110, 110, length=20))
  barplot(hist_data$counts,
          horiz = TRUE,  # Horizontal bars
          names.arg = NULL,
          axes=FALSE)
  x_seq <- seq(-100, 100, length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(res_ji$resid, na.rm = TRUE),
                     sd = sd(res_ji$resid, na.rm = TRUE))
  scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
  lines(scaled_den, seq(0, 24, length=1000), col="grey") # Not entiresure where those 22 come from
  
} # eoF

# --------------------------------------------------------
# ---------- Plotting Labels in into Canvas --------------
# --------------------------------------------------------

# serves as input for PlotRes
plotLabel <- function(text, cex=1.4, srt=0, xpos=0.5) {
  par(mar=rep(0,4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  text(xpos, 0.5, text, cex=cex, srt=srt)
}

# Create plot for pseudoresiduals
PlotRes <- function(model,
                    data,
                    vrb,
                    subject){
  dep_labels <- model$input$dep_labels
  nplot <- length(subject)
  vrb_index <- which(dep_labels == vrb)
  lmat <- matrix((nplot+3):(4*nplot+2), nplot, 3, byrow = TRUE)
  lmat <- rbind(c(1:2, 0), lmat)
  lmat <- cbind(c(0, 3:(nplot+2)), lmat)
  lo <- layout(mat = lmat, widths = c(0.15, 1, 1, .3), heights = c(0.15, rep(1, (nplot+1))))
  # layout.show(lo)
  n_subj <- length(subject)
  # Plot Labels
  plotLabel("Data + Predictions", cex=1.6)
  plotLabel("Pseudo Residuals", cex=1.6)
  for(s in subject) plotLabel(paste0("Subject ", s), srt=90)
  
  for(s in subject) {
    
    # Get predictions, residuals
    res_1i <- GetResid(data = data, 
                       model = model,
                       vrb = vrb, # Variable
                       subject = s) # This function is in 0_Helpers.R
    
    # Plot data + Predictions
    par(mar=c(4,2,1,1))
    plot.new()
    plot.window(xlim=c(0, 600), ylim=c(0,100))
    axis(1)
    axis(2, las=2)
    lines(res_1i$emp, lwd=1.5)
    lines(res_1i$model, col="orange", lty=2, lwd=1.5)
    if(s==subject[1]) legend("bottomright", legend=c(paste0("Data ", "'", vrb, "'"), "Prediction"),
                             text.col=c("black", "orange"), lty=1:2, col=c("black", "orange"),
                             bty="n")
    
    # Plot residuals
    PlotRes_right(res_1i,
                  layout = FALSE) # This function is in 0_Helpers.R
    
  } # end for: subj
}