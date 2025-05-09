# --------------------------------------------------------
# ---------- Extract Emission Estimates & Plot -----------
# --------------------------------------------------------

PlotEmiss <- function(model, parameter="Means") {
  
  # Basic info
  m <- model$input$m
  n_dep <- model$input$n_dep 
  N <- model$input$n_subj
  
  # Parameter indicator
  indpar <- ifelse(parameter=="Means", 1, 2)
  
  # ------ Getting the Parameters ------
  emiss_group <- obtain_emiss(model)
  labels <- names(emiss_group)
  
  ## Fixed effects
  m_fixed <- matrix(NA, n_dep, m)
  for(i in 1:n_dep) m_fixed[i, ] <- emiss_group[[i]][, indpar] # 1=mean; 2=SD; later make variable
  colnames(m_fixed) <- paste("K = ", 1:m)
  rownames(m_fixed) <- labels
  # Order effects
  ord <- order(m_fixed[1,], decreasing = TRUE)
  m_fixed_ord <- m_fixed[, ord]
  
  ## Random Effects
  emiss_subject <- obtain_emiss(model, level = "subject")
  a_REs <- array(NA, dim=c(n_dep, m, N))
  for(j in 1:N) for(i in 1:n_dep) a_REs[i, , j] <- emiss_subject[[i]][[j]][, indpar] # 1=mean; 2=SD; later make variable
  # Order REs
  a_REs_ord <- a_REs[, ord, ]
  
  # ------ Plotting ------
  
  ## Plotting basics
  library(RColorBrewer)
  cols <- brewer.pal(n_dep+1, "Set1")[-6]
  
  par(mar=c(3,3,2,1))
  old_par <- par(no.readonly = TRUE)
  par(mgp = c(3, 0.25, 0)) # moving x-axis labels up a bit
  
  ## Get started
  bp <- barplot(t(m_fixed_ord),
                beside=TRUE,
                space = c(0.2, 1),
                ylim=c(0, 100),
                col = rep(cols, each=m),
                names.arg = rep(1:m, n_dep),
                cex.names=0.7, axes=FALSE)
  # Emotion labels
  em_m <- colMeans(bp)
  for(i in 1:n_dep) text(em_m[i], 98, labels[i], adj=0.5, col=cols[i], cex=0.85)
  # Restore old par
  # Axes/labels/etc
  title(xlab="States", line=1.5)
  
  ## Add random effects
  # REs are available only for means
  if(indpar==1) {
    
    N_disp <- N #N
    for(i in 1:n_dep) {
      # Plot lines between dots
      for(s in 1:(m-1)) {
        segments(x0 = bp[s, i],
                 y0 = a_REs_ord[i, s, 1:N_disp],
                 x1 = bp[s+1, i],
                 y1 = a_REs_ord[i, s+1, 1:N_disp], col=alpha("grey", alpha=0.75), lwd=0.5)
      }
      
      # Plot those dots
      for(s in 1:m) points(rep(bp[s, i], N_disp), a_REs_ord[i, s, 1:N_disp],
                           col="black", bg=cols[i], pch=21, cex=0.5)
      
    } # end for: vars
  } # end if: means?
  
  par(old_par)
  axis(2, las=2, seq(0, 100, length=6), at=seq(0, 1, length=6))
}
