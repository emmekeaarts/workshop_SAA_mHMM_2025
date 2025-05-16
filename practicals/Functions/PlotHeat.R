# --------------------------------------------------------
# ---------- Heatplot [for Transition Matrices] ----------
# --------------------------------------------------------

PlotHeat <- function(gamma,
                     main="",
                     labels=NULL,
                     las.x=1,
                     cex.axis=1.5,
                     cex.from = 1.5,
                     cex.to=1.5,
                     cex.val=1) {
  
  
  # Save the current par settings
  old_par <- par(no.readonly = TRUE)
  
  # -- Aux Variables --
  p <- ncol(gamma)
  
  # -- Make color gradient --
  color.gradient <- function(x, colors=c("#E41A1C", "white", "#377EB8"), colsteps=201) {
    return( grDevices::colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
  }
  x <- 1:201
  grad <- color.gradient(x)
  
  # Make canvas
  par(mar=c(2,2.5,2.5,1)*2)
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0, 1))
  
  # Auxiliary plotting variables
  sfm <- 1/(p*2)
  seq_mp_x <- seq(0, 1, length=p+1)[-(p+1)] + sfm
  
  # Plot Axes & Axis labels
  # xy_labels <- paste0("S ", 1:p)
  # Adjust mgp for custom axis labels
  par(mgp = c(3, 0.35, 0))  # Move tick labels closer to the plot
  
  y_labels <- sapply(p:1, function(i) as.expression(bquote(S[.(i)])))
  x_labels <- sapply(1:p, function(i) as.expression(bquote(S[.(i)])))
  
  axis(3, labels = x_labels, at=seq_mp_x, cex.axis=cex.axis, tick=FALSE)
  axis(2, labels = y_labels, at=seq_mp_x, las=2, cex.axis=cex.axis, tick=FALSE)
  title(main, font.main=1)
  
  title(ylab="From", cex.lab=cex.from, line=2)
  mtext("To", side=3, cex=cex.to, line=2)
  
  gamma_col <- matrix(NA, p, p)
  
  # Plot Data
  for(i in 1:p) {
    for(j in 1:p) {
      
      # Get color
      gamma_ij <- gamma[p:1, ][j, i]
      if(gamma_ij < -1) {
        col_ij <- grad[1]
      } else if(gamma_ij > 1 ) {
        col_ij <- grad[201]
      } else {
        col_ij <- grad[gamma[p:1, ][j, i] * 100 + 101]
        gamma_col[j,i] <- col_ij
      }
      
      # Plot box
      rect(xleft = seq_mp_x[i]-sfm,
           ybottom = seq_mp_x[j]-sfm,
           xright = seq_mp_x[i]+sfm,
           ytop = seq_mp_x[j]+sfm,
           col = col_ij)
      # Plot text
      text(seq_mp_x[i], seq_mp_x[j], round(gamma_ij , 2), cex=cex.val, col="black")
    }
  }
  
  # Reset par settings back to original
  par(mgp = c(3, 1, 0))
  
  
  # Return colors
  return(gamma_col)
  
} # eoF