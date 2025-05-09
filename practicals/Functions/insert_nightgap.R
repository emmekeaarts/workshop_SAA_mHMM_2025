insert_nightgap <- function(data, # data frame. First column should be the subject ID, second column should be the day.
                            beeps_per_night,
                            ndays # vector of length n_subj. Number of days for each subject. Can also be a single scalar if constant over subjects
                            ){
  cols_data <- colnames(data)
  ncol_data <- ncol(data)
  # ----- Insert them in the Data -----
  # Unique subjects
  u_subjid <- sort(unique(data[,1]), decreasing = FALSE)
  n_subj <- length(u_subjid)
  if(length(ndays) == 1){
    ndays <- rep(ndays, n_subj)
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
  colnames(out) <- cols_data
  return(out)
}