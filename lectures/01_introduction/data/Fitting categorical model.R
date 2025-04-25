# Fitting example intro lecture 

load("~/surfdrive/Presentations/2024/fitting mHMM examples /MHMM_nonverbal_DM.Rda")

MHMM_nonverbal_DM

## specifying general model properties:
m <- 3     # number of hidden states
n_dep <- 6     # number of dependent input variables
q_emiss <- c(3, 2, 2, 3, 2, 2)   # number of categories within each dep var

# specifying starting values
start_TM <- diag(.9, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .05
start_EM <- list(matrix(c(0.70, 0.20, 0.10, # vocalizing therapist
                          0.05, 0.90, 0.05,
                          0.25, 0.70, 0.05), byrow = TRUE, nrow = m, ncol = q_emiss[1]), 
                 matrix(c(0.15, 0.85, # looking therapist
                          0.35, 0.65,
                          0.30, 0.70), byrow = TRUE, nrow = m, ncol = q_emiss[2]), 
                 matrix(c(0.80, 0.20, # leg therapist
                          0.80, 0.20,
                          0.80, 0.20), byrow = TRUE, nrow = m, ncol = q_emiss[3]), 
                 matrix(c( 0.10, 0.80, 0.10, # vocalizing patient
                           0.90, 0.05, 0.05,
                           0.85, 0.05, 0.10), byrow = TRUE, nrow = m, ncol = q_emiss[4]), 
                 matrix(c(0.30, 0.70, # looking patient
                          0.10, 0.90,
                          0.50, 0.50), byrow = TRUE, nrow = m, ncol = q_emiss[5]), 
                 matrix(c(0.70, 0.30, # leg patient
                          0.90, 0.10,
                          0.35, 0.65), byrow = TRUE, nrow = m, ncol = q_emiss[6])) 

J <- 5000
burn_in <- 2000
out_3st <- mHMM(s_data = MHMM_nonverbal_DM,
                data_distr = "categorical",
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = J, burn_in = burn_in))


saveRDS(out_3st, file = "out_vll_3st.rds")
