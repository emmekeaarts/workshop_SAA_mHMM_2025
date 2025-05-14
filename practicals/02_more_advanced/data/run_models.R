library(mHMMbayes)
getwd()
source('practicals/Functions/insert_nightgap.R')
emotion_data <- readRDS("practicals/01_introduction/data/data_Rowland2020.rds")
emotion_data <- emotion_data[,c('subj_id', 'dayno', 'beep','happy','relaxed','anxious','depressed')]
emotion_nightgap <- insert_nightgap(data = emotion_data, # make sure the first column contains the subject IDs, and the second column contains the day numbers
                                    beeps_per_night = 8, # we want to insert 8 beeps for each night
                                    ndays = 40 # number of days in the study (constant over subjects)
)
emotion_mHMM <- emotion_nightgap[,c('subj_id', 'happy', 'relaxed', 'anxious', 'depressed')]

m <- 2
n_dep <- 4

start_gamma <- matrix(c(0.7, 0.3, 
                        0.3, 0.7), byrow = TRUE, ncol = m, nrow = m)

start_emiss <- list(matrix(c(70, 15,                                     # happy
                             30, 15), byrow = TRUE, ncol = 2, nrow = m),
                    matrix(c(70, 15,                                     # relaxed
                             30, 15), byrow = TRUE, ncol = 2, nrow = m),
                    matrix(c(15, 10,                                     # anxious
                             50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # depressed
                             50, 15), byrow = TRUE, ncol = 2, nrow = m))
emotion_prior_emiss <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(70, 30), nrow = 1),  # happy
                   matrix(c(70, 30), nrow = 1),  # relaxed
                   matrix(c(15, 50), nrow = 1),  # anxious
                   matrix(c(15, 50), nrow = 1)),  # depressed
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)
set.seed(42)
out_2st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma), start_emiss),
                        emiss_hyp_prior = emotion_prior_emiss,
                        mcmc = list(J = 500, burn_in = 200))
saveRDS(out_2st_emotion, file = "practicals/01_introduction/data/out_2st_emotion.rds")
