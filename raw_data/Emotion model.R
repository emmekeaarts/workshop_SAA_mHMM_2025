
library(mHMMbayes)
emotion_data <- readRDS("./raw_data/data_Rowland2020.rds")
emotion_data$beep2 <- (emotion_data$dayno - 1) * 6 + emotion_data$beep

ggplot_emotion <- rbind(data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$happy,
                                 emotion = "happy"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$excited,
                                 emotion = "excited"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$relaxed,
                                 emotion = "relaxed"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$satisfied,
                                 emotion = "satisfied"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$angry,
                                 emotion = "angry"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$anxious,
                                 emotion = "anxious"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$depressed,
                                 emotion = "depressed"),
                      data.frame(ID = emotion_data$subj_id, 
                                 beep2 = emotion_data$beep2, 
                                 outcome = emotion_data$sad,
                                 emotion = "sad"))

ggplot_emotion$emotion <- as.factor(ggplot_emotion$emotion)

ggplot(ggplot_emotion[ggplot_emotion$ID %in% c(1:5),]) +
  geom_line( mapping = aes(x = beep2, y = outcome, group = emotion, color = emotion)) +
  facet_wrap(vars(ID), ncol = 1)


emotion_mHMM <- data.frame(subj_ID = emotion_data$subj_id,
                           happy = emotion_data$happy,
                           excited = emotion_data$excited,
                           relaxed = emotion_data$relaxed,
                           satisfied = emotion_data$satisfied,
                           angry = emotion_data$angry,
                           anxious = emotion_data$anxious,
                           depressed = emotion_data$depressed,
                           sad = emotion_data$sad)


#####################
## 2 state model ####
#####################
m <- 2
n_dep <- 8

start_gamma <- matrix(c(0.7, 0.3, 
                        0.3, 0.7), byrow = TRUE, ncol = m, nrow = m)

start_emiss <- list(matrix(c(70, 15,                                     # happy
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # excited
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # relaxed
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # satisfied
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # angry
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # anxious
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # depressed
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # sad
                             40, 15), byrow = TRUE, ncol = 2, nrow = m))

emotion_prior_emiss <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(70, 30), nrow = 1),  # happy
                   matrix(c(70, 30), nrow = 1),  # excited
                   matrix(c(70, 30), nrow = 1),  # relaxed
                   matrix(c(70, 30), nrow = 1),  # satisfied
                   matrix(c(15, 40), nrow = 1),  # angry
                   matrix(c(15, 40), nrow = 1),  # anxious
                   matrix(c(15, 40), nrow = 1),  # depressed
                   matrix(c(15, 40), nrow = 1)), # sad 
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)


out_2st_emotion <- mHMM(s_data = emotion_mHMM,
                                  data_distr = "continuous",
                                  gen = list(m = m, n_dep = n_dep),
                                  start_val = c(list(start_gamma), start_emiss),
                                  emiss_hyp_prior = emotion_prior_emiss,
                                  mcmc = list(J = 500, burn_in = 200))

saveRDS(out_2st_emotion, file = "out_2st_emotion.rds")

out_2st_emotion
summary(out_2st_emotion)
gamma_group <- obtain_gamma(out_2st_emotion) 

plot(gamma_group)

emiss_group <- obtain_emiss(out_2st_emotion) 
plot(emiss_group)

plot(out_2st_emotion, component = "emiss", dep = 8)

#####################
## 3 state model ####
#####################
## general model properties
m <- 3
n_dep <- 8

## starting values for gamma
start_gamma_3st <- matrix(c(0.8, 0.1, 0.1,
                            0.1, 0.8, 0.1,
                            0.1, 0.1, 0.8), byrow = TRUE, ncol = m, nrow = m)

## starting values for the emission distribution 
start_emiss_3st <- list(matrix(c(80, 10,                                     # happy
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # excited
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # relaxed
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # satisfied
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # angry
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # anxious
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # depressed
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # sad
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m))

## specifying weakly informative prior for continuous emission distributions
emotion_prior_emiss_3st <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(80, 50, 20), nrow = 1),  # happy
                   matrix(c(80, 50, 20), nrow = 1),  # excited
                   matrix(c(80, 50, 20), nrow = 1),  # relaxed
                   matrix(c(80, 50, 20), nrow = 1),  # satisfied
                   matrix(c(10, 30, 50), nrow = 1),  # angry
                   matrix(c(10, 30, 50), nrow = 1),  # anxious
                   matrix(c(10, 30, 50), nrow = 1),  # depressed
                   matrix(c(10, 30, 50), nrow = 1)), # sad 
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)

out_3st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma_3st), start_emiss_3st),
                        emiss_hyp_prior = emotion_prior_emiss_3st,
                        mcmc = list(J = 500, burn_in = 200))

saveRDS(out_3st_emotion, file = "out_3st_emotion.rds")

summary(out_3st_emotion)
out_3st_emotion

#####################
## 4 state model ####
#####################
## general model properties
m <- 4
n_dep <- 8

## starting values for gamma
start_gamma_4st <- matrix(c(0.7, 0.1, 0.1, 0.1,
                            0.1, 0.7, 0.1, 0.1,
                            0.1, 0.1, 0.7, 0.1,
                            0.1, 0.1, 0.1, 0.7), byrow = TRUE, ncol = m, nrow = m)

## starting values for the emission distribution 
start_emiss_4st <- list(matrix(c(80, 10,                                     # happy
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # excited
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # relaxed
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # satisfied
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # angry
                                 20, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # anxious
                                 320, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # depressed
                                 20, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # sad
                                 20, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m))

## specifying weakly informative prior for continuous emission distributions
emotion_prior_emiss_4st <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(80, 60, 40, 20), nrow = 1),  # happy
                   matrix(c(80, 60, 40, 20), nrow = 1),  # excited
                   matrix(c(80, 60, 40, 20), nrow = 1),  # relaxed
                   matrix(c(80, 60, 40, 20), nrow = 1),  # satisfied
                   matrix(c(10, 20, 40, 60), nrow = 1),  # angry
                   matrix(c(10, 20, 40, 60), nrow = 1),  # anxious
                   matrix(c(10, 20, 40, 60), nrow = 1),  # depressed
                   matrix(c(10, 20, 40, 60), nrow = 1)), # sad 
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)

out_4st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma_4st), start_emiss_4st),
                        emiss_hyp_prior = emotion_prior_emiss_4st,
                        mcmc = list(J = 500, burn_in = 200))

saveRDS(out_4st_emotion, file = "out_4st_emotion.rds")

out_4st_emotion
summary(out_4st_emotion)



## Including covariate ##
# specify a list with the correct number of elements, where all elements are set to NULL
out_2st_emotion <- readRDS("./practicals/02_more_advanced/data/out_2st_emotion.rds")
covariate <- vector("list", length = 1 + n_dep)

# extract the covariate `group`
group <- apply(table(emotion_data$subj_id, emotion_data$group), 1, which.max)
group <- group - 1

# specify the first element of the list, where the first column consists of ones only,  
# the second column contains the covariate group, with only one entry per subject
n_subj <- out_2st_emotion$input$n_subj
covariate[[1]] <- matrix(c(rep(1,n_subj), 
                           group), byrow = FALSE, ncol = 2)
covariate

m <- 2
n_dep <- 8

start_gamma <- matrix(c(0.7, 0.3, 
                        0.3, 0.7), byrow = TRUE, ncol = m, nrow = m)

start_emiss <- list(matrix(c(70, 15,                                     # happy
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # excited
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # relaxed
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(70, 15,                                     # satisfied
                             30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # angry
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # anxious
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # depressed
                             40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # sad
                             40, 15), byrow = TRUE, ncol = 2, nrow = m))

emotion_prior_emiss <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(70, 30), nrow = 1),  # happy
                   matrix(c(70, 30), nrow = 1),  # excited
                   matrix(c(70, 30), nrow = 1),  # relaxed
                   matrix(c(70, 30), nrow = 1),  # satisfied
                   matrix(c(15, 40), nrow = 1),  # angry
                   matrix(c(15, 40), nrow = 1),  # anxious
                   matrix(c(15, 40), nrow = 1),  # depressed
                   matrix(c(15, 40), nrow = 1)), # sad 
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)


out_2st_emotion_cov <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        xx = covariate,
                        start_val = c(list(start_gamma), start_emiss),
                        emiss_hyp_prior = emotion_prior_emiss,
                        mcmc = list(J = 500, burn_in = 200))

saveRDS(out_2st_emotion_cov, file = "out_2st_emotion_cov.rds")

burn_in <- 200
J <- 500
summary_covariate <- data.frame(median = 
                                  apply(out_2st_emotion_cov$gamma_cov_bar[burn_in:J,], 2, median),
                                lower_CrI = 
                                  apply(out_2st_emotion_cov$gamma_cov_bar[burn_in:J,], 2, quantile, 0.025),
                                upper_CrI =
                                  apply(out_2st_emotion_cov$gamma_cov_bar[burn_in:J,], 2, quantile, 0.975))




emiss_subject <- obtain_emiss(out_2st_emotion, level = "subject")
vars <- names(emiss_subject)
gg_emiss_subject <- data.frame(Subj = rep(rep(1:n_subj, each = m), n_dep),
                               State = rep(1:m, n_subj * n_dep), 
                               Dep = factor(c(rep(vars, each = m * n_subj))))
                                            
gg_emiss_subject$Mean <-c(unlist(lapply(emiss_subject[[1]], "[", 1:m)), 
                                                                      unlist(lapply(emiss_subject[[2]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[3]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[4]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[5]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[6]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[7]], "[", 1:m)),
                                                                      unlist(lapply(emiss_subject[[8]], "[", 1:m)))


vars <- names(emiss_subject)
gg_emiss_subject <- data.frame(Subj = rep(rep(1:n_subj, each = m), n_dep),
                               State = rep(1:m, n_subj * n_dep), 
                               Dep = factor(c(rep(vars, each = m * n_subj)), levels = vars))

gg_emiss_subject$Mean <-c(unlist(lapply(emiss_subject[[1]], "[", 1:m)), 
                          unlist(lapply(emiss_subject[[2]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[3]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[4]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[5]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[6]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[7]], "[", 1:m)),
                          unlist(lapply(emiss_subject[[8]], "[", 1:m)))

library(reshape2)
emiss_group <- obtain_emiss(out_2st_emotion)

emiss_group <- lapply(emiss_group, function(x,m){rownames(x) <- paste0(1:m);x}, m = m)
emiss_group_melt <- lapply(emiss_group, melt)
emiss_group_melt <- do.call(rbind, emiss_group_melt)
emiss_group_melt <- emiss_group_melt[emiss_group_melt$Var2 == "Mean", c(1, 3)]
colnames(emiss_group_melt) <- c("State", "Mean")
emiss_group_melt$Dep <- factor(c(rep(vars, each = m)), levels = names(emiss_group))
emiss_group_melt$State <- factor(emiss_group_melt$State, label = 1:m)

ggplot(emiss_group_melt, aes(x = State, y = Mean, fill = Dep)) +
  geom_bar(stat="identity") +
  geom_point(data = gg_emiss_subject[gg_emiss_subject$Subj %in% 1:50,], aes(x = State, y = Mean, fill = Dep), 
             alpha = 0.5, colour="black",pch=21) +
  geom_line(data = gg_emiss_subject[gg_emiss_subject$Subj %in% 1:50,], aes(x = State, y = Mean, group = Subj), color = "grey", alpha = 0.5) +
  facet_grid(cols = vars(Dep)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Mood state")

library(ggplot2)
## plot
ggplot(emiss_group_melt, aes(x = State, y = Mean, fill = Dep)) +
  geom_bar(stat="identity") +
  geom_jitter(data = gg_emiss_subject, aes(x = State, y = Mean, color = Dep)) +
  facet_grid(cols = vars(Dep)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Mood state")

## Fitting 2 state model with different starting values ####
m <- 2
n_dep <- 8

start_gamma_b <- matrix(c(0.9, 0.1, 
                          0.1, 0.9), byrow = TRUE, ncol = m, nrow = m)

start_emiss_b <- list(matrix(c(80, 10,                                     # happy
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(80, 10,                                     # happy
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(80, 10,                                     # happy
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(80, 10,                                     # happy
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # angry
                               30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # angry
                               30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # angry
                               30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # angry
                               30, 15), byrow = TRUE, ncol = 2, nrow = m))


out_2st_emotion_b <- mHMM(s_data = emotion_mHMM,
                          data_distr = "continuous",
                          gen = list(m = m, n_dep = n_dep),
                          start_val = c(list(start_gamma_b), start_emiss_b),
                          emiss_hyp_prior = emotion_prior_emiss,
                          mcmc = list(J = 500, burn_in = 200))

saveRDS(out_2st_emotion_b, file = "out_2st_emotion_b.rds")

## checking traceplots ####
data1 <- out_2st_emotion
J <- 500
burn_in <- 200
j <- 1 
i <- 1
  for(j in 1:n_dep){
    par(mfrow = c(m, 2))
    for(i in 1:m){
      plot(data1$emiss_mu_bar[[j]][, i], type = "l", 
           main = paste("Mu dep", j, "in state", i), 
           ylim = c(1,100))
      abline(h = mean(data1$emiss_mu_bar[[j]][((burn_in + 1): J), i]), col = "blue")
      plot(data1$emiss_sd_bar[[j]][, i], type = "l", 
           main = paste("Var dep", j, "in state", i), 
           ylim = c(0,30))
      abline(h = mean(data1$emiss_sd_bar[[j]][((burn_in + 1): J), i]), col = "blue")
    }
  }

n_subj <- 20

for(s in 1:n_subj){
  par(mfrow = c(2, m))
  for(j in 3:4){
    for(i in 1:m){
      plot(data1$PD_subj[[s]]$cont_emiss[, (j-1) * m + i], type = "l", 
           main = paste("Mu dep", j, "in state", i), 
           ylim = c(1,100))
      abline(h = mean(data1$PD_subj[[s]]$cont_emiss[((burn_in + 1): J), (j-1) * m + i]), col = "blue")
    }
  }
}
