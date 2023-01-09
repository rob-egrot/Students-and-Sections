# your path here
setwd()
getwd()

library(rstan)
library(loo)

num_iters = 1000

# A convenience function for printing the proportion of the probability mass for the bS parameter below a specified value
# and a specified model.
prop_below <- function(model, prop) {
  samples <- extract(model, 'bS') 
  counts <- ifelse(samples$bS < prop, 1, 0)
  p <- sum(counts)/length(counts)
  paste("Proportion less than", prop, ":", p, sep = " ")
}

data <- read.csv("students_combined.csv", header=TRUE, stringsAsFactors=FALSE)

df_base <- data.frame(data)
str(df_base)

df_base$Score_1 <- as.numeric(df_base$Midterm.Y1)
df_base$Score_2 <- as.numeric(df_base$Final.Y1)
df_base$Score_3 <- as.numeric(df_base$Exam.Y2)
str(df_base)

df <- df_base[complete.cases(df_base$Score_1, df_base$Score_2, df_base$Score_3), ]
str(df)

drops <- c("Midterm.Y1","Final.Y1","Exam.Y2","Teacher.Midterm.Y1","Teacher.Final.Y1")
df <- df[ , !(names(df) %in% drops)]
str(df)

summary(df)

for (y in 2 : 9){
  index <- df$Cohort == y
  
  m <- mean(df$Score_1[index])
  s <- sd(df$Score_1[index])
  df$Score_1_std[index] <- ( df$Score_1[index] - m )/s
  
  m <- mean(df$Score_2[index])
  s <- sd(df$Score_2[index])
  df$Score_2_std[index] <- ( df$Score_2[index] - m )/s
  
  m <- mean(df$Score_3[index])
  s <- sd(df$Score_3[index])
  df$Score_3_std[index] <- ( df$Score_3[index] - m )/s  
}

summary(df)

df_full <- df

df_full$Stream_level.Y1 <- ifelse(df_full$Section.Y1 == 1 & df_full$Cohort < 6, 3., ifelse(df_full$Cohort < 6, 1., 2. ))
df_full$Stream_level.Y2 <- ifelse(df_full$Section.Y2 == 1 & df_full$Cohort < 6, 3., ifelse(df_full$Cohort < 6, 1., 2. ))
str(df_full)

df_trim <- df_full
drops <- c("Score_1","Score_2","Score_3")
df_trim <- df_trim[ , !(names(df) %in% drops)]

str(df_trim)

summary(df_trim)

dat_1 <- list(
  N = 1289,
  K = 3,
  A_1 = df_trim$Stream_level.Y1,
  A_2 = df_trim$Stream_level.Y2,  
  S_1 = df_trim$Section.Y1,
  S_2 = df_trim$Section.Y2,
  E = matrix(c(df_trim$Score_1_std, df_trim$Score_2_std, df_trim$Score_3_std), ncol = 3),
  alpha = rep( 2 , 2 ))

str(dat_1)

code_1 <- 'data{
    int<lower=1> K;
    int<lower=0> N;
    int S_1[N];
    int S_2[N];
    int A_1[N];
    int A_2[N];
    array[N] vector[K] E;
    vector[2] alpha;
}
parameters{
    corr_matrix[K] Omega;     
    vector<lower=0>[K] tau;
    real bA;
    real bS;
    real a_1;
    real a_2;
    real a_3;
    simplex[2] deltaA;
    simplex[2] deltaS;
}

transformed parameters{
    vector[3] deltaA_j;
    vector[3] deltaS_j;
    deltaS_j = append_row(0, deltaS);
    deltaA_j = append_row(0, deltaA);
}

model{
    tau ~ lognormal( 0, 0.25 );
    Omega ~ lkj_corr(2);
    array[N] vector[K] phi;
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    a_1 ~ normal( 0 , 1 );
    a_2 ~ normal( 0 , 1 );
    a_3 ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );

    for ( i in 1:N ) {
        phi[i][1] = a_1 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][2] = a_2 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][3] = a_3 + bA*sum(deltaA_j[1:A_2[i]]) + bS*sum(deltaS_j[1:S_2[i]]);
    }
    E ~ multi_normal(phi, quad_form_diag(Omega, tau));
}

generated quantities{
    vector[N] log_lik; 
    array[N] vector[K] phi;
    for ( i in 1:N ) {
        phi[i][1] = a_1 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][2] = a_2 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][3] = a_3 + bA*sum(deltaA_j[1:A_2[i]]) + bS*sum(deltaS_j[1:S_2[i]]);
        log_lik[i] = multi_normal_lpdf(E[i] | phi[i] , quad_form_diag(Omega, tau) );
    }
}
'
model_1 <- stan( model_code = code_1 , data = dat_1 , chains=4, cores = 4, iter = num_iters )

pars = c('a_1', 'a_2', 'a_3', 'bA', 'bS', 'deltaS', 'deltaA')
results_2 <- summary(model_1, pars = pars, probs = c(0.025, 0.975))$summary
results_2

stan_hist(model_1, pars = pars)

stan_trace(model_1, pars = pars)

prop_below(model_1,-0.1)

prop_below(model_1, -0.05)

prop_below(model_1, -0.01)

prop_below(model_1, 0)

loo_1 <- loo(model_1)
loo_1

# with imputation

df_m <- df_base

drops <- c("Midterm.Y1","Final.Y1","Exam.Y2","Teacher.Midterm.Y1","Teacher.Final.Y1")
df_m <- df_m[ , !(names(df_m) %in% drops)]
str(df_m)

summary(df_m)

for (y in 1 : 9){
  index <- df_m$Cohort == y
  
  m <- mean(df_m$Score_1[index], na.rm = TRUE)
  s <- sd(df_m$Score_1[index], na.rm = TRUE)
  df_m$Score_1_std[index] <- ( df_m$Score_1[index] - m )/s
  
  m <- mean(df_m$Score_2[index], na.rm = TRUE)
  s <- sd(df_m$Score_2[index], na.rm = TRUE)
  df_m$Score_2_std[index] <- ( df_m$Score_2[index] - m )/s
  
  m <- mean(df_m$Score_3[index], na.rm = TRUE)
  s <- sd(df_m$Score_3[index], na.rm = TRUE)
  df_m$Score_3_std[index] <- ( df_m$Score_3[index] - m )/s  
}

summary(df_m)

df_m$Stream_level.Y1 <- ifelse(df_m$Section.Y1 == 1 & df_m$Cohort < 6, 3., ifelse(df_m$Cohort < 6, 1., 2. ))
df_m$Stream_level.Y2 <- ifelse(df_m$Section.Y2 == 1 & df_m$Cohort < 6, 3., ifelse(df_m$Cohort < 6, 1., 2. ))

drops <- c("Score_1","Score_2","Score_3")
df_m <- df_m[ , !(names(df_m) %in% drops)]

str(df_m)

summary(df_m)

exam_matrix = matrix(c(df_m$Score_1_std, df_m$Score_2_std, df_m$Score_3_std), ncol = 3)
exam_matrix_zero = exam_matrix
exam_matrix_zero[is.na(exam_matrix)] <- 0
dat_2 <- list(
  N = 1674,
  K = 3,
  A_1 = df_m$Stream_level.Y1,
  A_2 = df_m$Stream_level.Y2,  
  S_1 = df_m$Section.Y1,
  S_2 = df_m$Section.Y2,
  E = exam_matrix_zero,
  E_missidx = which(is.na(exam_matrix)),
  N_missing = length(which(is.na(exam_matrix))),
  alpha = rep( 2 , 2 ))

str(dat_2)

code_2 <- '
data{
    int<lower=1> K;
    int<lower=0> N;
    int<lower=0> N_missing;
    int S_1[N];
    int S_2[N];
    int A_1[N];
    int A_2[N];
    int E_missidx[N_missing];
    matrix[N,K] E;
    vector[2] alpha;
}
parameters{
    corr_matrix[K] Omega;     
    vector<lower=0>[K] tau;
    real bA;
    real bS;
    real a_1;
    real a_2;
    real a_3;
    simplex[2] deltaA;
    simplex[2] deltaS;
    vector[N_missing] E_impute;
}

transformed parameters{
    vector[3] deltaA_j;
    vector[3] deltaS_j;
    deltaS_j = append_row(0, deltaS);
    deltaA_j = append_row(0, deltaA);
}

model{
    array[N] vector[K] E_merge;
    vector[3*N] E_vec;
    vector[K] temp_vec;
    E_vec = to_vector(E);
    tau ~ lognormal( 0, 0.25 );
    Omega ~ lkj_corr(2);
    array[N] vector[K] phi;
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    a_1 ~ normal( 0 , 1 );
    a_2 ~ normal( 0 , 1 );
    a_3 ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    for ( i in 1:N_missing )
        E_vec[ E_missidx[i] ] = E_impute[i];
    for ( i in 1:N ) {
        for (j in 1:K) {
            temp_vec[j] = E_vec[(j-1)*N + i];
        }
        E_merge[i] = temp_vec; 
        phi[i][1] = a_1 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][2] = a_2 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][3] = a_3 + bA*sum(deltaA_j[1:A_2[i]]) + bS*sum(deltaS_j[1:S_2[i]]);
    }
    E_merge ~ multi_normal(phi, quad_form_diag(Omega, tau));
}

generated quantities{
    array[N] vector[K] E_merge;
    vector[3*N] E_vec;
    vector[K] temp_vec;
    E_vec = to_vector(E);
    vector[N] log_lik; 
    array[N] vector[K] phi;
    for ( i in 1:N_missing )
        E_vec[ E_missidx[i] ] = E_impute[i];
    for ( i in 1:N ) {
        for (j in 1:K) {
            temp_vec[j] = E_vec[(j-1)*N + i];
        }
        E_merge[i] = temp_vec; 
        phi[i][1] = a_1 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][2] = a_2 + bA*sum(deltaA_j[1:A_1[i]]) + bS*sum(deltaS_j[1:S_1[i]]);
        phi[i][3] = a_3 + bA*sum(deltaA_j[1:A_2[i]]) + bS*sum(deltaS_j[1:S_2[i]]);
        log_lik[i] = multi_normal_lpdf(E_merge[i] | phi[i] , quad_form_diag(Omega, tau) );
    }
}

'

model_2 <- stan( model_code = code_2 , data = dat_2 , chains=4, cores = 4, iter = num_iters )

pars = c('a_1', 'a_2', 'a_3', 'bA', 'bS', 'deltaS', 'deltaA')
results_2 <- summary(model_2, pars = pars, probs = c(0.025, 0.975))$summary
results_2

prop_below(model_2,-0.1)

prop_below(model_2, -0.05)

prop_below(model_2, -0.01)

loo_2 <- loo(model_2)
loo_2

stan_hist(model_2, pars = pars)

low_n_effs <- which(psis_n_eff_values(loo_2)< 100)

indices_1 <- which(is.na(df_m$Score_1_std))
indices_2 <- which(is.na(df_m$Score_2_std))
indices_3 <- which(is.na(df_m$Score_3_std))

NAs = union(union(indices_1, indices_2),indices_3)

length(NAs)

length(low_n_effs)

length(intersect(NAs, low_n_effs))

high_k <- which(pareto_k_values(loo_2)> 0.5)
length(high_k)

length(intersect(high_k, NAs))

length(intersect(high_k, low_n_effs))

draws <- extract(model_2, pars = c('bS', 'deltaS'), permuted = TRUE, inc_warmup = FALSE,
                 include = TRUE)

s <- sd(c(df_base$Score_1, df_base$Score_2, df_base$Score_3), na.rm = TRUE)

diff_S2 <- density(draws$bS * draws$deltaS[,1] * s) 

diff_S3 <- density(draws$bS * s) 

plot(diff_S2, ylab = "density", xlab = 'sampled score difference S1 and S2', main ='')

plot(diff_S3, ylab = "density", xlab = 'sampled score difference S1 and S3', main ='')
