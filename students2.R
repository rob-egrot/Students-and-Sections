# your path here
setwd()
getwd()

library(loo)
library(rstan)
library(pwr)

# set number of years
num_years <- 8

# set number of iterations for Bayesian models
num_iters = 1000

# read data
data <- read.csv("students_teachers_anon.csv", header=TRUE, stringsAsFactors=FALSE)
df_base <- data.frame(data)
df_base$Midterm <- as.numeric(df_base$Midterm)
df_base$Final <- as.numeric(df_base$Final)
str(df_base)

df <- df_base[ complete.cases(df_base$Midterm, df_base$Final) , ]
str(df)

df <- df[!df$Repeat == 1,]
str(df)

# summarizing the data

library(dplyr)
d = df %>% group_by(Year) %>% summarise(total_students = length(Section), 
                                        total_S1 = sum(Section == 1),
                                        total_S2 = sum(Section == 2),
                                        total_S3 = sum(Section == 3),
                                        teacher_S1_M = mean(Teacher_M[Section==1]),
                                        teacher_S2_M = mean(Teacher_M[Section==2]),
                                        teacher_S3_M = mean(Teacher_M[Section==3]),
                                        overall_average_M = mean(Midterm),
                                        mean_M_S1 = mean(Midterm[Section == 1]),
                                        mean_M_S2 = mean(Midterm[Section == 2]),
                                        mean_M_S3 = mean(Midterm[Section == 3]),
                                        teacher_S1_F = mean(Teacher_F[Section==1]),
                                        teacher_S2_F = mean(Teacher_F[Section==2]),
                                        teacher_S3_F = mean(Teacher_F[Section==3]),
                                        overall_average_F = mean(Final),
                                        mean_F_S1 = mean(Final[Section == 1]),
                                        mean_F_S2 = mean(Final[Section == 2]),
                                        mean_F_S3 = mean(Final[Section == 3]))

d

sections = list(d$mean_M_S1, d$mean_M_S2, d$mean_M_S3)
colours = c('black', 'red', 'blue')

d$s1_key_1 <- ifelse(d$teacher_S1_M == 1, 19, ifelse(d$teacher_S1_M == 2 , 15, ifelse(d$teacher_S1_M == 3, 7, 3)))
d$s2_key_1 <- ifelse(d$teacher_S2_M == 1, 19, ifelse(d$teacher_S2_M == 2 , 15, ifelse(d$teacher_S2_M == 3, 7, 3)))
d$s3_key_1 <- ifelse(d$teacher_S3_M == 1, 19, ifelse(d$teacher_S3_M == 2 , 15, ifelse(d$teacher_S3_M == 3, 7, 3)))
icons_1 <- list(d$s1_key_1, d$s2_key_1, d$s3_key_1) 

i = 1
for (section in sections)
{
  plot(d$Year, section, ylim=c(30,68), ylab = "score Midterm", xlab = 'year', pch = icons_1[[i]], col = 'black')
  lines(d$Year, section, ylim=c(30,68), ylab = "", col = colours[i])
  par(new = TRUE)
  i = i + 1
}

legend(6.5,42.1, lty = c(1,1,1,0,0,0,0), pch = c(3,3,3,19,15,7,3), 
       col = c("black", "red", "blue", "black", "black", "black", "black"),
       legend = c("section 1","section 2", "section 3",
                  "teacher 1", "teacher 2", "teacher 3", "teacher 4"))

sections = list(d$mean_F_S1, d$mean_F_S2, d$mean_F_S3)

d$s1_key_2 <- ifelse(d$teacher_S1_F == 1, 19, ifelse(d$teacher_S1_F == 2 , 15, ifelse(d$teacher_S1_F == 3, 7, 3)))
d$s2_key_2 <- ifelse(d$teacher_S2_F == 1, 19, ifelse(d$teacher_S2_F == 2 , 15, ifelse(d$teacher_S2_F == 3, 7, 3)))
d$s3_key_2 <- ifelse(d$teacher_S3_F == 1, 19, ifelse(d$teacher_S3_F == 2 , 15, ifelse(d$teacher_S3_F == 3, 7, 3)))
icons_2 <- list(d$s1_key_2, d$s2_key_2, d$s3_key_2)

i = 1
for (section in sections)
{
  plot(d$Year, section, ylim=c(30,68), ylab = "score Final", xlab = 'year', pch = icons_2[[i]], col = 'black')
  lines(d$Year, section, ylim=c(30,68), ylab = "", col = colours[i])
  par(new = TRUE)
  i = i + 1
}

legend(6.5,42.1, lty = c(1,1,1,0,0,0), pch = c(3,3,3,19,15,7), 
       col = c("black", "red", "blue", "black", "black", "black"),
       legend = c("section 1","section 2", "section 3",
                  "teacher 1", "teacher 2", "teacher 3"))

# standardizing scores

for (y in 1 : num_years){
  index <- df$Year == y
  m_1 <- mean(df$Midterm[index])
  m_2 <- mean(df$Final[index])  
  s_1 <- sd(df$Midterm[index])
  s_2 <- sd(df$Final[index])
  df$Midterm_std[index] <- ( df$Midterm[index] - m_1 )/s_1
  df$Final_std[index] <- ( df$Final[index] - m_2 )/s_2  
}

str(df)

# normality assumptions

index <- df$Section == 1 & (df$Year <= 4)

df_trimmed <- df[!index,]

str(df_trimmed)

for (i in 1:4){ 
  index = df_trimmed$Teacher_M == i
  scores = df_trimmed$Midterm_std[index]
  hist(scores, main = paste("Histogram of scores for Teacher", i, "in midterm"))
}

for (i in 1:3){ 
  index = df_trimmed$Teacher_F == i
  scores = df_trimmed$Final_std[index]
  hist(scores, main = paste("Histogram of scores for Teacher", i, "in final"))
}

for (i in 1:4){ 
  index = df_trimmed$Teacher_M == i
  scores = df_trimmed$Midterm_std[index]
  text <- paste("Teacher", i, "Midterm", "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

for (i in 1:3){ 
  index = df_trimmed$Teacher_F == i
  scores = df_trimmed$Final_std[index]
  text <- paste("Teacher", i, "Final", "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

for (i in 1:3){ 
  index = df_trimmed$Section == i
  scores = df_trimmed$Midterm_std[index]
  text <- paste("Section", i, "Midterm", "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

for (i in 1:3){ 
  index = df_trimmed$Section == i
  scores = df_trimmed$Final_std[index]
  text <- paste("Section", i, "Final", "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

for (i in 1:4){ 
  index <- df_trimmed$Teacher_M == i
  scores <- df_trimmed$Midterm_std[index]
  print(paste("Teacher", i, "Midterm  p =", shapiro.test(scores))[2])
}

for (i in 1:3){ 
  index <- df_trimmed$Teacher_F == i
  scores <- df_trimmed$Final_std[index]
  print(paste("Teacher", i, "Midterm  p =", shapiro.test(scores))[2])
}

for (i in 1:3){ 
  index <- df_trimmed$Section == i
  scores <- df_trimmed$Midterm_std[index]
  print(paste("Section", i, "Midterm  p =", shapiro.test(scores))[2])
}

for (i in 1:3){ 
  index <- df_trimmed$Section == i
  scores <- df_trimmed$Final_std[index]
  print(paste("Section", i, "Final  p =", shapiro.test(scores))[2])
}

# box plots of standardized scores

boxplot(Midterm_std ~ Teacher_M,
        data = df_trimmed
)

boxplot(Final_std ~ Teacher_F,
        data = df_trimmed
)

boxplot(Midterm_std ~ Section,
        data = df_trimmed
)

boxplot(Final_std ~ Section,
        data = df_trimmed
)

# classical investigation of teacher influence

aov_results <- aov(Midterm_std ~ factor(Teacher_M), # need 'factor' here to tell aov that Teacher_M is categorical
                   data = df_trimmed
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

aov_results <- aov(Final_std ~ factor(Teacher_F),
                   data = df_trimmed
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

index <- df_trimmed$Teacher_M == 1
scores_t1_m <- df_trimmed$Midterm_std[index]

index <- df_trimmed$Teacher_M == 2
scores_t2_m <- df_trimmed$Midterm_std[index]

index <- df_trimmed$Teacher_F == 1
scores_t1_f <- df_trimmed$Final_std[index]

index <- df_trimmed$Teacher_F == 2
scores_t2_f <- df_trimmed$Final_std[index]

t.test(x = scores_t1_m, y = scores_t2_m,
       alternative = "two.sided", paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

t.test(x = scores_t1_f, y = scores_t2_f,
       alternative = "two.sided", paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

effect_sizes = c(0.1,0.2,0.3)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_t1_e1), 
                            n2 = length(scores_t2_e1), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "greater")
  powers[k] <- paste("when d =", d, " power =", h_object[5], sep = " ") 
  k = k + 1
}

powers

# the influence of section numbering - classical analysis

index <- df_trimmed$Section == 2
scores_M2 <- df_trimmed$Midterm_std[index]

index <- df_trimmed$Section == 3
scores_M3 <- df_trimmed$Midterm_std[index]

index <- df_trimmed$Section == 2
scores_F2 <- df_trimmed$Final_std[index]

index <- df_trimmed$Section == 3
scores_F3 <- df_trimmed$Final_std[index]

t.test(x = scores_M2, y = scores_M3,
       alternative = "two.sided", paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

t.test(x = scores_F2, y = scores_F3,
       alternative = "two.sided", paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

effect_sizes = c(0.1,0.2,0.3)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_M2), 
                            n2 = length(scores_M3), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "greater")
  powers[k] <- paste("when d =", d, " power =", h_object[5], sep = " ") 
  k = k + 1
}

powers

aov_results <- aov(Midterm_std ~ factor(Section),
                   data = df_trimmed
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

aov_results <- aov(Final_std ~ factor(Section),
                   data = df_trimmed
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

# three bayesian models

df_full <- df

df_full$M_t1 <- ifelse(df_full$Teacher_M == 1, 1., 0.)
df_full$M_t2 <- ifelse(df_full$Teacher_M == 2, 1., 0.)
df_full$M_t3 <- ifelse(df_full$Teacher_M == 3, 1., 0.)
df_full$M_t4 <- ifelse(df_full$Teacher_M == 4, 1., 0.)

df_full$F_t1 <- ifelse(df_full$Teacher_F == 1, 1., 0.)
df_full$F_t2 <- ifelse(df_full$Teacher_F == 2, 1., 0.)
df_full$F_t3 <- ifelse(df_full$Teacher_F == 3, 1., 0.)
df_full$F_t4 <- ifelse(df_full$Teacher_F == 4, 1., 0.)

df_full$Stream_level <- ifelse(df_full$Section == 1 & df_full$Year < 5, 3., ifelse(df_full$Year < 5, 1., 2. ))

str(df_full)

dat <- list(
  N = 1359,
  M = df_full$Midterm_std, # scores for midterm
  F = df_full$Final_std, # scores for final
  
  # indicator variables for teachers in midterm
  M_t1 = df_full$M_t1, 
  M_t2 = df_full$M_t2,
  M_t3 = df_full$M_t3, 
  M_t4 = df_full$M_t4,  
  
  # indicator variables for teachers in final
  F_t1 = df_full$F_t1, 
  F_t2 = df_full$F_t2,
  F_t3 = df_full$F_t3, 
  F_t4 = df_full$F_t4,
  
  A = df_full$Stream_level, # discrete ordered predictor for 3 levels of streaming
  S = df_full$Section, # section number
  alpha = rep( 2 , 2 ) # parameters for Dirichlet priors
)

code_1 <- 'data{
    int N;
    vector[N] M;
    vector[N] F;
    int M_t1[N];
    int M_t2[N];
    int M_t3[N];
    int M_t4[N];
    int F_t1[N];
    int F_t2[N];
    int F_t3[N];
    int F_t4[N];
    int S[N];
    int A[N];
    vector[2] alpha;
}
parameters{
    real bt1;
    real bt2;
    real bt3;
    real bt4;
    real a_F;
    real a_M;
    real bS;
    real bA;
    real<lower=0> sigma_M;
    real<lower=0> sigma_F;
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
    vector[N] mu_M;
    vector[N] mu_F;
    a_M ~ normal( 0 , 1 );
    a_F ~ normal( 0 , 1 );
    bt1 ~ normal( 0 , 1 );
    bt2 ~ normal( 0 , 1 );
    bt3 ~ normal( 0 , 1 );
    bt4 ~ normal( 0 , 1 );
    sigma_F ~ lognormal( 0, 0.25 );
    sigma_M ~ lognormal( 0,  0.25 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    for ( i in 1:N ) {
        mu_F[i] = a_F + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu_M[i] = a_M + bt1*M_t1[i] + bt2*M_t2[i] + bt3*M_t3[i] + bt4*M_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
    }
    F ~ normal( mu_F , sigma_F );
    M ~ normal( mu_M , sigma_M );
}
generated quantities{
vector[N] log_lik;

for (i in 1:N) log_lik[i] = normal_lpdf(F[i] | a_F + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]), 
                                    sigma_F)

                             + normal_lpdf(M[i] | a_M + bt1*M_t1[i] + bt2*M_t2[i] + bt3*M_t3[i] + bt4*M_t4[i] 
                                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]), 
                                    sigma_M);
                              
}'

model_1 <- stan( model_code = code_1 , data = dat , chains=4, 
                 cores = 4, iter = num_iters )

results_1 <- summary(model_1, probs = c(0.05, 0.95), 
                     pars = c('sigma_M', 'sigma_F', 'bt1', 'bt2', 
                              'bt3', 'bt4', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_1

pars = c('sigma_M', 'sigma_F', 'bt1', 'bt2', 
         'bt3', 'bt4', 'a_M', 'a_F', 
         'bS', 'bA', 'deltaA', 'deltaS')

stan_trace(model_1, pars = pars)

stan_hist(model_1, pars = pars)

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.2, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.2:", p, sep = " ")

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.1, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.1:", p, sep = " ")

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.05, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.05:", p, sep = " ")

code_2 <- 'data{
    int N;
    vector[N] M;
    vector[N] F;
    int M_t1[N];
    int M_t2[N];
    int M_t3[N];
    int M_t4[N];
    int F_t1[N];
    int F_t2[N];
    int F_t3[N];
    int F_t4[N];
    int S[N];
    int A[N];
    vector[2] alpha;
}
parameters{
    real bM_t1;
    real bM_t2;
    real bM_t3;
    real bM_t4;
    real bF_t1;
    real bF_t2;
    real bF_t3;
    real bF_t4;
    real a_F;
    real a_M;
    real bS;
    real bA;
    real<lower=0> sigma_M;
    real<lower=0> sigma_F;
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
    vector[N] mu_M;
    vector[N] mu_F;
    a_M ~ normal( 0 , 1 );
    a_F ~ normal( 0 , 1 );
    bM_t1 ~ normal( 0 , 1 );
    bF_t1 ~ normal( 0 , 1 );
    bM_t2 ~ normal( 0 , 1 );
    bF_t2 ~ normal( 0 , 1 );
    bM_t3 ~ normal( 0 , 1 );
    bF_t3 ~ normal( 0 , 1 );
    bM_t4 ~ normal( 0 , 1 );
    bF_t4 ~ normal( 0 , 1 );
    sigma_F ~ lognormal( 0, 0.25 );
    sigma_M ~ lognormal( 0,  0.25 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    for ( i in 1:N ) {
        mu_F[i] = a_F + bF_t1*F_t1[i] + bF_t2*F_t2[i] + bF_t3*F_t3[i] + bF_t4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu_M[i] = a_M + bM_t1*M_t1[i] + bM_t2*M_t2[i] + bM_t3*M_t3[i] + bM_t4*M_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
    }
    F ~ normal( mu_F , sigma_F );
    M ~ normal( mu_M , sigma_M );
}
generated quantities{
vector[N] log_lik;

for (i in 1:N) log_lik[i] = normal_lpdf(F[i] | a_F + bF_t1*F_t1[i] + bF_t2*F_t2[i] + bF_t3*F_t3[i] + bF_t4*F_t4[i] 
                                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]), 
                                    sigma_F)

                             + normal_lpdf(M[i] | a_M + bM_t1*M_t1[i] + bM_t2*M_t2[i] + bM_t3*M_t3[i] + bM_t4*M_t4[i] 
                                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]), 
                                    sigma_M);
                              
}'

model_2 <- stan( model_code = code_2 , data = dat , chains=4, 
                 cores = 4, iter = num_iters )

results_2 <- summary(model_2, probs = c(0.025, 0.975), 
                     pars = c('sigma_M', 'sigma_F', 'bM_t1', 'bM_t2', 
                              'bM_t3', 'bM_t4', 'bF_t1', 'bF_t2', 'bF_t3', 'bF_t4', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_2

pars = c('sigma_M', 'sigma_F', 'bM_t1', 'bM_t2', 
         'bM_t3', 'bM_t4', 'bF_t1', 'bF_t2', 'bF_t3', 'bF_t4', 'a_M', 'a_F', 
         'bS', 'bA', 'deltaA', 'deltaS')

stan_trace(model_2, pars = pars)

stan_hist(model_2, pars = pars)

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.2, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.2:", p, sep = " ")

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.1, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.1:", p, sep = " ")

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.05, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.05:", p, sep = " ")

dat_2 <- list(
  N = 1359,
  E = matrix(c(df_full$Midterm_std, df_full$Final_std), ncol = 2), # exam results as a matrix
  M = df_full$Midterm_std, # scores for midterm
  F = df_full$Final_std, # scores for final
  
  # indicator variables for teachers in midterm
  M_t1 = df_full$M_t1, 
  M_t2 = df_full$M_t2,
  M_t3 = df_full$M_t3, 
  M_t4 = df_full$M_t4,  
  
  # indicator variables for teachers in final
  F_t1 = df_full$F_t1, 
  F_t2 = df_full$F_t2,
  F_t3 = df_full$F_t3, 
  F_t4 = df_full$F_t4,
  
  A = df_full$Stream_level, # discrete ordered predictor for 3 levels of streaming
  S = df_full$Section, # section number
  alpha = rep( 2 , 2 ) # parameters for Dirichlet priors
)

code_3 <- 'data{
    int N;
    array[N] vector[2] E;
    int M_t1[N];
    int M_t2[N];
    int M_t3[N];
    int M_t4[N];
    int F_t1[N];
    int F_t2[N];
    int F_t3[N];
    int F_t4[N];
    int S[N];
    int A[N];
    vector[2] alpha;
}
parameters{
    corr_matrix[2] Omega;     
    vector<lower=0>[2] tau;
    real bt1;
    real bt2;
    real bt3;
    real bt4;
    real a_F;
    real a_M;
    real bS;
    real bA;
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
    array[N] vector[2] mu;
    a_M ~ normal( 0 , 1 );
    a_F ~ normal( 0 , 1 );
    bt1 ~ normal( 0 , 1 );
    bt2 ~ normal( 0 , 1 );
    bt3 ~ normal( 0 , 1 );
    bt4 ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    for ( i in 1:N ) {
        mu[i][1] = a_M + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu[i][2] = a_F + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
    }

    E ~ multi_normal(mu, quad_form_diag(Omega, tau));
}

generated quantities{
    vector[N] log_lik; 
    array[N] vector[2] mu;
    for ( i in 1:N ) {
        mu[i][1] = a_M + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu[i][2] = a_F + bt1*F_t1[i] + bt2*F_t2[i] + bt3*F_t3[i] + bt4*F_t4[i] 
                  + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        log_lik[i] = multi_normal_lpdf(E[i] | mu[i] , quad_form_diag(Omega, tau) );
    }
}

'

model_3 <- stan( model_code = code_3 , data = dat_2 , chains=4, 
                 cores = 4, iter = num_iters )

results_3 <- summary(model_3, probs = c(0.025, 0.975), 
                     pars = c('tau', 'Omega', 'bt1', 'bt2', 
                              'bt3', 'bt4', 'bt1', 'bt2', 'bt3', 'bt4', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_3

pars = c('tau', 'Omega', 'bt1', 'bt2', 'bt3', 'bt4', 'a_M', 'a_F', 
         'bS', 'bA', 'deltaA', 'deltaS')

stan_trace(model_3, pars = pars)

stan_hist(model_3, pars = pars)

loo_1 <- loo(model_1)

loo_2 <- loo(model_2)

loo_3 <- loo(model_3)


comp <- loo_compare(loo_1, loo_2, loo_3)
print(comp, simplify=FALSE)

# revisiting the dropping of absents

df_abs <- df_base
df_abs[is.na(df_abs)] <- 0
df_abs <- df_abs[!df_abs$Repeat == 1,]

for (y in 1 : num_years){
  index <- df_abs$Year == y
  m_1 <- mean(df_abs$Midterm[index])
  m_2 <- mean(df_abs$Final[index])  
  s_1 <- sd(df_abs$Midterm[index])
  s_2 <- sd(df_abs$Final[index])
  df_abs$Midterm_std[index] <- ( df_abs$Midterm[index] - m_1 )/s_1
  df_abs$Final_std[index] <- ( df_abs$Final[index] - m_2 )/s_2  
}

str(df_abs)

df_abs_full <- df_abs

df_abs_full$M_t1 <- ifelse(df_abs_full$Teacher_M == 1, 1, 0)
df_abs_full$M_t2 <- ifelse(df_abs_full$Teacher_M == 2, 1, 0)
df_abs_full$M_t3 <- ifelse(df_abs_full$Teacher_M == 3, 1, 0)
df_abs_full$M_t4 <- ifelse(df_abs_full$Teacher_M == 4, 1, 0)

df_abs_full$F_t1 <- ifelse(df_abs_full$Teacher_F == 1, 1, 0)
df_abs_full$F_t2 <- ifelse(df_abs_full$Teacher_F == 2, 1, 0)
df_abs_full$F_t3 <- ifelse(df_abs_full$Teacher_F == 3, 1, 0)
df_abs_full$F_t4 <- ifelse(df_abs_full$Teacher_F == 4, 1, 0)

df_abs_full$Stream_level <- ifelse(df_abs_full$Section == 1 & df_abs_full$Year < 5, 3., ifelse(df_abs_full$Year < 5, 1., 2. ))

str(df_abs_full)

dat_abs <- list(
  N = 1413,
  E = matrix(c(df_abs_full$Midterm_std, df_abs_full$Final_std), ncol = 2),
  
  M_t1 = df_abs_full$M_t1, 
  M_t2 = df_abs_full$M_t2,
  M_t3 = df_abs_full$M_t3, 
  M_t4 = df_abs_full$M_t4,  
  
  F_t1 = df_abs_full$F_t1, 
  F_t2 = df_abs_full$F_t2,
  F_t3 = df_abs_full$F_t3, 
  F_t4 = df_abs_full$F_t4,
  
  A = df_abs_full$Stream_level, 
  S = df_abs_full$Section,
  alpha = rep( 2 , 2 ) 
)

str(dat_abs)

model_4 <- stan( model_code = code_3 , data = dat_abs , chains=4, 
                 cores = 4, iter = num_iters )

results_4 <- summary(model_4, probs = c(0.025, 0.975), 
                     pars = c('tau', 'Omega', 'bt1', 'bt2', 
                              'bt3', 'bt4', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_4

samples <- extract(model_4, 'bS') 
counts <- ifelse(samples$bS < -0.1, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.1:", p, sep = " ")

samples <- extract(model_4, 'bS') 
counts <- ifelse(samples$bS < -0.05, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.05:", p, sep = " ")

# dropping year 5

index <- df_abs_full$Year == 5
df_no_5 <- df_abs_full[!index,]
str(df_no_5)

dat_no_5 <- list(
  N = 1324,
  E = matrix(c(df_no_5$Midterm_std, df_no_5$Final_std), ncol=2),
  
  # indicator variables for teachers in midterm
  M_t1 = df_no_5$M_t1, 
  M_t2 = df_no_5$M_t2,
  M_t3 = df_no_5$M_t3, 
  M_t4 = df_no_5$M_t4,  
  
  # indicator variables for teachers in final
  F_t1 = df_no_5$F_t1, 
  F_t2 = df_no_5$F_t2,
  F_t3 = df_no_5$F_t3, 
  F_t4 = df_no_5$F_t4,     
  A = df_no_5$Stream_level, # discrete ordered predictor for 3 levels of streaming
  S = df_no_5$Section, # section number
  alpha = rep( 2 , 2 ) # parameters for Dirichlet priors
)

str(dat_no_5)

model_5 <- stan( model_code = code_3 , data = dat_no_5 , chains=4, 
                 cores = 4, iter = num_iters )

results_5 <- summary(model_5, probs = c(0.025, 0.975), 
                     pars = c('tau', 'Omega', 'bt1', 'bt2', 
                              'bt3', 'bt4', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_5

samples <- extract(model_5, 'bS') 
counts <- ifelse(samples$bS < -0.05, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.05:", p, sep = " ")

samples <- extract(model_5, 'bS') 
counts <- ifelse(samples$bS < -0.01, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than -0.01:", p, sep = " ")

samples <- extract(model_5, 'bS') 
counts <- ifelse(samples$bS < 0, 1, 0)
p <- sum(counts)/length(counts)
paste("Proportion less than 0:", p, sep = " ")

# model without teacher influence

dat_no_T <- list(
  N = 1359,
  E = matrix(c(df_full$Midterm_std, df_full$Final_std), ncol = 2),   
  A = df_full$Stream_level, # discrete ordered predictor for 3 levels of streaming
  S = df_full$Section, # section number
  alpha = rep( 2 , 2 ) # parameters for Dirichlet priors
)

str(dat_no_T)

code_4 <- 'data{
    int N;
    array[N] vector[2] E;
    int S[N];
    int A[N];
    vector[2] alpha;
}
parameters{
    corr_matrix[2] Omega;     
    vector<lower=0>[2] tau;
    real a_F;
    real a_M;
    real bS;
    real bA;
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
    array[N] vector[2] mu;
    a_M ~ normal( 0 , 1 );
    a_F ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    for ( i in 1:N ) {
        mu[i][1] = a_M + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu[i][2] = a_F + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
    }

    E ~ multi_normal(mu, quad_form_diag(Omega, tau));
}

generated quantities{
    vector[N] log_lik; 
    array[N] vector[2] mu;
    for ( i in 1:N ) {
        mu[i][1] = a_M + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        mu[i][2] = a_F + bA*sum(deltaA_j[1:A[i]]) + bS*sum(deltaS_j[1:S[i]]);
        log_lik[i] = multi_normal_lpdf(E[i] | mu[i] , quad_form_diag(Omega, tau) );
    }
}

'

model_6 <- stan( model_code = code_4 , data = dat_no_T , chains=4, 
                 cores = 4, iter = num_iters )

results_6 <- summary(model_6, probs = c(0.025, 0.975), 
                     pars = c('tau', 'Omega', 'a_M', 'a_F', 
                              'bS', 'bA', 'deltaA', 'deltaS'))$summary
results_6

# a model without section information

code_5 <- 'data{
    int N;
    array[N] vector[2] E;
    int A[N];
    vector[2] alpha;
}
parameters{
    corr_matrix[2] Omega;     
    vector<lower=0>[2] tau;
    real a_F;
    real a_M;
    real bA;
    simplex[2] deltaA;
    
}

transformed parameters{
vector[3] deltaA_j;
deltaA_j = append_row(0, deltaA);
}

model{
    tau ~ lognormal( 0, 0.25 );
    Omega ~ lkj_corr(2);
    array[N] vector[2] mu;
    a_M ~ normal( 0 , 1 );
    a_F ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    deltaA ~ dirichlet( alpha );
    for ( i in 1:N ) {
        mu[i][1] = a_M + bA*sum(deltaA_j[1:A[i]]);
        mu[i][2] = a_F + bA*sum(deltaA_j[1:A[i]]);
    }

    E ~ multi_normal(mu, quad_form_diag(Omega, tau));
}

generated quantities{
    vector[N] log_lik; 
    array[N] vector[2] mu;
    for ( i in 1:N ) {
        mu[i][1] = a_M + bA*sum(deltaA_j[1:A[i]]);
        mu[i][2] = a_F + bA*sum(deltaA_j[1:A[i]]);
        log_lik[i] = multi_normal_lpdf(E[i] | mu[i] , quad_form_diag(Omega, tau) );
    }
}

'

model_7 <- stan( model_code = code_5 , data = dat_no_T , chains=4, 
                 cores = 4, iter = num_iters )

results_7 <- summary(model_7, probs = c(0.025, 0.975), 
                     pars = c('tau', 'Omega', 'a_M', 'a_F', 'bA', 'deltaA'))$summary
results_7

loo_6 <- loo(model_6)

loo_7 <- loo(model_7)


comp <- loo_compare(loo_1, loo_2, loo_3, loo_6, loo_7)
print(comp, simplify=FALSE)