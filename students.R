library(dplyr)
library(pwr)
library(dagitty)
library(rstan)
library(loo)

##### Data preparation

setwd("") # set path here as necessary 
getwd()

data <- read.csv("students_anon.csv", header=TRUE, stringsAsFactors=FALSE)

num_years <- 9

df_base <- data.frame(data)
df_base$Score <- as.numeric(df_base$Total)
str(df_base)

df <- df_base[ complete.cases(df_base$Score) , ]
str(df)

df <- df[!df$Repeat == 1,]
str(df)


##### Data summary

# table

d = df %>% group_by(Year) %>% summarise(total_students = length(Section), 
                                        total_S1 = sum(Section == 1),
                                        total_S2 = sum(Section == 2),
                                        total_S3 = sum(Section == 3),
                                        overall_average = mean(Score),
                                        mean_S1 = mean(Score[Section == 1]),
                                        mean_S2 = mean(Score[Section == 2]),
                                        mean_S3 = mean(Score[Section == 3]))

d

# plot

sections = list(d$mean_S1, d$mean_S2, d$mean_S3)
colours = c('black', 'red', 'blue')
i = 1
for (section in sections)
{
  plot(d$Year, section, ylim=c(40,75), ylab = "score", xlab = 'year', pch = 15, col = colours[i])
  lines(d$Year, section, ylim=c(40,75), ylab = "", col = colours[i])
  par(new = TRUE)
  i = i + 1
}
legend(6,75,c("section 1","section 2", "section 3"), lwd=c(2,2,2), col=c("black","red", "blue"), y.intersp=1.5)

# top and bottom 10

rank <- seq (1,10,1)
top_students <- data.frame(Rank = rank)

bot_rank <- seq(-1,-10,-1)
bot_students <- data.frame(Rank = bot_rank)

for (i in 1:num_years){
  index <- df$Year == i
  
  d_t <- df[index,]
  sorted <- d_t[order(d_t$Score, decreasing = TRUE), ]
  top_ten <- c(sorted$Section[1:10])
  top_students <- cbind(top_students, top_ten)
  
  d_b <- df[index,]
  sorted_bot <- d_b[order(d_b$Score, decreasing = FALSE), ]
  bot_ten <- c(sorted_bot$Section[1:10])
  bot_students <- cbind(bot_students, bot_ten)
}

col_names = c('Rank','Year 1', 'Year 2','Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9')
colnames(top_students) <- col_names
colnames(bot_students) <- col_names
top_students
bot_students

##### Standardizing the exam scores

drops <- c("Total","Repeat")
df <- df[ , !(names(df) %in% drops)]
str(df)

for (y in 1 : num_years){
  index <- df$Year == y
  m <- mean(df$Score[index])
  s <- sd(df$Score[index])
  df$Score_std[index] <- ( df$Score[index] - m )/s
}

str(df)

##### The lead of section 1 students during years 1-5

# box plot

index <- df$Year == 6 | df$Year == 7 | df$Year == 8 | df$Year == 9

df_trimmed <- df[!index,]

str(df_trimmed)

boxplot(Score_std ~ Section,
        data = df_trimmed
)

# histograms

for (i in 1:3){ 
  index = df_trimmed$Section == i
  scores = df_trimmed$Score_std[index]
  hist(scores, main = paste("Histogram of scores for Section", i))
}

# Formal normality test

for (i in 1:3){ 
  index <- df_trimmed$Section == i
  scores <- df_trimmed$Score_std[index]
  print(paste("Section", i, "  p =", shapiro.test(scores))[2])
}

# QQ-plots

for (i in 1:3){ 
  index <- df_trimmed$Section == i
  scores <- df_trimmed$Score_std[index]
  text <- paste("Section", i, "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

# t-test

index <- df_trimmed$Section == 1
scores_1 <- df_trimmed$Score_std[index] 

index <- df_trimmed$Section == 2 | df_trimmed$Section == 3
scores_23 <- df_trimmed$Score_std[index]

str(scores_1)
str(scores_23)

t.test(x = scores_1, y = scores_23,
       alternative = "greater", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

# d calculation

n_1 = length(scores_1) 
n_23 = length(scores_23)

paste("n_1:", n_1, sep = " ")
paste("n_23:", n_23, sep = " ")

m_1 = mean(scores_1)
m_23 = mean(scores_23)

var_1 = var(scores_1)
var_23 = var(scores_23)

paste("var_1:", var_1, sep = " ")
paste("var_23:", var_23, sep = " ")

pooled_var = ((n_1 - 1)*var_1 + (n_23 - 1)*var_23 )/(n_1 + n_23 -2)
paste("pooled_var:", pooled_var, sep = " ")

effect_size = ( m_1 - m_23 )/( sqrt(pooled_var))
paste("Effect size:", effect_size, sep=" ")

s = sd(df_trimmed$Score)
paste("Length of one SD:", s, sep = " ")
paste("Effect size on outcome scale:", s * effect_size, sep = " ")

# the equal variances assumption
var.test(scores_1, scores_23, alternative = 'two.sided')

# ANOVA

aov_results <- aov(Score_std ~ factor(Section), # need 'factor' here to tell aov that Section is categorical
                   data = df_trimmed
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

##### Years 6 - 9

index <- df$Year == 6 | df$Year == 7 | df$Year == 8 | df$Year == 9

df_trimmed2 <- df[index,]

str(df_trimmed2)

# box plot

boxplot(Score_std ~ Section,
        data = df_trimmed2
)

# formal normality test

for (i in 1:3){ 
  index = df_trimmed2$Section == i
  scores = df_trimmed2$Score_std[index]
  print(paste("Section", i, "  p =", shapiro.test(scores))[2])
}

for (i in 1:3){ 
  index = df_trimmed2$Section == i
  scores = df_trimmed2$Score_std[index]
  text <- paste("Section", i, "Normal Q-Q plot")
  qqnorm(scores, main = text, pch = 1, frame = FALSE)
  qqline(scores, col = "steelblue", lwd = 2)
}

# ANOVA

aov_results <- aov(Score_std ~ factor(Section), # need 'factor' here to tell aov that Section is categorical
                   data = df_trimmed2
)

summary(aov_results)

TukeyHSD(aov_results, conf.level=.95)

plot(TukeyHSD(aov_results, conf.level=.95), las = 1)

# t-test for section 1 against the rest

index <- df_trimmed2$Section == 1
scores_1 <- df_trimmed2$Score_std[index] 

index <- df_trimmed2$Section == 2 | df_trimmed2$Section == 3
scores_23 <- df_trimmed2$Score_std[index]

str(scores_1)
str(scores_23)

effect_sizes = c(0.1,0.2,0.3)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_1), 
                            n2 = length(scores_23), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "greater")
  powers[k] <- paste("when d =", d, ", power = ", h_object[5], sep = "  ") 
  k = k + 1
}

paste('n1  =', h_object[1], 'n2  =', h_object[2], 'significance level  =', h_object[4], sep = "  ")

powers

t.test(x = scores_1, y = scores_23,
       alternative = "greater", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

##### Comparing section 2 with section 3 across all years except year 6

index <- df$Year == 6

df_trimmed3 <- df[!index,]

str(df_trimmed3)

index <- df_trimmed3$Section == 2
scores_2 <- df_trimmed3$Score_std[index] 

index <- df_trimmed3$Section == 3
scores_3 <- df_trimmed3$Score_std[index]

str(scores_2)
str(scores_3)

effect_sizes = c(0.1,0.2,0.3)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_2), 
                            n2 = length(scores_3), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "greater")
  powers[k] <- paste("when d =", d, ", power = ", h_object[5], sep = "  ") 
  k = k + 1
}

paste('n1  =', h_object[1], 'n2  =', h_object[2], 'significance level  =', h_object[4], sep = "  ")

powers

t.test(x = scores_2, y = scores_3,
       alternative = "greater", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

# Revisiting dropping of NAs

df_0 <- df_base

df_0[is.na(df_0)] <- 0

df_0 <- df_0[!df_0$Repeat == 1,]

drops <- c("Total","Repeat")
df_0 <- df_0[ , !(names(df_0) %in% drops)]

for (y in 1 : num_years){
  index <- df_0$Year == y
  m <- mean(df_0$Score[index])
  s <- sd(df_0$Score[index])
  df_0$Score_std[index] <- ( df_0$Score[index] - m )/s
}

index <- (df_0$Year == 6)
df_trimmed4 <- df_0[!index, ]

str(df_trimmed4)

index <- df_trimmed4$Section == 2
scores_2 <- df_trimmed4$Score_std[index] 

index <- df_trimmed4$Section == 3
scores_3 <- df_trimmed4$Score_std[index] 

m_2 = mean(scores_2)
m_3 = mean(scores_3)

paste("mean_2:", m_2, sep = " ")
paste("mean_3:", m_3, sep = " ")

effect_sizes = c(0.1,0.2,0.3)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_2), 
                            n2 = length(scores_3), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "greater")
  powers[k] <- paste("when d =", d, " power =", h_object[5], sep = " ") 
  k = k + 1
}

paste('n1  =', h_object[1], 'n2  =', h_object[2], 'significance level  =', h_object[4], sep = "  ")

powers

t.test(x = scores_2, y = scores_3,
       alternative = "greater", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

##### Examining online vs onsite

df_2 <- df_base[ complete.cases(df_base$Score) , ]

for (y in 1 : num_years){
  index <- df_2$Year == y
  m <- mean(df_2$Score[index])
  s <- sd(df_2$Score[index])
  df_2$Score_std[index] <- ( df$Score[index] - m )/s
}

str(df_2)

index <- df_2$Year == 7 | df_2$Year == 8
df_online <- df_2[index,]

index <- df_2$Year == 9
df_onsite <- df_2[index,]

str(df_online)
str(df_onsite)

scores_online <- df_online$Score_std
scores_onsite <- df_onsite$Score_std

t.test(x = scores_online, y = scores_onsite,
       alternative = "two.sided", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

effect_sizes = c(0.1,0.2,0.3,0.4,0.5)
powers = rep(NA, length(effect_sizes))
k = 1
for (d in effect_sizes){   
  h_object <-  pwr.t2n.test(n1 = length(scores_online), 
                            n2 = length(scores_onsite), 
                            d = d, 
                            sig.level = 0.05, 
                            power = NULL,
                            alternative = "two.sided")
  powers[k] <- paste("when d =", d, ", power = ", h_object[5], sep = "  ") 
  k = k + 1
}

paste('n1  =', h_object[1], 'n2  =', h_object[2], 'significance level  =', h_object[4], sep = "  ")

powers

####### Bayesian approach

# creating dataframe

df_full <- df

df_full$Stream_level <- ifelse(df_full$Section == 1 & df_full$Year < 6, 3., ifelse(df_full$Year < 6, 1., 2. ))

drops <- c("Year","Score")
df_full <- df_full[ , !(names(df) %in% drops)]

str(df_full)

# DAG analysis

dag <- dagitty( "dag {
A -> S -> E
A -> E
}")

adjustmentSets( dag , exposure="S" , outcome="E" )
impliedConditionalIndependencies(dag)

cor(df_full)

# the first model

dat <- list(
  A = df_full$Stream_level,
  S = df_full$Section,
  E = df_full$Score_std,
  alpha = rep( 2 , 2 ))

code_1 <- 'data{
    vector[1571] E;
    int S[1571];
    int A[1571];
    vector[2] alpha;
}
parameters{
    real sigma;
    real bS;
    real bA;
    real a;
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
    vector[1571] phi;
    deltaS ~ dirichlet( alpha );
    deltaA ~ dirichlet( alpha );
    a ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bS ~ normal( 0 , 1 );
    sigma ~ lognormal( 0, 0.25 );
    for ( i in 1:1571 ) {
        phi[i] = a + bA * sum(deltaA_j[1:A[i]]) + bS * sum(deltaS_j[1:S[i]]);
    }
    E ~ normal( phi , sigma );
}
generated quantities{
  vector[1571] log_lik;
  
  for (i in 1:1571) log_lik[i] = normal_lpdf(E[i] | a + bA * sum(deltaA_j[1:A[i]]) 
                                             + bS * sum(deltaS_j[1:S[i]]), sigma);
}
'

model_1 <- stan( model_code = code_1 , data = dat , chains=4, cores = 4, iter = 10000 )

saveRDS(model_1, "model_1.rds")

#model_1 <- readRDS("model_1.rds")

pars = c('sigma', 'a', 'bA', 'bS', 'deltaS', 'deltaA')

stan_trace(model_1, pars = pars)

results_1 <- summary(model_1, pars = pars, 
                     probs = c(0.05, 0.95))$summary
results_1

stan_hist(model_1, pars = pars)

samples <- extract(model_1, 'bS') 
counts <- ifelse(samples$bS < -0.05, 1, 0)
p <- sum(counts)/length(counts)
p

# the second model

code_2 <- 'data{
    vector[1571] E;
    int A[1571];
    vector[2] alpha;
}
parameters{
    real sigma;
    real bA;
    real a;
    simplex[2] deltaA;
}

transformed parameters{
vector[3] deltaA_j;
deltaA_j = append_row(0, deltaA);
}

model{
    vector[1571] phi;
    deltaA ~ dirichlet( alpha );
    a ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    sigma ~ exponential( 1 );
    for ( i in 1:1571 ) {
        phi[i] = a + bA * sum(deltaA_j[1:A[i]]);
    }
    E ~ normal( phi , sigma );
}
generated quantities{
  vector[1571] log_lik;
  
  for (i in 1:1571) log_lik[i] = normal_lpdf(E[i] | a + bA * sum(deltaA_j[1:A[i]]), sigma);
}
'

model_2 <- stan( model_code = code_2 , data = dat , chains=4, cores = 4, iter = 10000 )

saveRDS(model_2, "model_2.rds")
#model_2 <- readRDS("model_2.rds")

pars = c('sigma', 'a', 'bA', 'deltaA')

stan_trace(model_2, pars = pars)

results_2 <- summary(model_2, pars = pars, 
                     probs = c(0.05, 0.95))$summary
results_2

stan_hist(model_2, pars = pars)

loo_1 <- loo(model_1)
loo_2 <- loo(model_2)

comp <- loo_compare(loo_1, loo_2)
print(comp, simplify=FALSE)


