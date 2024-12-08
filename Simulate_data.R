#In this script I will just simulate some data to include in the Rmd report
rm(list = ls())
set.seed(157)
library(truncnorm)
n = 50 #Imagine I have 50 stakeholders for which I have to make a report
doy = seq(1, 365)#I am gonna simulate temporal trends in a certain value
#that differ among sites (N = 50). doy = day of the year
x = cbind(1, doy, doy^2)
xs  = x
xs[,2:ncol(xs)] = apply(xs[,2:ncol(xs)], 2, scale)#Scale covariates
intercept = rnorm(n, qlogis(0.05, 0.01))#A process with an intercept close to zero and little variaability
doy_eff = rtruncnorm(n, a = 0, b = 100, mean = 1, sd= 0.5)
doy2_eff = rtruncnorm(n, a = -100, b = 0, mean = -1, sd = 0.5)
#I am simylated a bell-shaped response. Hence, doy_eff will be positive and doy2_eff negative
betas = cbind(intercept, doy_eff, doy2_eff)

#Simulate trends per site

survey_days = round(seq(from = 1, to = 365, length = 24), 0)#Imagine I only twice a month
names = randomNames::randomNames(50, ethnicity = 4)
date = as.Date(doy, origin = "2024-01-01")

OUT = NULL
for(i in 1:n)
{
  tmp_b = betas[n,]
  mu = plogis(xs %*% betas[i,])
  y = rnorm(length(doy), mean = mu, sd = 0.001)
  #plot(mu~xs[,2])#You may want to look at trends
  #plot(y~xs[,2])
  y_surv = y[survey_days]
  days = doy[survey_days]
  dates = date[survey_days]
  out = cbind(i,names[i], y_surv, days, dates)
  OUT = rbind(OUT, out)
}
df = data.frame(OUT)
colnames(df)  = c( "id", "name","Rate_of_X", "DOY", "date")
write.csv(df, "df1.csv", row.names = F)



# Create differences to a dummy variable ----------------------------------
plots = 20#Plots within sites (each site = 1 stakeholder)
#Half of them with a values of a qualitative variable
x2 = cbind(1, rbinom(plots, 1, 0.5))
intercept2 = rtruncnorm(n, a = 1, b = 4, mean = 1, sd = 0.1)#Very similar intercepts
beta2 = rtruncnorm(n, a = 0, b = 100, mean = 1, sd = 0.1)
beta2 = cbind(intercept2, beta2)

OUT = NULL
for(i in 1:n)
{
  lambda = exp(x2 %*% beta2[i,])
  y = rpois(plots,lambda)
  #boxplot(y~x2[,2])
  out = cbind( i,  names[i], x2[,2], y)
  OUT = rbind(OUT, out)
}

df = data.frame(OUT)
colnames(df) = c( "id", "name", "plot", "abundance_of_X")
df$plot = ifelse(df$plot == 0, "control", "treated")
write.csv(df, "df2.csv", row.names = F)

#Now I create a df with the id of sites and stakeholders' name

id = seq(from = 1, to = n)
xxx = rep(NA, n)
for(i in 1:n)
{
  tmp = strsplit(names, ",")
  name = gsub(" " ,"", tmp[[i]][2])
  surname = gsub(" ", "", tmp[[i]][1])
  xxx[i] = paste(name, surname, sep = " ")
  

}
df = data.frame(id= id, name = xxx)
write.csv(df, "id_sites.csv", row.names = F)


#I generate a random list of species of birds (based on those detected in Asturias
#in previous work of BESLab (beslab.net))

load("birds.RData")
Ri = rpois(n, 20)#Establish a Richness per site
OUT = NULL#it would be better to create lists of names, but I keep it simple here for
#people not used to use lists in R
for(i in 1:n)
{
  bb = sample( birds, Ri[n],replace = FALSE)
  out = cbind(i, bb)
  OUT = rbind(OUT, out)
}
OUT = data.frame(OUT)
colnames(OUT) = c("id", "spp")
write.csv(OUT, "birds_in_sites.csv", row.names = F)
