
## PARTIAMO CON 2 CLUSTER ##
# per ora teniamo i cluster fatti con average, divisi in 2, dovrebbero essere i furgoni

load("~/Desktop/Nonpar/Proj/cl1.rda")
load("~/Desktop/Nonpar/Proj/cl2.rda")
load("~/Desktop/Nonpar/Proj/definitivedata.Rdata")

# partiamo con un test permutazionale multivariato: statistica test: 
# usiamo prima di tutto distanza delle medie^2

t1 = data[cl1,-c(12,13,14)]
t2 = data[cl2,-c(12,13,14)]

# qui togliamo le variabili categoriche naturalmente

t1.mean = colMeans(t1)
t2.mean = colMeans(t2)

# calcoliamo le statistiche test

n1 = dim(t1)[1]
n2 = dim(t2)[1]
n  = n1 + n2

T20 = as.numeric((t1.mean-t2.mean) %*% (t1.mean-t2.mean))
T20

B<-100000

T2 = numeric(B)
set.seed(26111193)
for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled = rbind(t1,t2)
  permutation = sample(n)
  t_perm = t_pooled[permutation,]
  t1_perm = t_perm[1:n1,]
  t2_perm = t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm = colMeans(t1_perm)
  t2.mean_perm = colMeans(t2_perm)
  T2[perm]  = (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}

#plots

hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

#let's compute the pval

p_val = sum(T2>=T20)/B
p_val

# we can't reject null hypothesis at 5% but we can at 10%
# now, we check considering first the physical characteristics, LENGTH, HEIGHT, PAYLOAD, CARGO_VOL;
# I expect big differences here

t1p = t1[,c(2,3,4,5)]
t2p = t2[,c(2,3,4,5)]

# qui togliamo le variabili categoriche naturalmente

t1p.mean = colMeans(t1p)
t2p.mean = colMeans(t2p)

# calcoliamo le statistiche test

T20p = as.numeric((t1p.mean-t2p.mean) %*% (t1p.mean-t2p.mean))
T20p
T2p = numeric(B)

for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled = rbind(t1p,t2p)
  permutation = sample(n)
  t_perm = t_pooled[permutation,]
  t1_perm = t_perm[1:n1,]
  t2_perm = t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm = colMeans(t1_perm)
  t2.mean_perm = colMeans(t2_perm)
  T2p[perm]  = (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}

#plots

hist(T2p,xlim=range(c(T2p,T20p)))
abline(v=T20p,col=3,lwd=4)

plot(ecdf(T2p))
abline(v=T20p,col=3,lwd=4)

#let's compute the pvalue

p_valp = sum(T2p>=T20p)/B
p_valp

#obviously these features are very different

# let's check the others between these two groups

t1o = t1[,-c(2,3,4,5)]
t2o = t2[,-c(2,3,4,5)]

# qui togliamo le variabili categoriche naturalmente

t1o.mean = colMeans(t1o)
t2o.mean = colMeans(t2o)

# calcoliamo le statistiche test

T20o = as.numeric((t1o.mean-t2o.mean) %*% (t1o.mean-t2o.mean))
T20o
T2o = numeric(B)

for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled = rbind(t1o,t2o)
  permutation = sample(n)
  t_perm = t_pooled[permutation,]
  t1_perm = t_perm[1:n1,]
  t2_perm = t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm = colMeans(t1_perm)
  t2.mean_perm = colMeans(t2_perm)
  T2o[perm]  = (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}

#plots

hist(T2o,xlim=range(c(T2o,T20o)))
abline(v=T20o,col=3,lwd=4)

plot(ecdf(T2o))
abline(v=T20o,col=3,lwd=4)

#let's compute the pvalue

p_valo = sum(T2o>=T20o)/B
p_valo

# much more similar to the result with the complete dataset this time

cl<-numeric(n)
cl[cl1]="auto"
cl[cl2]="furgone"
levels(factor(cl))

data_anova<-data[,-c(12,13,14)]

fit <- manova(as.matrix(data_anova) ~ cl)
summary.manova(fit,test="Wilks") 
Ta <- -summary.manova(fit,test="Wilks")$stats[1,2]
Ta                 

C<-10000
Ta_stat <- numeric(C)

for(perm in 1:C){
  # choose random permutation
  permutation <- sample(1:n)
  cl.perm <- cl[permutation]
  fit.perm <- manova(as.matrix(data_anova) ~ cl.perm)
  Ta_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}

# p-value
p_val <- sum(Ta_stat>=Ta)/B
p_val


