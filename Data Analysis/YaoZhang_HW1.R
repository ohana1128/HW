#1 Multivariate Normal I

#1.Generate 1000 standard normal random variables and store them in a matrix of size 100*10
N <- matrix(rnorm(1000),nrow = 100,ncol = 10)
#2.Calculate the correlation matrix of this random matirx, and plot it
cor(N)
M <- cor(N)
image(t(M)[ncol(M):1,])
#3.Repeat 1. and 2.with matrices of size 1000*10 and 1000*10.
N1 <- matrix(rnorm(10000),nrow = 1000,ncol = 10)
cor(N1)
M1 <- cor(N1)
image(t(M1)[ncol(M1):1,])
N2 <- matrix(rnorm(100000),nrow = 10000,ncol = 10)
cor(N2)
M2 <- cor(N2)
image(t(M2)[ncol(M2):1,])

#2.Multivariate Normal II

#1.Create a 10*10 matrix E with 1 on diagonals and 0.4 elsewhere.
M4 <- matrix(rep(0.4,100),nrow=10,ncol=10)+diag(rep(0.6,10))
#2.Compute the Choleski factorization of your newly created matrix and store it. Name it "choleski"
choleski <- chol(M4)
#3.Generate 1000 standard normal random variables and store them in a matrix of size 100*10
N4 <- matrix(rnorm(1000),nrow = 100,ncol = 10)
#Then, multiply this matrix on the right by "choleski"
N5 <- N4%*%choleski
#4.Calculate the correlation matrix of this random matrix, and plot it using image()
cor(N5)
M5 <- cor(N5)
image(t(M5)[ncol(M5):1,])
#5.Repeat 3. and 4. with matrices of size 1000*10 and 10000*10
N6 <- matrix(rnorm(10000),nrow = 1000,ncol = 10)
N7 <- N6%*%choleski
cor(N7)
M7 <- cor(N7)
image(t(M7)[ncol(M7):1,])
N8 <- matrix(rnorm(100000),nrow = 10000,ncol = 10)
N9 <- N8%*%choleski
cor(N9)
M9 <- cor(N9)
image(t(M9)[ncol(M9):1,])

#3.The Guinness Distribution

#Download the Student.R and run
X_MEAN <- 10
X_SIGMA <- 2
LOOP_SIZE <- 10000
SAMPLE_SIZE <- 5

z <- rep(0,LOOP_SIZE)

for(i in 1:LOOP_SIZE){
  temp <- rnorm(SAMPLE_SIZE,mean=X_MEAN,sd = X_SIGMA) 
  z[i] <- (mean(temp)-X_MEAN)/sqrt(X_SIGMA^2/SAMPLE_SIZE)
}

hist(z,100)
#Estimate the S2
X_MEAN <- 10
X_SIGMA <- 2
LOOP_SIZE <- 10000
SAMPLE_SIZE <- 5
X_S  <- ((SAMPLE_SIZE/(SAMPLE_SIZE-1))*X_SIGMA)

newz <- rep(0,LOOP_SIZE)

for(i in 1:LOOP_SIZE){
  temp <- rnorm(SAMPLE_SIZE,mean=X_MEAN,sd = X_S) 
  newz[i] <- (mean(temp)-X_MEAN)/sqrt(X_S^2/SAMPLE_SIZE)
}

#Plot the histogram of the new z. add the density of Gaussian and the t distribution
hist(newz,100,freq=FALSE)
x <- seq(-4,4,by=.05)
lines(x,dnorm(x),type='l',col=3)
y <- rt(10000,df=4)
lines(density(y),col=2)
  
#4.Test and Significance

#install"HSAUR" package and load dataset "skull"
install.packages("HSAUR")
library(tools)
library(HSAUR)
data("skulls",package = "HSAUR")

#1.Wilcoxon test the height
Group3300 <- subset(skulls,epoch=="c3300BC",select = nh)
Group200 <- subset(skulls,epoch=="c200BC",select = nh)
wilcox.test(Group3300$nh,Group200$nh,alternative = "two.sided")

#2.Compare the variance
GroupBC <- subset(skulls,epoch=="c4000BC",select = nh,drop = TRUE)
GroupAD <- subset(skulls,epoch=="cAD150",select = nh,drop = TRUE)
var.test(GroupBC,GroupAD)
