# source: https://doi.org/10.1002/sim.8281
set.seed(42)
a <- 4.5
b <- 1.5
sample_size <- 10000
number <- rbeta(sample_size, a, b)
P <- sort(number)
Y <- sapply(P, function(x) ifelse(runif(1) < x, 1, 0))
# P <- rep(mean(Y), sample_size)
# P <- runif(sample_size)
loess.calibrate <- loess(Y ~ P)
P.calibrate <- predict(loess.calibrate, P)
# P.calibrate <- lowess(P, Y, f=0.1, iter=0)$y
plot(P, P.calibrate, "l", xlim=c(0, 1), ylim=c(0, 1), xlab="P", ylab="P.calibrate", asp=1)
abline(0, 1, col="red")
ICI <- mean(abs(P.calibrate - P))
E50 <- median(abs(P.calibrate - P))
E90 <- quantile(abs(P.calibrate - P), 0.9, names=FALSE)
Emax <- max(abs(P.calibrate - P))

print(ICI)
print(E50)
print(E90)
print(Emax)