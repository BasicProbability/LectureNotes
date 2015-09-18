# Script to generate likelihood plots for chapter 5.
# Plots are based on samples of bit-sequences of length 10
# Author: Philip Schulz

sequence_length = 10
sparse_samples = 2
dense_samples = 50
theta = 0.7

dense_data = rbinom(dense_samples, sequence_length, theta)
sparse_data = rbinom(sparse_samples, sequence_length, theta)

dense_likelihood = double()
sparse_likelihood = double()

params = seq(0,1,0.001)

for (param in params) { dense_likelihood = c(dense_likelihood, prod(dbinom(dense_data, sequence_length, param))) }
for (param in params) { sparse_likelihood = c(sparse_likelihood, prod(dbinom(sparse_data, sequence_length, param))) }

dense_mode = max(dense_likelihood)
sparse_mode = max(sparse_likelihood)

dense_mode_idx = match(dense_mode, dense_likelihood)/length(params)
sparse_mode_idx = match(sparse_mode, sparse_likelihood)/length(params)

png("sparse_likelihood.png", width=8, height=8, units="in", res=300)
plot(params, sparse_likelihood, xlab=expression(Theta), ylab="Likelihood", type ="l", col="blue")
axis(1,at = seq(0,10,0.1))
segments(x0=sparse_mode_idx, y0=0, x1=sparse_mode_idx, sparse_mode)
dev.off()

png("dense_likelihood.png", width=8, height=8, units="in", res=300)
plot(params, dense_likelihood, xlab=expression(Theta), ylab="Likelihood", type ="l", col="red")
axis(1,at = seq(0,10,0.1))
segments(x0=dense_mode_idx, y0=0, x1=dense_mode_idx, dense_mode)
dev.off()

