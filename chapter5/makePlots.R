# Script to generate likelihood plots for chapter 5.
# Plots are based on samples of bit-sequences of length 10
# Author: Philip Schulz

sequence_length = 10
sparse_samples = 2
dense_samples = 50
theta = 0.7

dense_data = rbinom(dense_samples, sequence_length, theta)
sparse_data1 = rbinom(sparse_samples, sequence_length, theta)
sparse_data2 = rbinom(sparse_samples, sequence_length, theta)
sparse_data3 = rbinom(sparse_samples, sequence_length, theta)

dense_likelihood = double()
sparse_likelihood1 = double()
sparse_likelihood2 = double()
sparse_likelihood3 = double()

params = seq(0,1,0.001)

for (param in params) { dense_likelihood = c(dense_likelihood, prod(dbinom(dense_data, sequence_length, param))) }
for (param in params) { sparse_likelihood1 = c(sparse_likelihood1, prod(dbinom(sparse_data1, sequence_length, param))) }
for (param in params) { sparse_likelihood2 = c(sparse_likelihood2, prod(dbinom(sparse_data2, sequence_length, param))) }
for (param in params) { sparse_likelihood3= c(sparse_likelihood3, prod(dbinom(sparse_data3, sequence_length, param))) }

dense_mode = max(dense_likelihood)
sparse_mode1 = max(sparse_likelihood1)
sparse_mode2 = max(sparse_likelihood2)
sparse_mode3 = max(sparse_likelihood3)
highest_mode = max(c(sparse_mode1, sparse_mode2, sparse_mode3))

dense_mode_idx = match(dense_mode, dense_likelihood)/length(params)
sparse_mode1_idx = match(sparse_mode1, sparse_likelihood1)/length(params)
sparse_mode2_idx = match(sparse_mode2, sparse_likelihood2)/length(params)
sparse_mode3_idx = match(sparse_mode3, sparse_likelihood3)/length(params)

png("sparse_likelihood.png", width=8, height=8, units="in", res=300)
plot(params, sparse_likelihood1, xlab=expression(Theta), ylab="Likelihood", type ="l", col="blue", ylim = c(0,highest_mode))
axis(1,at = seq(0,10,0.1))
lines(params, sparse_likelihood2, col="green")
lines(params, sparse_likelihood3, col="red")
segments(x0=sparse_mode1_idx, y0=0, x1=sparse_mode1_idx, sparse_mode1)
segments(x0=sparse_mode2_idx, y0=0, x1=sparse_mode2_idx, sparse_mode2)
segments(x0=sparse_mode3_idx, y0=0, x1=sparse_mode3_idx, sparse_mode3)
dev.off()

png("dense_likelihood.png", width=8, height=8, units="in", res=300)
plot(params, dense_likelihood, xlab=expression(Theta), ylab="Likelihood", type ="l", col="red")
axis(1,at = seq(0,10,0.1))
segments(x0=dense_mode_idx, y0=0, x1=dense_mode_idx, dense_mode)
dev.off()

