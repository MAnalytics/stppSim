## install the required packages
# install.packages("BiocManager")
# BiocManager::install("survcomp")
# BiocManager::install("EmpiricalBrownsMethod")
# BiocManager::install("multtest")
# install.packages("poolr")
# install.packages("genetics")
# install.packages("metap")
# install.packages("aggregation")
# install.packages("gap")
# install.packages("CombinePValue")
# install.packages("TFisher")
# install.packages("harmonicmeanp")
# install.packages("RcppEigen")
# install.packages("xtable")

### page 9

## correlation between p-values from two- and one-sided t-tests
set.seed(2468)
n <- 20
Rmat <- matrix(0.8, nrow = 2, ncol = 2)
diag(Rmat) <- 1
P <- replicate(1000, {
  X <- mvtnorm::rmvnorm(n, mean = c(0, 0), sigma = Rmat)
  p.two.sided.1 <- t.test(X[, 1], alternative = "two.sided")$p.value
  p.two.sided.2 <- t.test(X[, 2], alternative = "two.sided")$p.value
  p.one.sided.1 <- t.test(X[, 1], alternative = "greater")$p.value
  p.one.sided.2 <- t.test(X[, 2], alternative = "greater")$p.value
  c(p.two.sided.1, p.two.sided.2, p.one.sided.1, p.one.sided.2)
})

Rmat[1, 2]^3
cor(P[1, ], P[2, ])
Rmat[1, 2]
cor(P[3, ], P[4, ])

## creating Figure 1
rhos <- seq(-0.99, 0.99, by = 0.02)
if (F) {
  ## note: code for the simulation, but not run because it takes too much time
  n <- 20
  R <- sapply(rhos, function(rho) {
    Rmat <- matrix(rho, nrow = 2, ncol = 2)
    diag(Rmat) <- 1
    P <- replicate(1e+06, {
      X <- mvtnorm::rmvnorm(n, mean = c(0, 0), sigma = Rmat)
      p.one.sided.1 <- t.test(X[, 1], alternative = "greater")$p.value
      p.one.sided.2 <- t.test(X[, 2], alternative = "greater")$p.value
      p.two.sided.1 <- t.test(X[, 1], alternative = "two.sided")$p.value
      p.two.sided.2 <- t.test(X[, 2], alternative = "two.sided")$p.value
      c(p.one.sided.1, p.one.sided.2, p.two.sided.1, p.two.sided.2)
    })
    c(cor(P[1, ], P[2, ]), cor(P[3, ], P[4, ]))
  })
  ## using the output from this for the generation of R below
  dput(round(R, 4))
}

R <- structure(c(-0.9886, 0.9593, -0.9658, 0.8886, -0.9435, 0.8275, -0.9211, 0.7719,
  -0.899, 0.7202, -0.8773, 0.6732, -0.8563, 0.6313, -0.8343, 0.5897, -0.8125, 0.553,
  -0.792, 0.5175, -0.771, 0.4853, -0.7495, 0.4516, -0.7279, 0.4232, -0.7084, 0.3959,
  -0.6869, 0.368, -0.6679, 0.3463, -0.6469, 0.3207, -0.6266, 0.3011, -0.6071, 0.2795,
  -0.5863, 0.2583, -0.568, 0.2387, -0.547, 0.2206, -0.5263, 0.2028, -0.5075, 0.1874,
  -0.4877, 0.1726, -0.4687, 0.1599, -0.447, 0.1443, -0.4314, 0.1334, -0.4089, 0.1198,
  -0.3902, 0.1083, -0.3699, 0.097, -0.3518, 0.0865, -0.3314, 0.0767, -0.3112, 0.0696,
  -0.2938, 0.0605, -0.2743, 0.0525, -0.2545, 0.0448, -0.2355, 0.0384, -0.2189,
  0.0323, -0.1976, 0.0279, -0.1799, 0.0221, -0.1609, 0.0184, -0.1405, 0.0147, -0.1238,
  0.0094, -0.1032, 0.0062, -0.084, 0.0047, -0.0664, 0.0033, -0.0499, 0.0011, -0.0288,
  9e-04, -0.0079, 1e-04, 0.0098, -9e-04, 0.0278, 5e-04, 0.0466, 0.0038, 0.0642,
  0.0041, 0.0835, 0.0049, 0.1055, 0.0101, 0.1222, 0.0106, 0.1418, 0.0136, 0.1596,
  0.0166, 0.1798, 0.0223, 0.1987, 0.0268, 0.2168, 0.033, 0.2368, 0.0398, 0.2568,
  0.0446, 0.2749, 0.0523, 0.294, 0.0586, 0.3129, 0.0702, 0.3327, 0.0764, 0.3525,
  0.0882, 0.3685, 0.0973, 0.3896, 0.1063, 0.409, 0.1201, 0.4288, 0.1316, 0.4465,
  0.1443, 0.4674, 0.1581, 0.4871, 0.1713, 0.5073, 0.1888, 0.5277, 0.2036, 0.5457,
  0.2193, 0.5667, 0.239, 0.5846, 0.2571, 0.6071, 0.277, 0.6262, 0.2984, 0.6471,
  0.3211, 0.6674, 0.3457, 0.6876, 0.3686, 0.7085, 0.3967, 0.7292, 0.4239, 0.7496,
  0.4528, 0.7701, 0.4834, 0.792, 0.5185, 0.8137, 0.5538, 0.834, 0.5894, 0.8555,
  0.6309, 0.8773, 0.6738, 0.8994, 0.7207, 0.9213, 0.7715, 0.9436, 0.8274, 0.9658,
  0.8888, 0.9886, 0.9592), .Dim = c(2L, 100L))
# pdf("../Figures/poolr_rev1_oc-figCorP.pdf", height = 5, width = 9.5)
par(mfrow = c(1, 2))
plot(rhos, R[1, ], xlab = expression(rho), ylab = "Estimated Correlation", type = "o",
  pch = 19, cex = 0.4, main = "(a) One-Sided Tests")
lines(rhos, rhos, lwd = 5, col = "gray70")
points(rhos, R[1, ], type = "o", pch = 19, cex = 0.4)

plot(rhos, R[2, ], xlab = expression(rho), ylab = "Estimated Correlation", type = "o",
  pch = 19, cex = 0.4, main = "(a) Two-Sided Tests")
lines(rhos, abs(rhos)^3, lwd = 5, col = "gray70")
points(rhos, R[2, ], type = "o", pch = 19, cex = 0.4)
# dev.off()

### page 15

## install and load the "poolr" package
# install.packages("poolr")
library("poolr")

### page 16

## arguments of one of the base functions
args(fisher)

## illustrate the use of the fisher() function for combining independent
## p-values
pvals <- c(0.02, 0.03, 0.08, 0.2)
(res <- fisher(pvals))

## combine the p-values with all base methods
fun <- c("fisher", "stouffer", "invchisq", "binomtest", "bonferroni", "tippett")
round(sapply(fun, function(f) do.call(f, list(p = pvals))$p), digits = 5)

### page 17

## power comparison of the base methods when there is only one false null
## hypothesis
k <- 10
iters <- 10000
set.seed(8780)
Z <- replicate(iters, rnorm(k, mean = c(3, rep(0, k - 1)), sd = 1))
P <- 2 * pnorm(abs(Z), lower.tail = FALSE)
sapply(fun, function(f)
  mean(apply(P, 2, function(pvals) do.call(f, list(p = pvals))$p) <= .05))

## power comparison of the base methods when all null hypotheses are false
Z <- replicate(iters, rnorm(k, mean = rep(1, k), sd = 1))
P <- 2 * pnorm(abs(Z), lower.tail = FALSE)
sapply(fun, function(f)
  mean(apply(P, 2, function(pvals) do.call(f, list(p = pvals))$p) <= .05))


### page 18

## generate the correlation matrix for 5 test statistics
Rmat <- matrix(0.7, nrow = 5, ncol = 5)
diag(Rmat) <- 1
Rmat

## generate the test statistics based on the given correlation matrix
ti <- c(mvtnorm::rmvnorm(1, mean = rep(0, 5), sigma = Rmat))
round(ti, digits = 5)

## convert the test statistics into two-sided p-values
pvals <- 2 * pnorm(abs(ti), lower.tail = FALSE)
round(pvals, digits = 5)

## combine the p-values with the base functions
round(sapply(fun, function(f) do.call(f, list(p = pvals))$p), digits = 5)

## adjust Fisher's method with the Li & Ji adjustment
fisher(pvals, adjust = "liji", R = Rmat)

### page 19

## obtain the estimate of the effective number of tests with all methods
methods <- c("nyholt", "liji", "gao", "galwey")
sapply(methods, function(method) meff(Rmat, method = method))

## convert the correlation matrix of the test statistics into the correlation
## matrix of the (two-sided) p-values based on multivariate theory
(Rpmat <- mvnconv(Rmat, target = "p", cov2cor = TRUE))


### page 20

## adjust Fisher's method with the Li & Ji adjustment using this correlation
## matrix
fisher(pvals, adjust = "liji", R = Rpmat)

## illustrate the use of the "m" argument
fisher(pvals, adjust = "liji", R = Rmat)
fisher(pvals, m = meff(Rmat, method = "liji"))

## adjust Fisher's method based on an empirically-derived null distribution
## using pseudo replicates (with the default settings)
fisher(pvals, adjust = "empirical", R = Rmat)

### page 21

## illustrate the use of the stepwise algorithm
fisher(pvals, adjust = "empirical", R = Rmat,
  size = c(1000, 10000, 100000), threshold = c(.10, .01))

## obtain the running time when repeatedly using the adjustment based on an
## empirically-derived null distribution (with the default settings)

## Note: To reduce the amount of time it takes to "compile" the paper using
## Sweave() and to provide results that correspond to the CPU indicated in the
## paper (an Intel Xeon E5-2630v4 processor), the timing results shown in the
## paper are not generated "on the fly" but are hard-coded. However, if the code
## below is run, the random number seed changes, making some of the results that
## follow further down non-reproducible. Therefore, we store the current status
## of the random seed before running the following simulations and restore it
## afterwards.

rand_seed <- .Random.seed

Z <- mvtnorm::rmvnorm(1000, mean = rep(0, 5), sigma = Rmat)
P <- 2 * pnorm(abs(Z), lower.tail = FALSE)
system.time(p <- apply(P, 1, function(pvals) fisher(pvals, adjust = "empirical",
  R = Rmat)$p))

## obtain the running time when repeatedly using the adjustment based on an
## empirically-derived null distribution (with the stepwise algorithm)
system.time(p <- apply(P, 1, function(pvals) fisher(pvals, adjust = "empirical",
  R = Rmat, size = c(1000, 10000, 1e+05), threshold = c(0.1, 0.01))$p))

### page 22

## obtain the running time when repeatedly using the adjustment based on an
## empirically-derived null distribution (generated in batches of size 100)
system.time(p <- apply(P, 1, function(pvals) fisher(pvals, adjust = "empirical",
  R = Rmat, batchsize = 100)$p))

## restore the random number seed
.Random.seed <- rand_seed

## show first six lines of the lookup tables
head(mvnlookup)

### page 23

## convert the correlation matrix of the test statistics into the matrix with
## the correlations among the -2*log(p) values
mvnconv(Rmat, target = "m2lp")

## adjust the Fisher, Stouffer, and inverse chi-square methods based on their
## generalizations under the assumption that the test statistics follow a
## multivariate normal distribution (i.e., Brown's method, Strube's method, and
## the generalized inverse chi-square method, respectively)
fisher(pvals, adjust = "generalized", R = mvnconv(Rmat))
stouffer(pvals, adjust = "generalized", R = mvnconv(Rmat))
invchisq(pvals, adjust = "generalized", R = mvnconv(Rmat))

### page 24

## convert the test statistics into one-sided p-values
pvals <- pnorm(ti, lower.tail = FALSE)
round(pvals, digits = 5)

## combine the one-sided p-values with Brown's method
fisher(pvals, adjust = "generalized", R = mvnconv(Rmat, side = 1))

## combine the one-sided p-values with Fisher's method using the adjustment
## based on an empirically-derived null distribution
fisher(pvals, adjust = "empirical", R = Rmat, side = 1)

### page 25

## genotypes of the first six subjects for the first five SNPs
grid2ip.geno[1:6, 1:5]

## (log-transforemd) CES-D values of the first six subjects
head(grid2ip.pheno)

## convert genotypes into the number of minor alleles
G <- as.data.frame(lapply(grid2ip.geno, function(snp) genetics::genotype(snp)))
X <- as.data.frame(lapply(G, function(snp) genetics::allele.count(snp)[, 2]))
X[1:6, 1:5]

## using the number of minor alleles as predictors, fit an additive model to
## each SNP, with the (log-transformed) CES-D values as response variable
pvals <- sapply(X, function(x) coef(summary(lm(grid2ip.pheno ~ x)))[2, 4])
pvals[1:5]

### page 26

## construct the 23x23 LD correlation matrix based on the genotypes
LD <- genetics::LD(G)$r
LD[lower.tri(LD)] <- t(LD)[lower.tri(LD)]
diag(LD) <- 1
LD[1:5, 1:5]

## creating the Figure 2
# pdf("../Figures/poolr_rev1_oc-figLDmat.pdf", width = 7, height = 4.8)
cols <- hcl.colors(101)

par(mar = c(1, 6, 1, 5))

image(1:nrow(LD), 1:ncol(LD), LD, axes = FALSE, xlab = "", ylab = "", col = cols,
  zlim = c(-1, 1))
axis(2, 1:23, labels = rownames(LD), las = 1, tick = FALSE, cex.axis = 0.8)

UT0 <- LD
UT0[upper.tri(UT0, diag = TRUE)] <- NA
UT0[lower.tri(UT0)] <- 0
image(1:nrow(UT0), 1:ncol(UT0), UT0, axes = FALSE, xlab = "", ylab = "", col = "white",
  zlim = c(-1, 1), add = TRUE)

abline(h = seq(0.5, 23.5, by = 1), col = "white")
abline(v = seq(0.5, 23.5, by = 1), col = "white")

par(xpd = TRUE)
nc <- length(cols)
y.lo <- 3
y.hi <- 20
r.height <- (y.hi - y.lo)/(nc - 1)
rect(24, seq(y.lo - r.height/2, y.hi - r.height/2, length = nc), 25, seq(y.lo + r.height/2,
  y.hi + r.height/2, length = nc), col = cols, border = cols)
l.pos <- seq(-1, 1, by = 0.1)
y.pos <- (l.pos + 1)/2
text(25, y.lo + y.pos * (y.hi - y.lo), formatC(l.pos, format = "f", digits = 1, flag = "+"),
  pos = 4, cex = 0.7)
par(xpd = FALSE)
# dev.off()

## check that pvals is identical to grid2ip.p and LD to grid2ip.ld
c(all.equal(pvals, grid2ip.p), all.equal(LD, grid2ip.ld))

### page 27

## combine the p-values with Fisher's method adjusted with a variety of
## techniques
c(fisher = fisher(pvals)$p, liji = fisher(pvals, adjust = "liji", R = LD)$p, emp = fisher(pvals,
  adjust = "emp", R = LD)$p, brown = fisher(pvals, adjust = "gen", R = mvnconv(LD))$p)

## creating Table 1

## For the reasons described earlier, the code below is not run "on the fly"
## when compiling the paper with Sweave(). Hence, the random number seed is
## again stored and restored afterwards. Note that running the permutation tests
## can take hours (depending on the CPU).

rand_seed <- .Random.seed

perm_iter <- 1e+06
res <- matrix(0, nrow = 6, ncol = 8)

set.seed(1234)

res[1, 1:7] <- c(fisher(grid2ip.p)$p, fisher(grid2ip.p, adjust = "nyholt", R = grid2ip.ld)$p,
  fisher(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p, fisher(grid2ip.p, adjust = "gao",
    R = grid2ip.ld)$p, fisher(grid2ip.p, adjust = "galwey", R = grid2ip.ld)$p,
  fisher(grid2ip.p, adjust = "empirical", R = grid2ip.ld, size = perm_iter)$p,
  fisher(grid2ip.p, adjust = "gen", R = mvnconv(grid2ip.ld))$p)

set.seed(1234)

res[2, 1:7] <- c(stouffer(grid2ip.p)$p, stouffer(grid2ip.p, adjust = "nyholt", R = grid2ip.ld)$p,
  stouffer(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p, stouffer(grid2ip.p, adjust = "gao",
    R = grid2ip.ld)$p, stouffer(grid2ip.p, adjust = "galwey", R = grid2ip.ld)$p,
  stouffer(grid2ip.p, adjust = "empirical", R = grid2ip.ld, size = perm_iter)$p,
  stouffer(grid2ip.p, adjust = "gen", R = mvnconv(grid2ip.ld))$p)

set.seed(1234)

res[3, 1:7] <- c(invchisq(grid2ip.p)$p, invchisq(grid2ip.p, adjust = "nyholt", R = grid2ip.ld)$p,
  invchisq(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p, invchisq(grid2ip.p, adjust = "gao",
    R = grid2ip.ld)$p, invchisq(grid2ip.p, adjust = "galwey", R = grid2ip.ld)$p,
  invchisq(grid2ip.p, adjust = "empirical", R = grid2ip.ld, size = perm_iter)$p,
  invchisq(grid2ip.p, adjust = "gen", R = mvnconv(grid2ip.ld))$p)

set.seed(1234)

res[4, 1:6] <- c(binomtest(grid2ip.p)$p, binomtest(grid2ip.p, adjust = "nyholt",
  R = grid2ip.ld)$p, binomtest(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p, binomtest(grid2ip.p,
  adjust = "gao", R = grid2ip.ld)$p, binomtest(grid2ip.p, adjust = "galwey", R = grid2ip.ld)$p,
  binomtest(grid2ip.p, adjust = "empirical", R = grid2ip.ld, size = perm_iter)$p)

set.seed(1234)

res[5, 1:6] <- c(bonferroni(grid2ip.p)$p, bonferroni(grid2ip.p, adjust = "nyholt",
  R = grid2ip.ld)$p, bonferroni(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p,
  bonferroni(grid2ip.p, adjust = "gao", R = grid2ip.ld)$p, bonferroni(grid2ip.p,
    adjust = "galwey", R = grid2ip.ld)$p, bonferroni(grid2ip.p, adjust = "empirical",
    R = grid2ip.ld, size = perm_iter)$p)

set.seed(1234)

res[6, 1:6] <- c(tippett(grid2ip.p)$p, tippett(grid2ip.p, adjust = "nyholt", R = grid2ip.ld)$p,
  tippett(grid2ip.p, adjust = "liji", R = grid2ip.ld)$p, tippett(grid2ip.p, adjust = "gao",
    R = grid2ip.ld)$p, tippett(grid2ip.p, adjust = "galwey", R = grid2ip.ld)$p,
  tippett(grid2ip.p, adjust = "empirical", R = grid2ip.ld, size = perm_iter)$p)

## conduct "proper" permutation tests

## obtain the SNP-level p-values when repeatedly permuting the outcome variable
set.seed(1234)
P <- t(replicate(perm_iter, {
  yperm <- sample(grid2ip.pheno)
  sapply(X, function(snp) coef(summary(lm(yperm ~ snp)))[2, 4])
}))

## obtain the permutation distributions using the various base methods
fis_perm <- apply(P, 1, function(snp) fisher(snp)$p)
sto_perm <- apply(P, 1, function(snp) stouffer(snp)$p)
ics_perm <- apply(P, 1, function(snp) invchisq(snp)$p)
bin_perm <- apply(P, 1, function(snp) binomtest(snp)$p)
bon_perm <- apply(P, 1, function(snp) bonferroni(snp)$p)
tip_perm <- apply(P, 1, function(snp) tippett(snp)$p)

## compute the combined p-values based on the permutation distributions
perm_res <- c((sum(fis_perm <= fisher(grid2ip.p)$p) + 1) / (perm_iter + 1),
  (sum(sto_perm <= stouffer(grid2ip.p)$p) + 1) / (perm_iter + 1),
  (sum(ics_perm <= invchisq(grid2ip.p)$p) + 1) / (perm_iter + 1),
  (sum(bin_perm <= binomtest(grid2ip.p)$p) + 1) / (perm_iter + 1),
  (sum(bon_perm <= bonferroni(grid2ip.p)$p) + 1) / (perm_iter + 1),
  (sum(tip_perm <= tippett(grid2ip.p)$p) + 1) / (perm_iter + 1))

## put together the table
names(perm_res) <- c("fisher", "stouffer", "invchisq", "binomtest", "bonferroni",
  "tippett")
perm_res

res[, 8] <- perm_res
res[4:6, 7] <- NA

rownames(res) <- c("fisher", "stouffer", "invchisq", "binomtest", "bonferroni", "tippett")
colnames(res) <- c("base", "nyholt", "liji", "gao", "galwey", "empirical", "generalized",
  "permutation")

res
round(-log10(res), 3)

## restore the random number seed
.Random.seed <- rand_seed

### page 28

## compare the Bonferroni method with Holm's procedure (using the p.adjust()
## function)
c(p.adjust_bonferroni = min(p.adjust(pvals, method = "bonferroni")),
  p.adjust_holm = min(p.adjust(pvals, method = "holm")),
  poolr_bonferroni = bonferroni(pvals)$p)

## combine the p-values with a variety of packages / functions to combine
## independent p-values available in R
k <- length(pvals)
invisible(capture.output(pkgs_ind <- matrix(c(
  metap::sumlog(pvals)$p,
  metap::sumz(pvals)$p,
  metap::invchisq(pvals, 1)$p,
  metap::minimump(pvals)$p,
  metap::logitp(pvals)$p,
  survcomp::combine.test(pvals, method = "fisher"),
  survcomp::combine.test(pvals, method = "z.transform"), NA, NA,
  survcomp::combine.test(pvals, method = "logit"),
  aggregation::fisher(pvals), NA,
  aggregation::lancaster(pvals, rep(1, k)),
  aggregation::sidak(pvals), NA,
  gap::metap(data.frame(p = rbind(unname(pvals)), n = rbind(rep(1, k))),
    N = k, prefixp = "p.", prefixn = "n.")$p,
  gap::metap(data.frame(p = rbind(2 * unname(pvals)), n = rbind(rep(1, k))),
    N = k, prefixp = "p.", prefixn = "n.")$p1,
  NA, NA, NA,
  poolr::fisher(pvals)$p,
  poolr::stouffer(pvals)$p,
  poolr::invchisq(pvals)$p,
  poolr::tippett(pvals)$p, NA), nrow = 5, byrow = TRUE,
  dimnames = list(c("metap", "survcomp", "aggregation", "gap", "poolr"),
    c("Fisher", "Stouffer", "Invchisq", "Tippett", "Logit")))))
-log10(pkgs_ind)

### page 29

## comparison with the CombinePValue package
P <- replicate(200, {
  yperm <- sample(grid2ip.pheno)
  sapply(X, function(x) coef(summary(lm(yperm ~ x)))[2, 4])
})
CombinePValue::selfcontained.test(pvals, p_permu = P)[[1]]
fisher(pvals, adjust = "gen", R = cov(t(-2 * log(P))))$p

### page 30

## comparison with the EmpiricalBrownsMethod package
EmpiricalBrownsMethod::empiricalBrownsMethod(t(X), p_values = pvals)
V <- EmpiricalBrownsMethod:::calculateCovariances(t(X))
diag(V) <- 4
fisher(pvals, adjust = "gen", R = V)$p

### page 31

S <- mvtnorm::rmvnorm(886, mean = rep(0, k), sigma = LD)
EmpiricalBrownsMethod::empiricalBrownsMethod(t(S), p_values = pvals)
V <- EmpiricalBrownsMethod:::calculateCovariances(t(S))
diag(V) <- 4
fisher(pvals, adjust = "gen", R = V)$p

## compare the covariance matrix of the -2*log(p) values obtained above with the
## one we obtain using multivariate theory (assuming one-sided tests)
round(unname(V), digits = 4)[1:4, 1:8]
mvnconv(LD, target = "m2lp", side = 1)[1:4, 1:8]

## obtain the covariance matrix using multivariate theory (assuming two-sided
## tests)
mvnconv(LD, target = "m2lp")[1:4, 1:8]

## apply Brown's method
fisher(pvals, adjust = "gen", R = mvnconv(LD))$p

### page 32

## comparison with the TFisher package
1 - TFisher::p.tpm(sum(-2 * log(pvals)), n = k, tau1 = 1, M = LD)
fisher(pvals, adjust = "gen", R = 4 * LD^2)$p

## combine the p-values with the harmonicmeanp package
harmonicmeanp::p.hmp(pvals, L = k)

### page 33

## creating Figure 3
# pdf("poolr_rev1_oc-figTFdiff", height = 5, width = 7)
plot(mvnlookup$rhos, mvnlookup$m2lp_2 - 4 * mvnlookup$rhos^2, xlab = expression(rho),
  ylab = "Difference", type = "o", pch = 19, cex = 0.4)
# dev.off()

### page 34

## show that the correlations among the test statistics are quite similar to the
## correlations among the interchanging elements for one particular scenario

## correlation (phi coefficient) among the interchanging (binary) elements
p11 <- 0.4
p10 <- 0.2
p01 <- 0.1
p00 <- 0.3
(p11 * p00 - p10 * p01)/sqrt((p11 + p10) * (p01 + p00) * (p11 + p01) * (p10 + p00))

## obtain the correlation among the test statistics by simulation
n <- 100
Z <- replicate(10000, {
  X <- rmultinom(n, 1, c(p11, p10, p01, p00))
  x1 <- X[1, ] + X[2, ]
  x2 <- X[1, ] + X[3, ]
  y <- rnorm(n)
  res1 <- RcppEigen::fastLmPure(cbind(1, x1), y)
  res2 <- RcppEigen::fastLmPure(cbind(1, x2), y)
  c(res1$coefficients[2]/res1$se[2], res2$coefficients[2]/res2$se[2])
})
cor(Z[1, ], Z[2, ])

### page 35

## creating Figure 4 (left panel)
# png("../Figures/poolr-figContour.png", units = "in", width = 6.5, height = 5.8)
res <- MASS::kde2d(Z[1, ], Z[2, ], n = 50)
filled.contour(res, xlab = "Variable 1", ylab = "Variable 2", color = hcl.colors,
  nlevels = 50, xlim = c(-3, 3), ylim = c(-3, 3))
## dev.off()

## creating Figure 4 (right panel)
# png("../Figures/poolr-figQQ.png", units = "in", width = 6.5, height = 5.8)
V <- var(t(Z))
Vi <- solve(V)
z2 <- apply(Z, 2, function(z) t(z) %*% Vi %*% z)
sav <- qqplot(qchisq(ppoints(10000), df = 2), z2, plot.it = FALSE)
par(mar = c(5, 5, 4, 4))
plot(NA, xlim = c(0, max(c(sav$x, sav$y))), ylim = c(0, max(c(sav$x, sav$y))),
  xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(z2, distribution = function(p) qchisq(p, df = 2), lwd = 3, col = "gray")
points(sav$x, sav$y, pch = 19, cex = 0.5)
# dev.off()

