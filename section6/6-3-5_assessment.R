# config
library(tidyverse)
library(dslabs)

# building the student data
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64* matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Q1 - visualise the 24 test scores by plotting an image
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),, drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="", col = colors, zlim=zlim, ...)
  abline(h=rows + 0.5, v=cols+0.5)
  axis(side=1, cols, colnames(x), las = 2)
}

my_image(y)

    # ans - The students that test well are at the top of the image and there seem to be three groupings by subject.

# Q2 - examine the correlation between the tests, and describe the findings
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side=2, 1:ncol(y), rev(colnames(y)), las = 2)
    
    # ans - There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.

# Q3 - use the svd() function, this returns the orthognals of y
#      compute the sum of squares for Y, YV; confirming they're the same
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_vy <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_vy)

# Q4 - plot ss_y against the column number and do the same for ss_yv, report
plot(ss_y)
plot(ss_vy)

    # ans - ss_yv is decreasing and close to 0 for the 4th column and beyond.

# Q5 - plot the sqrt(ss_vy) against the s$d, report
qplot(sqrt(ss_vy), s$d)

# Q6 - compute the variability of the first 3 columns of VY, report proportion
cumsum(ss_vy/sum(ss_vy))

# Q7 - use the sweep() to compute UD without constructing diag(s$d) or using matrix multiplication
#    - this is a useful trick to avoid creating the matrix diag(s$d)
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

# Q8 - compute the average score for each student, plot it against U1d1,1
ud <- sweep(-s$u, 2, s$d, FUN = "*")
plot(rowMeans(y), ud[,1])

    # EDX CORRECT ANSWER
    plot(-s$u[,1]*s$d[1], rowMeans(y))
    
# Q9 - make and image plot of V, describe the first column relative to the others
image(s$v)

# Q10 - plot U1, then plot V1T, using the same range for the y-axis
#     - make an image of U1d1,1V1T
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)
    # with a vector length of 100, and a vector of 24, we can come close to the 100x24 matrix
    # this is our first matrix factorisation

# Q11 - in Q6 we calculated the total variability, 
#     - our approximation only explains the observation that good students are good in all subjects
#     - it doesnt explain the higher similarity within subject, we can see this:
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

    # - make an image of U2d2,2V2T and compare it to the resid image
    plot(s$u[,2], ylim = c(-0.5, 0.5))
    plot(s$v[,2], ylim = c(-0.5, 0.5))
    with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
    my_image(resid)

# Q12 - we can see the difference is between math/science versus the arts
plot(s$v[,2], ylim = c(-0.5, 0.5))

    # - adding the matrix of the 2 columns accounts for a good portion of the var
    sum(s$d[1:2]^2)/sum(s$d^2) * 100
    
    # - and we can compute new residuals
    resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
    my_image(cor(resid), zlim = c(-1,1))
    axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

    # - what is left is driven by the difference between maths and science
    # - plot U3d3,3V3T, compare to our new resids
    plot(s$u[,3], ylim = c(-0.5, 0.5))
    plot(s$v[,3], ylim = c(-0.5, 0.5))
    with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
    my_image(resid)

# Q13 - we can see the difference is between maths and science
plot(s$v[,3], ylim = c(-0.5, 0.5))
    
    # - adding the matrix of the 3 columns accounts for a good portion of the var
    sum(s$d[1:3]^2)/sum(s$d^2) * 100
    
    # - and we can compute new residuals
    resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
    my_image(cor(resid), zlim = c(-1,1))
    axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
    
    # - and with this we now longer see structure in the residuals
    # - the model is useful because we summarised 100x24 observations with 3x(100+24+1) - 375 numbers
    # - and the three components have useful interpretations
        # - overall ability
        # - difference in ability between math/science and arts
        # - difference between the three subjects
    
    # - plot an image of Y, an image of the final model, and an image of the resids
    y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
    my_image(y, zlim = range(y))
    my_image(y_hat, zlim = range(y))
    my_image(y - y_hat, zlim = range(y))
    