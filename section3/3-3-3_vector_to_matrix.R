# config
library(tidyverse)
library(dslabs)

# the matrix() function by default is filled by column
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

# we can use the byrow argument, note this allows us to transpose the matrix we just showed
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t 

# we can use the t() function to transpose a matrix
t(mat)
identical(t(mat), mat_t)

# the matrix() functionrecycles values in the vector without warning
matrix(my_vector, 5, 5)

# to put the pixel intensities of the third entry into a matrix we remember that all of the images were 28 by 28 pixels
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
grid <- matrix(x[3,], 28, 28)
grid

# we can use the image() function to show an image of the 3rd argument
image(1:28, 1:28, grid)

# the image is flipped because this is how R plots images, pixel 1 at the bottom
image(1:28, 1:28, grid[,28:1])
