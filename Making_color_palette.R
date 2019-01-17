#
# 1. Making a color palette from a photo
#

#need to read image with jpeg package
#install.packages("jpeg")
library("jpeg")

#read jpeg
#can do so from the website address
download.file("https://andreacirilloac.github.io/dataviz/images/sacra_famiglia_canigiani.jpg", "image.jpg")
painting <- readJPEG("image.jpg")
#this jpeg has been imported as an array (multidimentional matrix) of triples, each storing one value for red, green, and blue

#need to perform k-means, must make new dataframs which stores triples along with x,y info for pixels
dimension <- dim(painting)
painting_rgb <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  R = as.vector(painting[,,1]), #slicing our array into three
  G = as.vector(painting[,,2]),
  B = as.vector(painting[,,3])
)

#take a look at the painting
head(painting_rgb)

#now apply k-means to data
k_means <- kmeans(painting_rgb[,c("R","G","B")], centers = 20, iter.max = 30)

str(k_means)

#show colorpalette
#install.packages("scales")
library(scales)
rgb_values <- rgb(k_means$centers)
show_col(rgb_values)
