library(png)
library(jpeg)
library(grid)
#img <- readPNG("Trump_R.png")
#img <- readPNG("my_dad.png")
img <- readJPEG("alan_wake_icon.jpg")
graph_special_image <- rasterGrob(img, interpolate=TRUE)
