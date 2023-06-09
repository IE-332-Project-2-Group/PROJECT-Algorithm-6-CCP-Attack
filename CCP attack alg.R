#loading necessary libraries
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)

#setting working directory 
setwd("/Users/surre/Documents")

#loading the pre-trained model
model_list <- load_model_tf("./dandelion_model")
model <- model_list

#pre-defining the size of the image
target_size <- c(224, 224)
res <- data.frame(file = character(), class = character(), percent_dandelion = numeric(), percent_grass = numeric(), stringsAsFactors = FALSE)

#assigning a budget of 1% pixels
num_pixels <- 224 * 224
pixels_to_change <- round(num_pixels * 0.01)

#sampling random indices from the defined pixel budget
pixel_indices <- sample(num_pixels, pixels_to_change, replace = FALSE)

#Function 1- distorting the budgeted pixels by assigning scale factor and bias hyperparameters 
change_brightness <- function(img, alpha, beta) {
  new_image <- img
  for (k in pixel_indices) {
    row_index <- ceiling(k / 224) 
    col_index <- k %% 224
    if (col_index == 0){ col_index <- 224}
    new_image[, row_index, col_index ,] <- pmax(pmin(alpha*new_image[, row_index, col_index ,] + beta, 255), 0) #applying the distortion formula
  } 
  
  return(new_image)
}

#Function 2- generating random weights for r, g, b channels for distortion
ccp_attack <- function(x, trans) {
  img <- x
  for (channel in seq_len(dim(x)[3])) {
    for (j in pixel_indices) {
      row_index <- ceiling(j / 224)
      col_index <- j %% 224
      if (col_index == 0){ col_index <- 224}
      r <- img[,row_index,col_index,1]
      g <- img[,row_index,col_index,2]
      b <- img[,row_index,col_index,3]
      temp <- r * trans[1, channel] + g * trans[2, channel] + b * trans[3, channel] 
      img[, row_index, col_index , channel] <- temp/3
      img1 = change_brightness(img, 1, 30)
    }

    return(img1)
  }
  
  
}

f <- list.files("./grass")
for (i in f){
  test_image <- image_load(paste("./grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  
  #generating random weights 
  a <- runif(3, 0, 1) 
  b <- runif(3, 0, 1)
  c <- runif(3, 0, 1)
  trans <- array(c(a, b, c), dim = c(3, 3)) #creating a 3x3 matrix with the randomly generated channel weights
  
  #CCP attack
  img1 <- ccp_attack(x, trans)
  
  
  
  pred <- model %>% predict(img1)
  if (pred[1,2] < 0.50){
    res <- rbind(res, data.frame(file = i, class = "not_grass", percent_dandelion = pred[1,1], percent_grass = 1- pred[1,1]))
  } else {
    res <- rbind(res, data.frame(file = i, class = "grass", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  }
}


f <- list.files("./dandelions")
for (i in f){
  test_image <- image_load(paste("./dandelions/",i,sep=""), target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  
  #generating random weights 
  a <- runif(3, 0, 1)
  b <- runif(3, 0, 1)
  c <- runif(3, 0, 1)
  trans <- array(c(a, b, c), dim = c(3, 3)) #creating a 3x3 matrix with the randomly generated channel weights
  
  #CCP attack
  img1 <- ccp_attack(x, trans)
  
  
  
  pred <- model %>% predict(img1)
  if (pred[1,1] < 0.50){
    res <- rbind(res, data.frame(file = i, class = "not_dandelion", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  } else {
    res <- rbind(res, data.frame(file = i, class = "dandelion", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  }
}
print(res)
View(res)


