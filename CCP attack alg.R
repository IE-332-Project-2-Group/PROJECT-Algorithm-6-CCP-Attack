library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)

setwd("/Users/surre/Documents")

model_list <- load_model_tf("./dandelion_model")
model <- model_list

target_size <- c(224, 224)
res <- data.frame(file = character(), class = character(), percent_dandelion = numeric(), percent_grass = numeric(), stringsAsFactors = FALSE)

change_brightness <- function(x, alpha, beta) {
  new_image <- array(0L, dim = dim(x))
  new_image <- pmax(pmin(alpha*x + beta, 255), 0)
  return(new_image)
}

ccp_attack <- function(x, trans) {
  img <- x
  for (channel in seq_len(dim(x)[3])) {
    r <- x[ , , 1]
    g <- x[ , , 2]
    b <- x[ , , 3]
    
    temp = r * trans[channel][1] + g * trans[channel][2] + b * trans[channel][3]
    img[ , , channel] <- temp/3
    
    img1 <- change_brightness(img, 1, 30)
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
  
  
  #CCP attack
  
  
  
  
  
  
  pred <- model %>% predict(x)
  if (pred[1,2] < 0.50){
    res <- rbind(res, data.frame(file = i, class = "not_grass", percent_dandelion = pred[1,1], percent_grass = 1- pred[1,1]))
  } else {
    res <- rbind(res, data.frame(file = i, class = "grass", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  }
}

f <- list.files("./dandelions")
for (i in f){
  test_image <- image_load(paste("./dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if (pred[1,1] < 0.50){
    res <- rbind(res, data.frame(file = i, class = "not_dandelion", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  } else {
    res <- rbind(res, data.frame(file = i, class = "dandelion", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  }
}

print(res)