library(caret)

# Load Data while setting the image variable as a string and not as a factor.
training <- read.csv("training.csv", stringsAsFactors=F)
test <- read.csv("test.csv", stringsAsFactors=F)

im.train <- training$Image
training$Image <- NULL
as.integer(unlist(strsplit(im.train[1], " ")))

library(foreach)

im.train <- foreach(im = im.train, .combine=rbind) %do% {
    as.integer(unlist(strsplit(im, " ")))
}

im.test <- foreach(im = test$Image, .combine=rbind) %do% {
    as.integer(unlist(strsplit(im, " ")))
}
test$Image <- NULL

save(training, im.train, test, im.test, file='data.Rd')