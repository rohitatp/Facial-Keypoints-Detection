load('data.Rd')
library(reshape2)
library(foreach)

im <- matrix(data=rev(im.train[1,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

# Add points at keypoints in the image

points(96-training$nose_tip_x[1],         96-training$nose_tip_y[1],         col="red")
points(96-training$left_eye_center_x[1],  96-training$left_eye_center_y[1],  col="blue")
points(96-training$right_eye_center_x[1], 96-training$right_eye_center_y[1], col="green")

for(i in 1:nrow(training)) {
    points(96-training$nose_tip_x[i], 96-training$nose_tip_y[i], col="red")
}

# Another example that shows that there is lot of error in nose data points
idx <- which.max(training$nose_tip_x)
im  <- matrix(data=rev(im.train[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-training$nose_tip_x[idx], 96-training$nose_tip_y[idx], col="red")

# Building the first estimator - use mean of training set as the predictor for test set
colMeans(training, na.rm=T)

p <- matrix(data=colMeans(training, na.rm=T), nrow=nrow(test), ncol=ncol(training), byrow=T)
colnames(p) <- names(training)
predictions <- data.frame(ImageId = 1:nrow(test), p)
head(predictions)

#rowid = 1:nrow(predictions)
#predictions <- cbind(rowid, predictions)
#names(predictions)[1] <- "Row Id"
#names(predictions)[2] <- "Image Id"
#submission <- melt(predictions, id.vars="Row Id", "Image Id", variable.name="Feature Name", value.name="Location")

submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(submission)

example.submission <- read.csv('IdLookupTable.csv')
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]
write.csv(submission, file="submission_means.csv", quote=F, row.names=F)

# Using image patches for a better estimator. We use an image patch around left eye center
# patch size is 10 pixels

coord      <- "left_eye_center"
patch_size <- 10
search_size <- 2

# list the coordinates we have to predict
coordinate.names <- gsub("_x", "", names(training)[grep("_x", names(training))])

# for each one, compute the average patch
mean.patches <- foreach(coord = coordinate.names) %dopar% {
	cat(sprintf("computing mean patch for %s\n", coord))
	coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")

	# compute average patch
	patches <- foreach (i = 1:nrow(training), .combine=rbind) %do% {
		im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
		x   <- training[i, coord_x]
		y   <- training[i, coord_y]
		x1  <- (x-patch_size)
		x2  <- (x+patch_size)
		y1  <- (y-patch_size)
		y2  <- (y+patch_size)
		if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
		{
			as.vector(im[x1:x2, y1:y2])
		}
		else
		{
			NULL
		}
	}
	matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
}

# list the coordinates we have to predict
coordinate.names <- gsub("_x", "", names(training)[grep("_x", names(training))])

# for each coordinate and for each test image, find the position that best correlates with the average patch
p <- foreach(coord_i = 1:length(coordinate.names), .combine=cbind) %dopar% {
	# the coordinates we want to predict
	coord   <- coordinate.names[coord_i]
	coord_x <- paste(coord, "x", sep="_")
	coord_y <- paste(coord, "y", sep="_")

	# the average of them in the training set (our starting point)
	mean_x  <- mean(training[, coord_x], na.rm=T)
	mean_y  <- mean(training[, coord_y], na.rm=T)

	# search space: 'search_size' pixels centered on the average coordinates 
	x1 <- as.integer(mean_x)-search_size
	x2 <- as.integer(mean_x)+search_size
	y1 <- as.integer(mean_y)-search_size
	y2 <- as.integer(mean_y)+search_size

	# ensure we only consider patches completely inside the image
	x1 <- ifelse(x1-patch_size<1,  patch_size+1,  x1)
	y1 <- ifelse(y1-patch_size<1,  patch_size+1,  y1)
	x2 <- ifelse(x2+patch_size>96, 96-patch_size, x2)
	y2 <- ifelse(y2+patch_size>96, 96-patch_size, y2)

	# build a list of all positions to be tested
	params <- expand.grid(x = x1:x2, y = y1:y2)

	# for each image...
	r <- foreach(i = 1:nrow(test), .combine=rbind) %do% {
		if ((coord_i==1)&&((i %% 100)==0)) { cat(sprintf("%d/%d\n", i, nrow(test))) }
		im <- matrix(data = im.test[i,], nrow=96, ncol=96)

		# ... compute a score for each position ...
		r  <- foreach(j = 1:nrow(params), .combine=rbind) %do% {
			x     <- params$x[j]
			y     <- params$y[j]
			p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
			score <- cor(as.vector(p), as.vector(mean.patches[[coord_i]]))
			score <- ifelse(is.na(score), 0, score)
			data.frame(x, y, score)
		}

		# ... and return the best
		best <- r[which.max(r$score), c("x", "y")]
	}
	names(r) <- c(coord_x, coord_y)
	r
}

# prepare file for submission
predictions        <- data.frame(ImageId = 1:nrow(test), p)
submission         <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
example.submission <- read.csv('IdLookupTable.csv')
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL

submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]

write.csv(submission, file="submission_search.csv", quote=F, row.names=F)
