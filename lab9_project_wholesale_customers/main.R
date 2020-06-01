require(kohonen)

#data
data <- read.csv("...\\lab9_project\\data\\Wholesale customers data.csv", sep=",")
data_train <- data#[, c(3:8)]

#data to matrix
data_train_matrix <- as.matrix(scale(data_train))

#som grid definition
som_grid <- somgrid(xdim = 4, ydim = 4, topo="hexagonal")

#train the SOM
system.time(som_model <- som(data_train_matrix, 
                             grid=som_grid, 
                             rlen=100, 
                             alpha=c(0.05,0.01), 
                             keep.data = TRUE ))

plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model, type="dist.neighbours")
plot(som_model, type="codes")

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
#plot(som_model, type = "property", property = data.frame(som_model$codes)[,2], main=names(data.frame(som_model$codes))[2], palette.name = coolBlueHotRed)

var <- 6 #define the variable to plot
var_unscaled <- aggregate(as.numeric(som_model$data[[1]][,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)

mydata <- data.frame(som_model$codes)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 3:8) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(data.frame(som_model$codes))), 6)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

