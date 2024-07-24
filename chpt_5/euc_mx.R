dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Matrix of Euclidean Distances Between Room Centroids----

# Determines the Euclidean distance between room area centroids for use in determining room adjacency for control

# Author(s): Brodie W. Hobson
# Last Modified: Tuesday 23 August 2022 

#----Load packages----

# Load rstudioapi pacakge
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required packages
if ("Rfast" %in% rownames(installed.packages()) == FALSE) {install.packages("Rfast")}
require(Rfast)

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

rm_centroid <- read.csv("./data/rm_centroid.csv")

m <- as.matrix(dist(as.matrix(rm_centroid[,-1]), method = "euclidean"))
colnames(m) <- rm_centroid$Room
rownames(m) <- rm_centroid$Room

#write.csv(m,"./data/euc_mx.csv")