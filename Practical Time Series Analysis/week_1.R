help("trees")

#Pairs parameters together and draw each graphs
pairs(trees, pch = 21, bg = c("red"))

#Compute covariance
cov(trees)

#Compute correlation
cor(trees)

