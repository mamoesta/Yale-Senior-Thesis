install.packages("igraph")
install.packages("network") 
install.packages("sna")
install.packages("ndtv")
install.packages("qgraph")
install.packages("corrplot")

edges <- read.csv("edgelistmatrix.csv", as.is=T)
vals <- edges$X
edges <- edges[,-1]
rownames(edges) <- vals
edges <- as.matrix(edges)

library(corrplot)
corrplot(edges,method="color")

M <- cor(mtcars)
class(M)
class(edges)


corrplot(regression_final,method="color", tl.col = "black")


lm5 <- lm(tired_ESM ~ date2 + tempavg, data = esm)
summary(lm5)
