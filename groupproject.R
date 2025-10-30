read.csv("groupfile.csv")
# library
library(ggplot2)

# The iris dataset is provided natively by R
#head(iris)

# basic scatterplot
ggplot(iris, aes(x=Sepal, y=Sepal.Width)) + 
  geom_point()