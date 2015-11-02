#1.The iris data is already in R. Check ?iris.
library(ggplot2)
#(a) run the given codes then write the code to do the same plot but facet by species instead of coloring.
library(MASS)
data("iris")
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point()
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point() + facet_grid(. ~Species)

#(b)run the given codes then write the code to do the same plot but for petal length.
ggplot(iris,aes(y=Sepal.Length,x=Species)) + geom_boxplot()
ggplot(iris,aes(y=Petal.Length,x=Species)) + geom_boxplot()

#(c)run the given codes then write the code to do the same plot but for petal length.
#question is same as the previous one

#(d)The following makes line plots of tree size vs. time, for each tree, faceted by treatment.Produce the same plot, but color lines by size.
data("Sitka")
ggplot(Sitka, aes(x=Time, y=size, group=tree)) + geom_line() + facet_wrap(~treat)
ggplot(Sitka, aes(x=Time, y=size, group=tree, color=size)) + geom_line() + facet_wrap(~treat)

#(e)The following shows the 2d distribution of petal length vs. sepal length in the iris dataset, by making an x-y plot that shows the individual data points as well as contour lines indicating the density of points in a given spatial region.
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point() + geom_density2d()

#2.Compare the following plots, which one do you like better and why?
t.df <- data.frame(x=rnorm(2000), y=rnorm(2000)) 
p.norm <- ggplot(t.df, aes(x,y))
p.norm + geom_point() #a
p.norm + geom_point(shape=1) #b
p.norm + geom_point(shape=".") #c
p.norm + geom_point(colour='black', size =3) #d
p.norm + geom_point(colour='red', size = 1/100) #e
#As the number I give them above, I like b better, because the point size in e is too small so see; the point size in d is too big to seperate them;the point shape is c is not good enough for people to find the distribution; the point shape in a is dot, sometimes if two value close to each other;one will hide another; so overall, b is the best for people to recogonize the distribution and take look at each point.

#3.The dataset mtcars belongs to gglot package. What do the following plots represent? Compare them, which one do you like better and why?
?mtcars
p.tmp <- ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + geom_bar(width=1)
#use bar chart to tell people the number of cars with different number (4,6,8) cylinders in this dataset
p.tmp #print the plot
p.tmp + coord_polar(theta="y") # use the pie chart to indicate the number of cars with different number cylinders in the dataset
p.tmp + coord_polar() # use the bullseye chart to indicate the number of cars with different number cylinders in the dataset
ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) + geom_bar(width=1) + coord_polar()
# use the coxcomb chart to indicate the number of cars with different number cylinders in the dataset.I like the last one most, because I could easily and directly tell which type of car are the most in this dataset based one the last chart







