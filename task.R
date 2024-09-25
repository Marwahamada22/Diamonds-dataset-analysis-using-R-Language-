install.packages("ggplot2")
install.packages("corrplot")

library(ggplot2)
library(corrplot)

install.packages("e1071")
library(e1071)

# get data 
diamond <-diamonds
summary(diamond)
colSums(is.na(diamonds)) #ana hna b3ml check 3la null value
duplicat <- sum(duplicated(diamond))  #ana hna b3ml check 3la duplicat
print(duplicat)

duplicate_column <- names(diamond)[sapply(diamond, function(column) any(duplicated(column)))]
print(duplicate_column ) ##ana hna Display  column names that have duplicates

#diamond <- diamond[!duplicated(diamond), ] 
###
#dups <- which(duplicated(diamond))  #remove duplicated
#newdiamond <- diamond[-c(dups-1,dups),] 
#duplicat <- sum(duplicated(newdiamond))  #ana hna b3ml check 3la duplicat
#print(duplicat)
#str(newdiamond)
###
# إزالة duplicated
newdiamond <- unique(diamond)
nrow(diamond)
nrow(newdiamond)
str(newdiamond)
# check skewness value
skewness(newdiamond$price)
log_price <- log(newdiamond$price)
log_price
                     # start visualization #
                     ##boxplot##
boxplot(newdiamond$price)   # hna price before check skewness
boxplot(newdiamond$log_price) # w hna el price after check skewness
               
                    ##Scatter plot ##
plot(newdiamond$carat ,newdiamond$log_price , col="pink",main = "Carat vs. Price by Cut" , xlab = "Carat",
     ylab= "Price")

                    ##Histogram ##
hist(newdiamond$log_price , col="pink" , main="price Histogram" , xlab= "Price" )

hist(newdiamond$carat , col="pink" , main="carat Histogram" , xlab= "carat" )

hist(newdiamond$depth , col=rgb(red=62 ,green = 180 , blue = 137, maxColorValue = 255 ) , main="depth Histogram" , xlab= "depth"  )

                     ##line plot ##
plot(newdiamond$carat, newdiamond$log_price, type = "l",main = "Depth vs. Log Price", col=rgb(red=62 ,green = 180 , blue = 137, maxColorValue = 255 ),
     xlab = "carat",  ylab = "Log Price")

                      ## bar plot##
barplot(table(diamonds$color, diamonds$clarity),
        beside = TRUE,   
        xlab = " Clarity",   
        ylab = " Count",      
        main = "Clarity vs Color",
        legend = levels(diamonds$color) ,
        col = c(rgb(199, 21, 133, maxColorValue = 255),rgb(62, 180, 150, maxColorValue = 255)))  



barplot(table(diamonds$price, diamonds$clarity),
        beside = TRUE,   
        xlab = " Clarity",   
        ylab = " Count",      
        main = "Clarity vs log_price",
        legend = levels(diamonds$color) ,
        col = c(rgb(180, 51, 102, maxColorValue = 255), rgb(112, 128, 144, maxColorValue = 255)))


                    ###qplot#### 
qplot(x=Clarity , y=log_price   , data = newdiamond ,xlab=" Clarity" ,ylab="log_price" , main = "relations between  Clarity and log_price  " , geom = "point" , color= Clarity)


qplot(x=newdiamond$clarity, y=newdiamond$log_price, data = newdiamond, xlab="Clarity", ylab="Log Price", 
      main = "Relation between Clarity and Log Price", geom = "point", color=newdiamond$clarity)

qplot(x=newdiamond$cut, y=newdiamond$log_price, data = newdiamond, xlab="cut", ylab="Log Price", 
      main = "Relation between cut and Log Price", geom = "point", color=newdiamond$cut)

             ## at the end heatmap to see correlation between numeric feature ###

correlation <- cor(newdiamond[c("depth", "carat", "log_price")])
corrplot(correlation, method = "color", title="heatmap")

