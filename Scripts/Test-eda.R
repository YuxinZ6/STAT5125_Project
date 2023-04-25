emissions<- read.csv("/Users/jennguyen/Documents/Lecture Contents/Spring 2023/STAT 5125/Final Project/Direct_Investment-related_Indicators.csv")
length(emissions)
unique(emissions$Country) # 59 countries
summary(emissions)
summary(is.na(emissions))
table(is.na(emissions))