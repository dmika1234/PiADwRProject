library(data.table)
library(ggplot2)




dt <- fread(input = "D:/Studia/PiADwR/PiADwRProject/Datasets/Joined_Data_mod.csv")
dt <- dt[, -1]


dt
t1 <- "Liverpool"
t2 <- "Man United"
t3 <- "Bournemouth"
t4 <- "Bolton"
s <- "2011/2012"


