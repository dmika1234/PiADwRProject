library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)


#Tworzenie Tabeli 1
#====================================================================================================
#Ładowanie plików
seasons_files <- list.files("D:/Studia/PiADwR/PiADwRProject/Datasets/Sezony", full.names = TRUE)
s_dfs <- lapply(seasons_files, fread)
#Dodanie sezonóW
for( i in 1:10){
  
  s_dfs[[i]] <- s_dfs[[i]][, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HTHG"
                               , "HTAG", "HTR","Referee", "HS","AS", "HST", "AST", "HF",
                                 "AF", "HC", "AC", "HY", "AY", "HR", "AR" )]
  s_dfs[[i]][, Season := paste(2007+i, 2008+i, sep = "/")]
  }
kolumny <- colnames(s_dfs[[1]])[1:23]

#Łączenie tabel
dt <- Reduce(function(...) merge(..., all.x = TRUE, all.y = TRUE, by = kolumny), s_dfs)
#Zmiana Dat
dt[, Date := dmy(Date)]
#Stworzenie nowej tabeli
write.table(dt, "D:/Studia/PiADwR/ProjekTGłówny/Joined_Data_mod.csv", sep="\t")
#=========================================================================================================
