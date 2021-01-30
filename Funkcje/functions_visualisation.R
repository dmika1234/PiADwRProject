library(data.table)
library(ggplot2)


#================================================OCENA SEZONOWA====================================================



#Funckja przedstawia punkty drużyn pod koniec sezonu===============================================================
season_stat_viz <- function(season, stat){
  
  
  sample_dt <- summary_season(season)
  
  order_vector <- order(sample_dt[, get(stat)], decreasing = TRUE)
  
  sample_dt <- sample_dt[order_vector]
  
  level_order <- sample_dt[, Team]
  
  #mean_pts <- sample_dt[, mean(Pts)]
  
  ggplot(sample_dt, aes(x = get(stat), y = factor(Team, levels = rev(level_order)))) +
    geom_point(size = 8, color = "darkblue") +
    geom_segment(aes(xend = 0, yend = Team), size = 3, color = "darkblue") +
    geom_text(aes(label = round(get(stat), 1)), color = "white", size = 3) +
    #geom_vline(xintercept = mean_pts, color = "grey40", linetype = 3, alpha = 0.5) +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black"),
          axis.title = element_blank(),
          legend.position = "none")
  
}
#przykładowe wywołanie: season_stat_viz("2017/2018", "YC")
#=======================================================================================================================




#Forma druzyny w sezonie-wykres=========================================================================================
plot_season_team_form <- function(season, team, data = dt){
  
  sample_dts <- season_matches(season, team, data)
  
  
  dates <- match_dates(season, team, data)
  matches_count <- length(dates)
  points_fraction <- 1:matches_count
  points <- 1:matches_count
  
  for(i in 1:matches_count){
    
    points[i] <-  summary_season(season, data = sample_dts[Date <= dates[i]])[Team == team, Pts]
    
    if(i == 1){
      form <- points[i]
      
      if(form == 0)
        points_fraction[i] <- -3
      
      if(form == 1)
        points_fraction[i] <- 0
      
      if(form == 3)
        points_fraction[i] <- 3
      
      
    }
    
    else{
      form <- points[i]-points[i-1]
      
      if(form == 0)
        points_fraction[i] <- points_fraction[i-1]-3
      
      if(form == 1)
        points_fraction[i] <- points_fraction[i-1]
      
      if(form == 3)
        points_fraction[i] <- points_fraction[i-1]+3
    }
    
  }
  
  ggplot() +
    geom_point(aes(x = 1:matches_count, y = points_fraction, color = points_fraction)) +
    geom_line(aes(x = 1:matches_count, y = points_fraction, color = points_fraction)) +
    theme_bw() +
    labs(title = paste(team, "form during season", season), x = "Matches played", y = "Form") +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  
}
#Przykladowe uzycie: plot_season_team_form("2016/2017", "Chelsea", dt)
#=====================================================================================================================








#=================================================OCENA PRZEDMECZOWA====================================================



#Forma druzyny w do daty-wykres==========================================================================================
plot_todate_team_form <- function(season, team, data = dt, date){
  
  sample_dts <- season_matches(season, team, data)
  sample_dts <- sample_dts[Date <= date]
  
  dates <- match_dates(season, team, data)
  dates <- dates[dates<=date]
  matches_count <- length(dates)
  points_fraction <- 1:matches_count
  points <- 1:matches_count
  
  for(i in 1:matches_count){
    
    points[i] <-  summary_season(season, data = sample_dts[Date <= dates[i]])[Team == team, Pts]
    
    if(i == 1){
      form <- points[i]
      
      if(form == 0)
        points_fraction[i] <- -3
      
      if(form == 1)
        points_fraction[i] <- 0
      
      if(form == 3)
        points_fraction[i] <- 3
      
      
    }
    
    else{
      form <- points[i]-points[i-1]
      
      if(form == 0)
        points_fraction[i] <- points_fraction[i-1]-3
      
      if(form == 1)
        points_fraction[i] <- points_fraction[i-1]
      
      if(form == 3)
        points_fraction[i] <- points_fraction[i-1]+3
    }
    
  }
  
  ggplot() +
    geom_point(aes(x = 1:matches_count, y = points_fraction, color = points_fraction)) +
    geom_line(aes(x = 1:matches_count, y = points_fraction, color = points_fraction)) +
    theme_bw() +
    labs(title =  paste(team, "form during season", "to ", date), x = "Matches played", y = "Form") +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  
}
#Przykladowe uzycie: plot_todate_team_form("2011/2012", "Chelsea", dt, "2011-11-11")
#====================================================================================================================






#Funckja przedstawia punkty drużyn do konkretnej daty================================================================
todate_pts_viz <- function(season, date, stat){
  
  
  sample_dt <- summary_season(season, data = dt[Date <= date])
  
  order_vector <- order(sample_dt[, get(stat)], decreasing = TRUE)
  
  sample_dt <- sample_dt[order_vector]
  
  level_order <- sample_dt[, Team]
  
  
  
  
  #mean_pts <- sample_dt[, mean(Pts)]
  
  
  ggplot(sample_dt, aes(x = get(stat), y = factor(Team, levels = rev(level_order)))) +
    geom_point(size = 8, color = "darkblue") +
    geom_segment(aes(xend = 0, yend = Team), size = 3, color = "darkblue") +
    geom_text(aes(label = round(get(stat), 1)), color = "white", size = 3) +
    #geom_vline(xintercept = mean_pts, color = "grey40", linetype = 3, alpha = 0.5) +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black"),
          axis.title = element_blank(),
          legend.position = "none")
  
}

#przykładowe wywołanie: todate_pts_viz("2011/2012", "2012-01-01", "fouled")
#===================================================================================================================



#Bar plot ostatnich wyników meczy====================================================================================
result_barplot <- function(team, num_of_games){
  
  
  last_games <- head(all_games(team_1 = team), num_of_games)
  
  
  for(i in 1:num_of_games){
    
    
    if(last_games[i, Winner != team & Winner != "Draw"])
      last_games[i, Winner := "Lose"]
    if(last_games[i, Winner == team])
      last_games[i, Winner := "Win"]
    
  }
  
  
  
  ggplot(last_games) +
    geom_bar(aes(x = Winner), color = "darkblue", fill = "darkblue") +
    theme_bw() +
    labs(title = paste("Last", num_of_games, team, "match results"), x = "Result", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5))
  
}


#przykładowe wywołanie: result_barplot("Liverpool", 10)
#==================================================================================================================






#=======================================================INNE=======================================================



#barplot wyników ostatnich meczy między dwoma drużynami============================================================
teams_result_barplot <- function(team_1, team_2, num_of_games = 4){
  
  results <- head(all_games(team_1, team_2), num_of_games)
  
  ggplot(results) +
    geom_bar(aes(x = Winner, fill = Winner)) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Results count in last", num_of_games, "games", "between", team_1, "and", team_2), x = "Result", y = "Count")
  
}


#przykładowe wywołanie: teams_result_barplot("Chelsea", "Man United", 10)
teams_result_barplot("Chelsea", "Chelsea", 10)
#==================================================================================================================


