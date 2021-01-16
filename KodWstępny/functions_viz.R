library(ggplot2)
library(data.table)



#Funckja przedstawia punkty drużyn pod koniec sezonu==================================================================
season_pts_viz <- function(season){
  
  sample_dt <- Season_Table(season)
  
  level_order <- sample_dt[, Team]
  
  mean_pts <- sample_dt[, mean(Pts)]
  
   ggplot(sample_dt, aes(x = Pts, y = factor(Team, levels = rev(level_order)))) +
    geom_point(size = 6, color = "darkblue") +
    geom_segment(aes(xend = 0, yend = Team), size = 3, color = "darkblue") +
    geom_text(aes(label = Pts), color = "white", size = 3) +
    #geom_vline(xintercept = mean_pts, color = "grey40", linetype = 3, alpha = 0.5) +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black"),
          axis.title = element_blank(),
          legend.position = "none")
  
  
}

#przykładowe wywołanie: season_pts_viz("2017/2018")

#=====================================================================================================================





