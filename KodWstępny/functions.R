library(data.table)

Percent_of_Wins <- function(place = "Home"){
  if(place == "Home"){
    table <- dt[, .( `Percent Of Wins` = mean(FTR == "H", na.rm = TRUE),
                     `Num Of Matches` = length(FTR),
                     `Num Of Wins` = sum(FTR == "H")),
                by = .(`Home Team` = HomeTeam)][order(-`Percent Of Wins`)]
  } else if (place == "Away"){
    table <- dt[, .( `Percent Of Wins` = mean(FTR == "A", na.rm = TRUE),
                     `Num Of Matches` = length(FTR),
                     `Num Of Wins` = sum(FTR == "A")),
                by = .(`Away Team` = AwayTeam)][order(-`Percent Of Wins`)]
  }
  return(table)
}

Home_Away_Stats <- function(team, place){
  if(place == "Home"){
    table <- dt[HomeTeam == team, .(`Matches` = .N, 
                                    `Scored(Avg)` = round(mean(FTHG), 2),
                                    `Conceded(Avg)` = round(mean(FTAG), 2),
                                    `Wins` = sum(FTR == "H"),
                                    `Draws` = sum(FTR == "D"),
                                    `Loses` = sum(FTR == "A"),
                                    `Wins(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2),
                                    `Loses(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2))]
  } else if (place == "Away"){
    table <- dt[AwayTeam == team, .(`Matches` = .N, 
                                    `Scored(Avg)` = round(mean(FTAG), 2),
                                    `Conceded(Avg)` = round(mean(FTHG), 2),
                                    `Wins` = sum(FTR == "A"),
                                    `Draws` = sum(FTR == "D"),
                                    `Loses` = sum(FTR == "H"),
                                    `Wins(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2),
                                    `Loses(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2))]
  }
  return(table)
}

Season_Table <- function(season){
  dt_long <- melt(dt,
                  id.vars = setdiff(colnames(dt), c("HomeTeam", "AwayTeam")),
                  measure.vars = c("HomeTeam", "AwayTeam"),
                  variable.name = "Stadium", value.name = "Team",
                  variable.factor = FALSE)
  dt_long$Stadium <- ifelse(dt_long$Stadium == "HomeTeam", "H", "A")
  dt_mod <- dt_long[,.(Team, FTHG, FTAG, FTR, Season, Stadium)]
  dt_mod$Points <- as.integer(with(dt_mod, ifelse((Stadium ==
                                                     "H" & FTR == "H"), 3, ifelse((Stadium == "A" &
                                                                                     FTR == "A"), 3, ifelse(FTR == "D", 1, 0)))))
  table <- dt_mod[Season == season, .(Pts = sum(Points), W = sum(Points == 3), D = sum(Points == 1), L = sum(Points == 0),
                                      GS = sum(ifelse(Stadium == "H", FTHG, FTAG)), GL = sum(ifelse(Stadium == "H", FTAG, FTHG)),
                                      GD = sum(ifelse(Stadium == "H", FTHG, FTAG)) - sum(ifelse(Stadium == "H", FTAG, FTHG))),
                  by = .(Team)][order(-Pts, -GD, -GS)]
  table[, P := 1:.N]
  return(as.data.table(table))
}

Last_Games <- function(team_1, team_2 = NA){
  if(is.na(team_2))
    last_games <- dt[HomeTeam == team_1 | AwayTeam == team_1, 
                     .( Date, `Home Team` = HomeTeam, `Score` = paste0(FTHG, ":", FTAG), `Away Team` = AwayTeam,
                        `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  else
    last_games <- dt[(HomeTeam == team_1 & AwayTeam == team_2) | (HomeTeam == team_2 & AwayTeam == team_1), 
                     .( Date, `Home Team` = HomeTeam, `Score` = paste0(FTHG, ":", FTAG), `Away Team` = AwayTeam,
                        `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  if(dim(last_games)[1] == 0) 
    last_games <- "No matches available for the given teams."
  return(last_games)
}

Stats <- function(team){
  seasons <- unique(dt$Season)
  positions <- sapply(seasons, function(x){
    table <- season_table(x)
    position <- table[Team == team, P]
    if(length(position) == 0)
      position <- NA
    return(position)
  })
  positions <- as.vector(positions)
  home <- Home_Away_Stats(team, "Home")
  away <- Home_Away_Stats(team, "Away")
  return(list("Seasons in Premier League" = sum(!is.na(positions)), "Best Position" = min(positions, na.rm = TRUE), 
              "Championships" = sum(positions == 1, na.rm = TRUE),
              "Scored (Home)" = home$`Scored(Avg)`, "Scored (Away)" = away$`Scored(Avg)`)) 
}

