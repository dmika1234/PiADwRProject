library(skellam)
library(data.table)

# funkcje do oceny sezonowej, bukmacherskiej i przedmeczowej

summary_season <- function(season, data = dt){
  dt_long <- melt(data,
                  id.vars = setdiff(colnames(data), c("HomeTeam", "AwayTeam")),
                  measure.vars = c("HomeTeam", "AwayTeam"),
                  variable.name = "Stadium", value.name = "Team",
                  variable.factor = FALSE)
  dt_long[, Points := ifelse((Stadium == "HomeTeam" & FTR == "H"), 3, 
                                                  ifelse((Stadium == "AwayTeam" & FTR == "A"), 3, 
                                                         ifelse(FTR == "D", 1, 0)))]
  season_table <- dt_long[Season == season, .(Pts = sum(Points), W = sum(Points == 3), D = sum(Points == 1), L = sum(Points == 0),
                                      GS = sum(ifelse(Stadium == "HomeTeam", FTHG, FTAG)), GL = sum(ifelse(Stadium == "HomeTeam", FTAG, FTHG)),
                                      GD = sum(ifelse(Stadium == "HomeTeam", FTHG, FTAG)) - sum(ifelse(Stadium == "HomeTeam", FTAG, FTHG)),
                                      `shots` = mean(ifelse(Stadium == "HomeTeam", HS, AS)), `rival shots` = mean(ifelse(Stadium == "HomeTeam", AS, HS)),
                                      `shots accuracy` = sum(ifelse(Stadium == "HomeTeam", HST, AST))/sum(ifelse(Stadium == "HomeTeam", HS, AS)),
                                      `fouls` = mean(ifelse(Stadium == "HomeTeam", HF, AF)), `fouled` = mean(ifelse(Stadium == "HomeTeam", AF, HF)),
                                      `corners` = mean(ifelse(Stadium == "HomeTeam", HC, AC)),
                                      `YC` = sum(ifelse(Stadium == "HomeTeam", HY, AY)), `RC` = sum(ifelse(Stadium == "HomeTeam", HR, AR))),
                  by = .(Team)][order(-Pts, -GD, -GS)]
  season_table[, P := 1:.N]
  return(as.data.table(season_table))
}

all_games <- function(team_1, team_2 = NULL, data = dt){
  if(is.null(team_2))
    matches <- data[HomeTeam == team_1 | AwayTeam == team_1, 
                       .(Date, `Home Team` = HomeTeam, FTHG, FTAG, `Away Team` = AwayTeam,
                         `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  else
    matches <- data[(HomeTeam == team_1 & AwayTeam == team_2) | (HomeTeam == team_2 & AwayTeam == team_1), 
                       .(Date, `Home Team` = HomeTeam, FTHG, FTAG, `Away Team` = AwayTeam,
                         `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  if(dim(matches)[1] == 0) 
    # matches <- "No matches available for the given teams."
    matches <- NA
  return(matches)
}

last_games_stats <- function(team_1, team_2 = NULL, data = dt, num_of_games = 8){
  matches <- head(all_games(team_1 = team_1, team_2 = team_2), num_of_games)
  if(!any(is.na(matches))){
    matches_long <- melt(matches,
                      id.vars = setdiff(colnames(matches), c("Home Team", "Away Team")),
                      measure.vars = c("Home Team", "Away Team"),
                      variable.name = "Stadium", value.name = "Team",
                      variable.factor = FALSE)
    matches <- matches_long[`Team` != team_1, .(`Rival` = `Team`, 
                                          `Scored` = ifelse(Stadium == "Home Team", FTAG, FTHG),
                                          `Conceded` = ifelse(Stadium == "Home Team", FTHG, FTAG),
                                          Winner)]
    if(is.null(team_2)){
      outcome <- list("Scored(Avg)" = mean(matches$`Scored`), "Conceded(Avg)" = mean(matches$`Conceded`), 
                      "Wins" = sum(matches$`Winner` == team_1), "Draws" = sum(matches$`Winner` == "Draw"), 
                      "Loses" = num_of_games - (sum(matches$`Winner` == team_1) + sum(matches$`Winner` == "Draw")))
    } else{
      outcome <- list(mean(matches$`Scored`), mean(matches$`Conceded`), sum(matches$`Winner` == team_1), sum(matches$`Winner` == "Draw"), 
                      num_of_games - (sum(matches$`Winner` == team_1) + sum(matches$`Winner` == "Draw")))
      names(outcome) <- c(paste("Scored(Avg)", team_1), paste("Scored(Avg)", team_2), paste("Wins", team_1), paste("Wins", team_2), "Draws")
    }
  } else outcome <- matches
  return(outcome)
}

home_away_stats <- function(team, place, data = dt){
  if(place == "Home"){
    stats_table <- data[HomeTeam == team, .(`Matches` = .N, 
                                      `Scored(Avg)` = round(mean(FTHG), 2),
                                      `Conceded(Avg)` = round(mean(FTAG), 2),
                                      `Wins` = sum(FTR == "H"),
                                      `Draws` = sum(FTR == "D"),
                                      `Loses` = sum(FTR == "A"),
                                      `Wins(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2),
                                      `Loses(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2))]
  } else if (place == "Away"){
    stats_table <- data[AwayTeam == team, .(`Matches` = .N, 
                                      `Scored(Avg)` = round(mean(FTAG), 2),
                                      `Conceded(Avg)` = round(mean(FTHG), 2),
                                      `Wins` = sum(FTR == "A"),
                                      `Draws` = sum(FTR == "D"),
                                      `Loses` = sum(FTR == "H"),
                                      `Wins(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2),
                                      `Loses(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2))]
  }
  return(stats_table)
}

all_time_stats <- function(team, data = dt){
  seasons <- unique(data$Season)
  positions <- sapply(seasons, function(x){
    table <- summary_season(x)
    position <- table[Team == team, P]
    if(length(position) == 0)
      position <- NA
    return(position)
  })
  positions <- as.vector(positions)
  home <- home_away_stats(team, "Home")
  away <- home_away_stats(team, "Away")
  return(list("Seasons in Premier League" = sum(!is.na(positions)), "Best Position" = min(positions, na.rm = TRUE), 
              "Championships" = sum(positions == 1, na.rm = TRUE),
              "Scored (Home)" = home$`Scored(Avg)`, "Scored (Away)" = away$`Scored(Avg)`,
              "Conceded (Home)" = home$`Conceded(Avg)`, "Conceded (Away)" = away$`Conceded(Avg)`,
              "Wins(%) (Home)" = home$`Wins(%)`, "Wins(%) (Away)" = away$`Wins(%)`,
              "Loses(%) (Home)" = home$`Loses(%)`, "Loses(%) (Away)" = away$`Loses(%)`)) 
}

predict_match <- function(home_team, away_team, data = dt){
  if(home_team == away_team) {return(NA)}
  home_team_last_games_stats <- last_games_stats(team_1 = home_team, num_of_games = 8)
  home_team_stats <- all_time_stats(team = home_team)
  away_team_last_games_stats <- last_games_stats(team_1 = away_team, num_of_games = 8)
  away_team_stats <- all_time_stats(team = away_team)
  face_to_face_stats <- last_games_stats(team_1 = home_team, team_2 = away_team, num_of_games = 4)
  if(!any(is.na(face_to_face_stats))){
    lambda_home_team <- mean(c(home_team_last_games_stats$`Scored(Avg)`, away_team_last_games_stats$`Conceded(Avg)`,
                               home_team_stats$`Scored (Home)`, away_team_stats$`Conceded (Away)`, face_to_face_stats[[1]]))
    lambda_away_team <- mean(c(away_team_last_games_stats$`Scored(Avg)`, home_team_last_games_stats$`Conceded(Avg)`,
                               away_team_stats$`Scored (Away)`, home_team_stats$`Conceded (Home)`, face_to_face_stats[[2]]))
  } else {
    lambda_home_team <- mean(c(home_team_last_games_stats$`Scored(Avg)`, away_team_last_games_stats$`Conceded(Avg)`,
                               home_team_stats$`Scored (Home)`, away_team_stats$`Conceded (Away)`))
    lambda_away_team <- mean(c(away_team_last_games_stats$`Scored(Avg)`, home_team_last_games_stats$`Conceded(Avg)`,
                               away_team_stats$`Scored (Away)`, home_team_stats$`Conceded (Home)`))
  }
  home_team_win_prob <- round(1 - pskellam(0, lambda_home_team, lambda_away_team), 3)
  draw_prob <- round(dskellam(0, lambda_home_team, lambda_away_team), 3)
  away_team_win_prob <- round(pskellam(0, lambda_home_team, lambda_away_team) - draw_prob, 3)
  summarized_results <- c(home_team_win_prob, draw_prob, away_team_win_prob) * 100
  names(summarized_results) <- c(home_team, "Draws", away_team)
  predicted_result <- c(round(lambda_home_team, 2), round(lambda_away_team, 2))
  names(predicted_result) <- c(home_team, away_team)
  return(list("Probability" = summarized_results, "Expected goals" = predicted_result))
}


##mecze druzyny w sezonie##
season_matches <- function(season, team, data = dt){
  
  sample_dts <- dt[Season == season & (HomeTeam == team | AwayTeam == team)]
  
  return(sample_dts)
}
##


##Daty meczy druzyny w sezonie##
match_dates <- function(season, team, data = dt){
  
  sample_dts <- season_matches(season, team, data)
  match_dates <- sort(sample_dts[, Date])
  
  return(match_dates)
  
}
##




#### funkcje ktore ostatecznie raczej sie nie przydadza ####

# funkcja obliczjaca procent wygranych w domu/ na wyjezdzie

Percent_of_Wins <- function(place = "Home", data = dt){
  if(place == "Home"){
    table <- data[, .( `Percent Of Wins` = mean(FTR == "H", na.rm = TRUE),
                       `Num Of Matches` = length(FTR),
                       `Num Of Wins` = sum(FTR == "H")),
                  by = .(`Home Team` = HomeTeam)][order(-`Percent Of Wins`)]
  } else if (place == "Away"){
    table <- data[, .( `Percent Of Wins` = mean(FTR == "A", na.rm = TRUE),
                       `Num Of Matches` = length(FTR),
                       `Num Of Wins` = sum(FTR == "A")),
                  by = .(`Away Team` = AwayTeam)][order(-`Percent Of Wins`)]
  }
  return(table)
}

# funkcja wyswietlajaca tabele dla podanego czasu (jak summary_season ale nie dla sezonu tylko od ustalonego dnia do ustalonego dnia)

Period_Table <- function(begin = min(dt$Date), end = max(dt$Date)){
  dt_long <- melt(dt,
                  id.vars = setdiff(colnames(dt), c("HomeTeam", "AwayTeam")),
                  measure.vars = c("HomeTeam", "AwayTeam"),
                  variable.name = "Stadium", value.name = "Team",
                  variable.factor = FALSE)
  dt_long$Stadium <- ifelse(dt_long$Stadium == "HomeTeam", "H", "A")
  dt_mod <- dt_long[,.(Team, FTHG, FTAG, FTR, Date, Stadium)]
  dt_mod$Points <- as.integer(with(dt_mod, ifelse((Stadium ==
                                                     "H" & FTR == "H"), 3, ifelse((Stadium == "A" &
                                                                                     FTR == "A"), 3, ifelse(FTR == "D", 1, 0)))))
  table <- dt_mod[Date <= end & Date >= begin, .(Matches = length(FTR), Pts = sum(Points), W = sum(Points == 3), D = sum(Points == 1), L = sum(Points == 0),
                                                 GS = sum(ifelse(Stadium == "H", FTHG, FTAG)), GL = sum(ifelse(Stadium == "H", FTAG, FTHG)),
                                                 GD = sum(ifelse(Stadium == "H", FTHG, FTAG)) - sum(ifelse(Stadium == "H", FTAG, FTHG))),
                  by = .(Team)][order(-Pts, -GD, -GS)]
  return(table)
}






#### przykladowe uzycie funkcji ####

t1 <- "Liverpool"
t2 <- "Man United"
t3 <- "Bournemouth"
t4 <- "Bolton"
s <- "2011/2012"

# summary_season ----

# Podajemy sezon w takiej formie jak dodal je Dominik
# Funkcja wypluwa tabele na koniec sezonu, tzn 
# - przypisuje druzynom punkty za kazdy mecz
# - zlicza strzelone i stracone gole
# - sortuje druzyny w kolejnosci: punkty, roznica bramek, gole strzelone
# mozliwe ze ostatnia kolumna bedzie niepotrzebna do wyswietlenia w aplikacji

summary_season(s)

# all_games ----

# mozna podac jedna lub dwie druzyny
# w przypadku jednej wypluwa wszystkie jej mecze 
# w przypadku dwoch wszystkie mecze pomiedzy nimi
# w aplikacji trzeba bedzie brac odpowiedni head() od wyniku, zalezy ile ostatnich meczow bedziemy chcieli pokazac

# wszystkie mecze Bournemouth
all_games(team_1 = t3) 
# ostatnie mecze Lpoolu
head(all_games(team_1 = t1), 5)
# ostatnie mecze Lpoolu z ManUtd
head(all_games(team_1 = t1, team_2 = t2), 5)
# kolejnosc podania druzyn nie ma znaczenia, wynik jest taki sam
head(all_games(team_1 = t2, team_2 = t1), 5)
# ostatnie mecze Bournemouth z Boltonem, ktore w naszych danych nie graly ze soba
head(all_games(team_1 = t3, team_2 = t4), 5)

# last_games_stats ----

# podstawowe informacje o ostatniech meczach
# podobnie jak all_games mozna wywolac dla 1 albo 2 druzyn
# tutaj trzeba podac w funkcji ile meczow nas interesuje, domyslnie 8

# informacje dla Liverpoolu
last_games_stats(team_1 = t1) 
# info dla ostatnich 10 meczow Lpool vs Manu
last_games_stats(team_1 = t1, team_2 = t2, num_of_games = 10)
# znowu przypadek Boltonu i Bournemouth
last_games_stats(team_1 = t3, team_2 = t4)

# home_away_stats ----

# statystyki podanej druzyny w domu/ na wyjezdzie

# dla Lpoolu:
home_away_stats(team = t1, "Home")
home_away_stats(team = t1, "Away")

# all_time_stats ----

# dla Lpoolu
all_time_stats(team = t1)

# mozna uzyc do porownania
comparison <- cbind(all_time_stats(t1), all_time_stats(t2))
colnames(comparison) <- c(t1, t2)
comparison

# predict_match ----

# tu troche nie wiedzialem jak to napisac, pomysl jest taki:
# bramki padaja niezaleznie od siebie w losowych momentach meczu wiec mozemy to przyblizyc rozkladem Poissona
# potrzebujemy dobrac parametr
# chcemy zeby wplyw na niego mialy ostatnie mecze obu druzyn, ich ostatnie mecze miedzy soba i ich skutecznosc w domu/na wyjezdzie
# w takim razie dla druzyny gospodarzy parametr bedzie srednia z:
# - goli strzelonych przez gospodarzy w ostatnich 8 meczach
# - goli straconych przez gosci w ostatnich 8 meczach
# - tego ile historycznie gospodarze strzelaja u siebie
# - tego ile historycznie goscie traca na wyjazdach
# - goli strzelonych przez druzyne gospodarzy w ostatnich 4 bezposrednich meczach z druzyna gosci (bez wzgledu na miejsce rozgrywania meczu)
# dla gosci analogicznie
# parametry sa wartoscia oczekiwana bramek zdobytych przez oba zespoly
# potem generujemy po 1000 zmiennych z rozkladu Poissona dla obu otrzymanych parametrow i zestawiamy je ze soba zeby otrzymac przewidywane wyniki
# obliczamy prawdopodobienstwa zwyciestw konkretnych druzyn

predict_match(t1, t2)[[1]]
predict_match(t1, t2)[[2]]

# w przypadku druzyn ktore ze soba nie graly tak samo tylko nie uwzgledniamy poprzednich meczow miedzy nimi
predict_match(t3, t4)

# Percent_of_Wins ----

Percent_of_Wins("Away")

# Period_Table ----
Period_Table("2017-03-12", "2017-09-15")
Period_Table()
