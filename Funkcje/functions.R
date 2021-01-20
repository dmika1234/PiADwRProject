library(data.table)

# funkcje do oceny sezonowej, bukmacherskiej i przedmeczowej

Season_Table <- function(season, data = dt){
  dt_long <- melt(data,
                  id.vars = setdiff(colnames(data), c("HomeTeam", "AwayTeam")),
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

Last_Games_Results <- function(team_1, team_2 = NA, data = dt){
  if(is.na(team_2))
    last_games <- data[HomeTeam == team_1 | AwayTeam == team_1, 
                       .(Date, `Home Team` = HomeTeam, FTHG, FTAG, `Away Team` = AwayTeam,
                         `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  else
    last_games <- data[(HomeTeam == team_1 & AwayTeam == team_2) | (HomeTeam == team_2 & AwayTeam == team_1), 
                       .(Date, `Home Team` = HomeTeam, FTHG, FTAG, `Away Team` = AwayTeam,
                         `Winner` = ifelse(FTR == "H", HomeTeam, ifelse(FTR == "A", AwayTeam, "Draw")))][order(-Date)]
  if(dim(last_games)[1] == 0) 
    last_games <- "No matches available for the given teams."
  return(last_games)
}

Last_Games_Stats <- function(team_1, team_2 = NA, data = dt, num_of_games = 8){
  form <- head(Last_Games_Results(team_1 = team_1, team_2 = team_2), num_of_games)
  if(!is.character(form)){
    form_long <- melt(form,
                      id.vars = setdiff(colnames(form), c("Home Team", "Away Team")),
                      measure.vars = c("Home Team", "Away Team"),
                      variable.name = "Stadium", value.name = "Team",
                      variable.factor = FALSE)
    form <- form_long[`Team` != team_1, .(`Opposing Team` = `Team`, 
                                          `Scored` = ifelse(Stadium == "Home Team", FTAG, FTHG),
                                          `Conceded` = ifelse(Stadium == "Home Team", FTHG, FTAG),
                                          Winner)]
    if(is.na(team_2)) {
      outcome <- list("Scored(Avg)" = mean(form$`Scored`), "Conceded(Avg)" = mean(form$`Conceded`), 
                      "Wins" = sum(form$`Winner` == team_1), "Draws" = sum(form$`Winner` == "Draw"), 
                      "Loses" = num_of_games - (sum(form$`Winner` == team_1) + sum(form$`Winner` == "Draw")))
    } else {
      outcome <- list(mean(form$`Scored`), mean(form$`Conceded`), sum(form$`Winner` == team_1), sum(form$`Winner` == "Draw"), 
                      num_of_games - (sum(form$`Winner` == team_1) + sum(form$`Winner` == "Draw")))
      names(outcome) <- c(paste("Scored(Avg)", team_1), paste("Scored(Avg)", team_2), paste("Wins", team_1), paste("Wins", team_2), "Draws")
    }
  } else outcome <- form
  return(outcome)
}

Home_Away_Stats <- function(team, place, data = dt){
  if(place == "Home"){
    table <- data[HomeTeam == team, .(`Matches` = .N, 
                                      `Scored(Avg)` = round(mean(FTHG), 2),
                                      `Conceded(Avg)` = round(mean(FTAG), 2),
                                      `Wins` = sum(FTR == "H"),
                                      `Draws` = sum(FTR == "D"),
                                      `Loses` = sum(FTR == "A"),
                                      `Wins(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2),
                                      `Loses(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2))]
  } else if (place == "Away"){
    table <- data[AwayTeam == team, .(`Matches` = .N, 
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

Stats <- function(team, data = dt){
  seasons <- unique(data$Season)
  positions <- sapply(seasons, function(x){
    table <- Season_Table(x)
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
              "Scored (Home)" = home$`Scored(Avg)`, "Scored (Away)" = away$`Scored(Avg)`,
              "Conceded (Home)" = home$`Conceded(Avg)`, "Conceded (Away)" = away$`Conceded(Avg)`,
              "Wins(%) (Home)" = home$`Wins(%)`, "Wins(%) (Away)" = away$`Wins(%)`,
              "Loses(%) (Home)" = home$`Loses(%)`, "Loses(%) (Away)" = away$`Loses(%)`)) 
}

Prediction <- function(home_team, away_team, data = dt){
  home_team_last_games_stats <- Last_Games_Stats(team_1 = home_team, num_of_games = 8)
  home_team_stats <- Stats(team = home_team)
  away_team_last_games_stats <- Last_Games_Stats(team_1 = away_team, num_of_games = 8)
  away_team_stats <- Stats(team = away_team)
  face_to_face_stats <- Last_Games_Stats(team_1 = home_team, team_2 = away_team, num_of_games = 4)
  if(!is.character(face_to_face_stats)){
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
  set.seed(24)
  predicted_FTHG <- rpois(n = 1000, lambda = lambda_home_team)
  predicted_FTAG <- rpois(n = 1000, lambda = lambda_away_team)
  predicted_results <- factor(ifelse(predicted_FTHG > predicted_FTAG, "H", ifelse(predicted_FTHG == predicted_FTAG, "D", "A")), levels = c("H", "D", "A"))
  summarized_results <- summary(predicted_results)/10
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

# funkcja wyswietlajaca tabele dla podanego czasu (jak Season_Table ale nie dla sezonu tylko od ustalonego dnia do ustalonego dnia)

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

# Season_Table ----

# Podajemy sezon w takiej formie jak dodal je Dominik
# Funkcja wypluwa tabele na koniec sezonu, tzn 
# - przypisuje druzynom punkty za kazdy mecz
# - zlicza strzelone i stracone gole
# - sortuje druzyny w kolejnosci: punkty, roznica bramek, gole strzelone
# mozliwe ze ostatnia kolumna bedzie niepotrzebna do wyswietlenia w aplikacji

Season_Table(s)

# Last_Games_Results ----

# mozna podac jedna lub dwie druzyny
# w przypadku jednej wypluwa wszystkie jej mecze 
# w przypadku dwoch wszystkie mecze pomiedzy nimi
# w aplikacji trzeba bedzie brac odpowiedni head() od wyniku, zalezy ile ostatnich meczow bedziemy chcieli pokazac

# wszystkie mecze Bournemouth
Last_Games_Results(team_1 = t3) 
# ostatnie mecze Lpoolu
head(Last_Games_Results(team_1 = t1), 5)
# ostatnie mecze Lpoolu z ManUtd
head(Last_Games_Results(team_1 = t1, team_2 = t2), 5)
# kolejnosc podania druzyn nie ma znaczenia, wynik jest taki sam
head(Last_Games_Results(team_1 = t2, team_2 = t1), 5)
# ostatnie mecze Bournemouth z Boltonem, ktore w naszych danych nie graly ze soba
head(Last_Games_Results(team_1 = t3, team_2 = t4), 5)

# Last_Games_Stats ----

# podstawowe informacje o ostatniech meczach
# podobnie jak Last_Games_Results mozna wywolac dla 1 albo 2 druzyn
# tutaj trzeba podac w funkcji ile meczow nas interesuje, domyslnie 8

# informacje dla Liverpoolu
Last_Games_Stats(team_1 = t1) 
# info dla ostatnich 10 meczow Lpool vs Manu
Last_Games_Stats(team_1 = t1, team_2 = t2, num_of_games = 10)
# znowu przypadek Boltonu i Bournemouth
Last_Games_Stats(team_1 = t3, team_2 = t4)

# Home_Away_Stats ----

# statystyki podanej druzyny w domu/ na wyjezdzie

# dla Lpoolu:
Home_Away_Stats(team = t1, "Home")
Home_Away_Stats(team = t1, "Away")

# Stats ----

# dla Lpoolu
Stats(team = t1)

# mozna uzyc do porownania
comparison <- cbind(Stats(t1), Stats(t2))
colnames(comparison) <- c(t1, t2)
comparison

# Prediction ----

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
 
Prediction(t1, t2)[[1]]
Prediction(t1, t2)[[2]]

# w przypadku druzyn ktore ze soba nie graly tak samo tylko nie uwzgledniamy poprzednich meczow miedzy nimi
Prediction(t3, t4)

# Percent_of_Wins ----

Percent_of_Wins("Away")

# Period_Table ----
Period_Table("2017-03-12", "2017-09-15")
Period_Table()
