#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dt <- fread(input = "Joined_Data_mod.csv")

stats_N <- colnames(Season_Table("2017/2018"))
stats_N <- stats_N[2:length(Stats_N)]
clubs_N <- sort(unique(dt$HomeTeam))
seasons_N <- sort(unique(dt$Season))


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Page",
    ##ZAKLADKA HOME
    tabPanel("HOME", 
        "Projekt został stworzony przez: Adriana Płoszyce, Dominika Mike, Jakuba Sobkowiaka, Mateusza Drobine, Stanisława Banaszka"
                          ),
    ##ZAKLADKA OCENA BUKMACHERSKA
    tabPanel("Ocena bukmacherska",
             verticalLayout(
             sidebarLayout(
                 sidebarPanel(
             selectInput(inputId = "clubs_C_buk", 
                         label = "Wybierz klub",
                         choices = clubs_N,
                         selected = clubs_N[1]),
             
             selectInput(inputId = "opponents_C_buk", 
                         label = "Przeciwnik",
                         choices = clubs_N,
                         selected = clubs_N[2]),
             tableOutput("prediction_gR"),
             tableOutput("prediction_wR")),
             
             mainPanel(
             plotOutput("teams_result_barplot_R_buk"))
             )),
             splitLayout(
                 plotOutput("buk_t1_R"),
                 plotOutput("buk_t2_R")
                 
                 
             )
             
             ),
    
    ##ZAKLADKA OCENA PRZEDMECZOWA
    tabPanel("Ocena przedmeczowa", 
             verticalLayout(

                 splitLayout(
                     plotOutput("todate_pts_viz_R", height = "400px"),
                     
                     plotOutput("plot_todate_team_form_R", height = "400px")
                 )
                ),
                 splitLayout(
                     plotOutput("result_barplot_R", height = "300px"),
                     
                     plotOutput("teams_result_barplot_R" , height = "300px")
                 ),
             
             sliderInput(
                 inputId = "matches_C_before" ,
                 label = "Wybierz ilość meczów",
                 min = 0,
                 max = 40,
                 value = 10,
                 width = "100%"
                 ),
             splitLayout(
                 
                 cellArgs = list(style = "height: 300px"),
                 
                 selectInput(inputId = "clubs_C_before", 
                             label = "Wybierz klub",
                             choices = clubs_N,
                             selected = clubs_N[1]),
                 
                 selectInput(inputId = "opponent_C_before", 
                             label = "Przeciwnik",
                             choices = clubs_N,
                             selected = clubs_N[2]),
                 
                 selectInput(inputId = "seasons_C_before", 
                             label = "Wybierz sezon",
                             choices = seasons_N,
                             selected = seasons_N[1]),
                 
                 dateInput(inputId = "data_C_before", 
                           label = "Do kiedy"),
                 
                 selectInput(inputId = "stats_C_before", 
                             label = "Wybierz statystyke",
                             choices = stats_N,
                             selected = stats_N[1])
                 
                 
             )
             ),
    
    ##ZAKLADKA OCENA SEZONOWA
    tabPanel("Ocena sezonowa",
             
             
             ####season_pts_viz
             sidebarPanel(
                 selectInput(inputId = "stats_C_season", 
                             label = "Wybierz statystyke",
                             choices = stats_N,
                             selected = stats_N[1]),
                 selectInput(inputId = "seasons_C_season", 
                             label = "Wybierz sezon",
                             choices = seasons_N,
                             selected = seasons_N[1]),
             ),
             mainPanel(
                 plotOutput("season_pts_viz_R"),
             ),
             
             
             ####plot_season_team_form
             sidebarPanel(
                 selectInput(inputId = "clubs_C_form", 
                             label = "Wybierz klub",
                             choices = clubs_N,
                             selected = clubs_N[1]),
                 selectInput(inputId = "seasons_C_form", 
                             label = "Wybierz sezon",
                             choices = seasons_N,
                             selected = seasons_N[1]),
             ),
             mainPanel(
                 plotOutput("plot_season_team_form_R"),
             )
             
             
             ),
    
    ##ZAKLADKA OCENA ARBITRA
    tabPanel("Ocena arbitra", "contents")
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(ggplot2)
    library(data.table)
    
    dt <- fread(input = "Joined_Data_mod.csv")
    
    
#funkcje do oceny sezonowej, bukmacherskiej i przedmeczowej===============================================================    
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
    
    Prediction_goals <- function(home_team, away_team, data = dt){
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
        tmp <- cbind(list(home_team, away_team),predicted_result)
        colnames(tmp)<- c("Druzynya", "strzelone bramki")
        return(tmp)
    }
    
    Prediction_wins <- function(home_team, away_team, data = dt){
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
        
        tmp <- cbind(list(home_team, "Remis" ,away_team),summarized_results)
        colnames(tmp)<- c("Wynik", "Szansa")
        return(tmp)
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
    
    
    
    
#=======================================================================================================================
    
    
    ########## FUNKCJE WIZUALIZACYJNE ################    
#Funckja przedstawia punkty drużyn pod koniec sezonu===============================================================
    season_pts_viz <- function(season, stat){
        
        
        sample_dt <- Season_Table(season)
        
        order_vector <- order(sample_dt[, get(stat)], decreasing = TRUE)
        
        sample_dt <- sample_dt[order_vector]
        
        level_order <- sample_dt[, Team]
        
        #mean_pts <- sample_dt[, mean(Pts)]
        
        ggplot(sample_dt, aes(x = get(stat), y = factor(Team, levels = rev(level_order)))) +
            geom_point(size = 6, color = "darkblue") +
            geom_segment(aes(xend = 0, yend = Team), size = 3, color = "darkblue") +
            geom_text(aes(label = get(stat)), color = "white", size = 3) +
            #geom_vline(xintercept = mean_pts, color = "grey40", linetype = 3, alpha = 0.5) +
            theme_classic() +
            theme(axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(color = "black"),
                  axis.title = element_blank(),
                  legend.position = "none")
        
    }
#przykładowe wywołanie: season_pts_viz("2017/2018", "Pts")
#=======================================================================================================================
    
#Forma druzyny w sezonie-wykres=========================================================================================
    plot_season_team_form <- function(season, team, data = dt){
        
        sample_dts <- season_matches(season, team, data)
        
        
        dates <- match_dates(season, team, data)
        matches_count <- length(dates)
        points_fraction <- 1:matches_count
        points <- 1:matches_count
        
        for(i in 1:matches_count){
            
            points[i] <-  Season_Table(season, data = sample_dts[Date <= dates[i]])[Team == team, Pts]
            
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
    #Przykladowe uzycie: plot_season_team_form(s, "Chelsea", dt)
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
            
            points[i] <-  Season_Table(season, data = sample_dts[Date <= dates[i]])[Team == team, Pts]
            
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
    #Przykladowe uzycie: plot_todate_team_form(s, "Chelsea", dt, "2011-11-11")
    #====================================================================================================================
    
    
    
    
    
    
    #Funckja przedstawia punkty drużyn do konkretnej daty================================================================
    todate_pts_viz <- function(season, date, stat){
        
        
        sample_dt <- Season_Table(season, data = dt[Date <= date])
        
        order_vector <- order(sample_dt[, get(stat)], decreasing = TRUE)
        
        sample_dt <- sample_dt[order_vector]
        
        level_order <- sample_dt[, Team]
        
        
        
        
        #mean_pts <- sample_dt[, mean(Pts)]
        
        
        ggplot(sample_dt, aes(x = get(stat), y = factor(Team, levels = rev(level_order)))) +
            geom_point(size = 6, color = "darkblue") +
            geom_segment(aes(xend = 0, yend = Team), size = 3, color = "darkblue") +
            geom_text(aes(label = get(stat)), color = "white", size = 3) +
            #geom_vline(xintercept = mean_pts, color = "grey40", linetype = 3, alpha = 0.5) +
            theme_classic() +
            theme(axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(color = "black"),
                  axis.title = element_blank(),
                  legend.position = "none")
        
    }
    
    #przykładowe wywołanie: todate_pts_viz("2011/2012", "2012-01-01", "Pts")
    #===================================================================================================================
    
    
    
    #Bar plot ostatnich wyników meczy====================================================================================
    result_barplot <- function(team, num_of_games){
        
        
        last_games <- head(Last_Games_Results(team_1 = team), num_of_games)
        
        
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
        
        results <- head(Last_Games_Results(team_1, team_2), num_of_games)
        
        ggplot(results) +
            geom_bar(aes(x = Winner, fill = Winner)) +
            theme_bw() +
            theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
            labs(title = paste("Results count in last ", num_of_games, "games", "between", team_1, "and", team_2), x = "Result", y = "Count")
        
    }
    
    
    #przykładowe wywołanie: teams_result_barplot("Chelsea", "Man United", 10)
    #==================================================================================================================
    
    
    
    ##OCENA PRZEDMOECZOWA
    ##plot_todate_team_form
    output$plot_todate_team_form_R = renderPlot({
    plot_todate_team_form(input$seasons_C_before, input$clubs_C_before, dt, input$data_C_before)
    })
    
    ##todate_pts_viz
    output$todate_pts_viz_R = renderPlot({
    todate_pts_viz(input$seasons_C_before, input$data_C_before, input$stats_C_before)
    })
    
    ##result_barplot
    output$result_barplot_R = renderPlot({
    result_barplot(input$clubs_C_before, input$matches_C_before)
    })
    
    ##teams_result_barplot
    output$teams_result_barplot_R = renderPlot({
        teams_result_barplot(input$clubs_C_before, input$opponent_C_before, input$matches_C_before)
    })
    
    
    ## OCENA SEZONOWA
    ##season_pts_viz
    output$season_pts_viz_R = renderPlot({
        season_pts_viz(input$seasons_C_season, input$stats_C_season)
    })
    
    ##plot_season_team_form
    output$plot_season_team_form_R = renderPlot({
        plot_season_team_form(input$seasons_C_form, input$clubs_C_form)
    })
    
    
    ## OCENA BUKMACHERSKA
    ##prediction
    output$prediction_gR = renderTable({
        Prediction_goals(input$clubs_C_buk, input$opponents_C_buk)
    })
    
    ##prediction
    output$prediction_wR = renderTable({
        Prediction_wins(input$clubs_C_buk, input$opponents_C_buk)
    })
    
    output$teams_result_barplot_R_buk = renderPlot({
        teams_result_barplot(input$clubs_C_buk, input$opponents_C_buk, 10)
    })
    
    ##"buk_t1_R"
    output$buk_t1_R = renderPlot({
        plot_season_team_form("2016/2017", input$clubs_C_buk)
    })
    
    ##
    output$buk_t2_R = renderPlot({
        plot_season_team_form("2016/2017", input$opponents_C_buk)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
