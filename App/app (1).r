library(shiny)
library(skellam)
library(data.table)
library(ggplot2)
library(DT)
library(shinydashboard)
library(shinythemes)


dt <- fread("D:/Studia/PiADwR/ProjekTgłówny/Joined_Data_mod.csv")


################ functions #############################

summary_season <- function(season, data = dt) {
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
                                                `shots` = round(mean(ifelse(Stadium == "HomeTeam", HS, AS)), 2), `rival shots` = round(mean(ifelse(Stadium == "HomeTeam", AS, HS)), 2),
                                                `shots on target` = round(mean(ifelse(Stadium == "HomeTeam", HST, AST)), 2), 
                                                `rival shots on target` = round(mean(ifelse(Stadium == "HomeTeam", AST, HST)), 2),
                                                `shots accuracy` = round(sum(ifelse(Stadium == "HomeTeam", HS, AS))/sum(ifelse(Stadium == "HomeTeam", FTHG, FTAG)), 2),
                                                `fouls` = round(mean(ifelse(Stadium == "HomeTeam", HF, AF)), 2), `fouled` = round(mean(ifelse(Stadium == "HomeTeam", AF, HF)), 2),
                                                `corners` = round(mean(ifelse(Stadium == "HomeTeam", HC, AC)), 2),
                                                `YC` = round(sum(ifelse(Stadium == "HomeTeam", HY, AY)), 2), `RC` = round(sum(ifelse(Stadium == "HomeTeam", HR, AR)), 2)),
                            by = .(Team)][order(-Pts, -GD, -GS)]
    season_table[, P := 1:.N]
    return(as.data.table(season_table))
}

all_time_stats <- function(team, data = dt) {
    seasons <- unique(data$Season)
    positions <- sapply(seasons, function(x) {
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

home_away_stats <- function(team, place, data = dt) {
    if(place == "Home") {
        stats_table <- data[HomeTeam == team, .(`Matches` = .N, 
                                                `Scored(Avg)` = round(mean(FTHG), 2),
                                                `Conceded(Avg)` = round(mean(FTAG), 2),
                                                `Wins` = sum(FTR == "H"),
                                                `Draws` = sum(FTR == "D"),
                                                `Loses` = sum(FTR == "A"),
                                                `Wins(%)` = round(mean(FTR == "H", na.rm = TRUE) * 100, 2),
                                                `Loses(%)` = round(mean(FTR == "A", na.rm = TRUE) * 100, 2))]
    } else if (place == "Away") {
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

predict_match <- function(home_team, away_team, data = dt){
    # if(home_team == away_team) { return(NA) }
    home_team_last_games_stats <- last_games_stats(team_1 = home_team, num_of_games = 8)
    home_team_stats <- all_time_stats(team = home_team)
    away_team_last_games_stats <- last_games_stats(team_1 = away_team, num_of_games = 8)
    away_team_stats <- all_time_stats(team = away_team)
    face_to_face_stats <- last_games_stats(team_1 = home_team, team_2 = away_team, num_of_games = 4)
    if(!any(is.na(face_to_face_stats))) {
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
    names(summarized_results) <- c(home_team, "Draw", away_team)
    predicted_result <- c(round(lambda_home_team, 2), round(lambda_away_team, 2))
    names(predicted_result) <- c(home_team, away_team)
    return(list("Probability" = summarized_results, "Expected goals" = predicted_result))
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
        matches <- NA
    return(matches)
}

last_games_stats <- function(team_1, team_2 = NULL, data = dt, num_of_games = 8){
    matches <- head(all_games(team_1 = team_1, team_2 = team_2), num_of_games)
    if(!any(is.na(matches))) {
        matches_long <- melt(matches,
                             id.vars = setdiff(colnames(matches), c("Home Team", "Away Team")),
                             measure.vars = c("Home Team", "Away Team"),
                             variable.name = "Stadium", value.name = "Team",
                             variable.factor = FALSE)
        matches <- matches_long[`Team` != team_1, .(`Rival` = `Team`, 
                                                    `Scored` = ifelse(Stadium == "Home Team", FTAG, FTHG),
                                                    `Conceded` = ifelse(Stadium == "Home Team", FTHG, FTAG),
                                                    Winner)]
        if(is.null(team_2)) {
            outcome <- list("Scored(Avg)" = mean(matches$`Scored`), "Conceded(Avg)" = mean(matches$`Conceded`), 
                            "Wins" = sum(matches$`Winner` == team_1), "Draws" = sum(matches$`Winner` == "Draw"), 
                            "Loses" = num_of_games - (sum(matches$`Winner` == team_1) + sum(matches$`Winner` == "Draw")))
        } else {
            outcome <- list(mean(matches$`Scored`), mean(matches$`Conceded`), sum(matches$`Winner` == team_1), sum(matches$`Winner` == "Draw"), 
                            num_of_games - (sum(matches$`Winner` == team_1) + sum(matches$`Winner` == "Draw")))
            names(outcome) <- c(paste("Scored(Avg)", team_1), paste("Scored(Avg)", team_2), paste("Wins", team_1), paste("Wins", team_2), "Draws")
        }
    } else outcome <- matches
    return(outcome)
}

period_table <- function(begin = min(dt$Date), end = max(dt$Date), data = dt) {
    dt_long <- melt(data,
                    id.vars = setdiff(colnames(data), c("HomeTeam", "AwayTeam")),
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

season_matches <- function(season, team, data = dt) {
    sample_dts <- dt[Season == season & (HomeTeam == team | AwayTeam == team)]
    return(sample_dts)
}

period_matches <- function(team, date_min, date_max, data = dt) {
    sample_dts <- dt[(Date <= date_max & Date >= date_min) & (HomeTeam == team | AwayTeam == team)][order(Date)]
    return(sample_dts)
}

match_dates <- function(season, team, data = dt) {
    sample_dts <- season_matches(season, team, data)
    match_dates <- sort(sample_dts[, Date])
    return(match_dates)
}

team_and_ref <- function(team, ref, data = dt) {
    table <- data[Referee == ref & (HomeTeam == team | AwayTeam == team),
                  .(Date, HomeTeam, FTHG, FTAG, AwayTeam, HF, AF, HY, AY, HR, AR)][order(-Date)]
    return(table)
}


################ functions to visualization ####################


season_stat_viz <- function(season, stat) {
    
    sample_dt <- summary_season(season)
    order_vector <- order(sample_dt[, get(stat)], decreasing = TRUE)
    sample_dt <- sample_dt[order_vector]
    level_order <- sample_dt[, Team]
    
    ggplot(sample_dt, aes(x = get(stat), y = factor(Team, levels = rev(level_order)))) +
        geom_point(size = 15, color = "darkblue") +
        geom_segment(aes(xend = 0, yend = Team), size = 5, color = "darkblue") +
        geom_text(aes(label = round(get(stat), 2)), color = "white", size = 5) +
        theme_classic() +
        labs(title = paste(stat, "in", season, "season")) +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text = element_text(color = "black", size = 15),
              axis.title = element_blank(),
              legend.position = "none")
    
}

teams_result_barplot <- function(team_1, team_2, num_of_games = 4) {
    
    results <- head(all_games(team_1, team_2), num_of_games)
    
    ggplot(results) +
        geom_bar(aes(x = Winner), color = "darkblue", fill = "darkblue") +
        theme_bw() +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
        labs(title = paste("Results count in last", dim(results)[1], "games", "between", team_1, "and", team_2), x = "Result", y = "Count")
    
}

result_barplot <- function(team, num_of_games){
    
    last_games <- head(all_games(team_1 = team), num_of_games)
    for(i in 1:num_of_games) {
        if(last_games[i, Winner != team & Winner != "Draw"])
            last_games[i, Winner := "Lose"]
        if(last_games[i, Winner == team])
            last_games[i, Winner := "Win"]
    }
    
    ggplot(last_games) +
        geom_bar(aes(x = Winner), color = "darkblue", fill = "darkblue") +
        theme_bw() +
        labs(title = paste("Last", dim(last_games)[1], team, "match results"), x = "Result", y = "Count") +
        theme(plot.title = element_text(hjust = 0.5))
}

plot_to_date_team_form <- function(team_1, team_2, date_min, date_max, data = dt) {
    
    sample_dts_1 <- period_matches(team_1, date_min, date_max, data)
    sample_dts_2 <- period_matches(team_2, date_min, date_max, data)
    
    dates <- apply(data.table(sample_dts_1$Date, sample_dts_2$Date), 1, function(d) { max(d) })
    matches_count <- length(dates)
    form <- as.data.table(sapply(c(team_1, team_2), function(t) {
            points_fraction <- 1:matches_count
            points <- 1:matches_count
            
            for(i in 1:matches_count) {
                
                prd_table <- period_table(date_min, dates[i])
                
                points[i] <-  prd_table[Team == t, Pts]
                
                if(i == 1){
                    form <- points[i]
                    if(form == 0)
                        points_fraction[i] <- -3
                    if(form == 1)
                        points_fraction[i] <- 0
                    if(form == 3)
                        points_fraction[i] <- 3
                } else {
                    form <- points[i]-points[i-1]
                    if(form == 0)
                        points_fraction[i] <- points_fraction[i-1]-3
                    if(form == 1)
                        points_fraction[i] <- points_fraction[i-1]
                    if(form == 3)
                        points_fraction[i] <- points_fraction[i-1]+3
                }
            }
            return(points_fraction)
        })
    )
    form[, match := 1:matches_count]
    form_melted <- melt(form, id.var = "match")
    ggplot(form_melted, aes(x = match, y = value, col = variable)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(title =  paste(team_1, "and", team_2, "form from", date_min, "to", date_max), x = "Matches played", y = "Form", color = "Teams") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "right")
}

ref_results <- function(stat){
    referee_table <- dt[, .(Referee, HomeTeam, AwayTeam, HTR, HF, AF, HY, AY, HR, AR)]
    
    refs <- unique(referee_table$Referee)
    
    count_ref <- length(refs)
    
    redcards = yellowcards = fouls = refsmatches = c()
    
    for (i  in 1:count_ref) {
        refsmatches[i] = sum(referee_table$Referee == refs[i])
        fouls[i] = sum(referee_table[Referee == refs[i]]$HF, referee_table[Referee == refs[i]]$AF)/refsmatches[i]
        yellowcards[i] = sum(referee_table[Referee == refs[i]]$HY, referee_table[Referee == refs[i]]$AY)/refsmatches[i]
        redcards[i] = sum(referee_table[Referee == refs[i]]$HR, referee_table[Referee == refs[i]]$AR)/refsmatches[i]
    }
    
    refs_hist <- data.table(refs, refsmatches, fouls, yellowcards, redcards)
    
    order_refs <- order(refs_hist[, get(stat)], decreasing = TRUE)
    
    refs_hist1 <- refs_hist[order_refs]
    
    level_order <- refs_hist1[, refs]
    
    ggplot(refs_hist1, aes(x = get(stat), y = factor(refs, levels = rev(level_order)))) +
        geom_segment(aes(xend = 0, yend = refs), size = 4, color = "darkblue") +
        theme_classic() +
        labs(x = "Result", y = "Referees") +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text = element_text(color = "black"),
              axis.title = element_text(),
              legend.position = "none")
}


team_and_ref_results <- function(team, ref) {
    referee_table <- dt[, .(Referee, HomeTeam, AwayTeam, HTR, HF, AF, HY, AY, HR, AR)]
    
    W = D = L = 0
    for (i in 1:3800){
        if (referee_table$HomeTeam[i] == team & referee_table$Referee[i] == ref) {
            if (referee_table$HTR[i] == 'H' ) {
                W = W + 1
            }
            if (referee_table$HTR[i] == 'D' ) {
                D = D + 1
            }
            if (referee_table$HTR[i] == 'A' ) {
                L = L + 1
            }
        }
        else if (referee_table$AwayTeam[i] == team & referee_table$Referee[i] == ref) {
            if (referee_table$HTR[i] == 'A' ) {
                W = W + 1
            }
            if (referee_table$HTR[i] == 'D' ) {
                D = D + 1
            }
            if (referee_table$HTR[i] == 'H' ) {
                L = L + 1
            }
        }
    }
    table = c(W, D, L, W + D + L)
    names(table) = c("Wins", "Draws", "Loses", "Matches")
    barplot(table, col = "darkblue",
            main = paste(team, "results when", ref, "was the referee"))
}








#### ui #########

seasons <- sort(unique(dt$Season), decreasing = TRUE)
teams <- sort(unique(dt$HomeTeam), decreasing = FALSE)
statistics <- colnames(summary_season("2017/2018"))
dates <- sort(unique(dt$Date))
referees <- sort(unique(dt$Referee))


ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    navbarPage(title = img(src = "https://png2.cleanpng.com/sh/b2b4999adf98483d729a0e7f1e3dd6a2/L0KzQYm3VsI3N5p2iJH0aYP2gLBuTgBzbZ5ufeQ2bHXkd8bsTfNwdJD3gdDwLXLyf7y0gfxtNaVtfZ95cnXwebb5kBhqeF46eqU7ZXHnQLOChMAxQV88UaI5N0S5R4K8U8AxPmQ6TqoCNkG0PsH1h5==/kisspng-premier-league-coloring-book-all-the-premiership-5b32ead0b9d009.7900746715300635687611.png", 
                           width = 120),
    
        #### Ocena bukmachera (pre-match)  ####             
        
        tabPanel("Pre-match Analysis",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("first_page_select_home_team", "Home Team:",
                                     choices = teams,
                                     selected = "Chelsea"),
                         selectInput("first_page_select_away_team", "Away Team:",
                                     choices = teams,
                                     selected = "Liverpool"),
                         sliderInput("first_page_last_matches", "Number of last matches to show:",
                                     min = 3,
                                     max = 10,
                                     value = 5),
                         actionButton("run_prediction", "Confirm")
                         ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Prediction",
                                fluidRow(
                                    column(width = 3,
                                      fluidRow(tableOutput("expected_goals")),
                                      fluidRow(tableOutput("results_distribution"))),
                                    column(width = 9,
                                      plotOutput("plot_results_distribution"))
                                    ),
                                fluidRow(dataTableOutput("face_to_face_results_table")
                                         )
                                ),
                             tabPanel("Last Games",
                                      fluidRow(
                                          column(width = 6,
                                                 plotOutput("home_team_results_distribution_plot")),
                                          column(width = 6,
                                                 plotOutput("away_team_results_distribution_plot"))),
                                      fluidRow(
                                          column(width = 6,
                                                 dataTableOutput("home_team_last_results_table")),
                                          column(width = 6,
                                                 dataTableOutput("away_team_last_results_table"))
                                          )
                                      )
                             )
                         )
                     )
                 ),
        #### Analiza Sezonu ####               
               
        tabPanel("Season Summary",
                 fluidRow(
                     column(width = 6,
                            style = "background-color: #DFE6EB",
                            column(width = 5, 
                                   selectInput("second_page_selected_season_input", "Season:",
                                               choices = seasons,
                                               selected = max(seasons))
                                   ),
                            column(width = 1, 
                                   actionButton("run_table_for_season_button", "Confirm",
                                                style = ("margin-top: 20px"))
                                   )
                            ),
                     column(width = 6,
                            style = "background-color: #DFE6EB",
                            column(width = 5, 
                                   selectInput("second_page_selected_stat_input", "Statistic:",
                                               choices = c("Points" = "Pts",
                                                           "Wins" = "W",
                                                           "Draws" = "D",
                                                           "Loses" = "L",
                                                           "Goals Scored" = "GS",
                                                           "Goals Conceded" = "GL",
                                                           "Shots (Avg)" = "shots",
                                                           "Rival Shots (Avg)" = "rival shots",
                                                           "Shots On Target (Avg)" = "shots on target",
                                                           "Rival Shots On Target (Avg)" = "rival shots on target",
                                                           "Shots Accuracy" = "shots accuracy",
                                                           "Fouls Committed (Avg)" = "fouls",
                                                           "Fouls Suffered (Avg)" = "fouled",
                                                           "Corners (Avg)" = "corners",
                                                           "Yellow Cards" = "YC",
                                                           "Red Cards" = "RC"),
                                               selected = "Pts")
                                   ),
                            column(width = 1, 
                                   actionButton("run_plot_for_stat_button", "Confirm",
                                                style = ("margin-top: 20px"))
                                   )
                            )
                     ),
                 fluidRow(
                     column(width = 6,
                            dataTableOutput("season_table")
                            ),
                     column(width = 6,
                            plotOutput("season_stat")
                            )
                     )
                 ),
        
        #### porownanie druzyn ####              
        
        tabPanel("Teams Comparison",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("third_page_select_home_team", "Home Team:",
                                     choices = teams,
                                     selected = "Chelsea"),
                         selectInput("third_page_select_away_team", "Away Team:",
                                     choices = teams,
                                     selected = "Liverpool"),
                         dateRangeInput("third_page_date_range", "Date Range:",
                                        min = min(dates),
                                        max = max(dates),
                                        start = max(dates) - 365,
                                        end = max(dates)),
                         actionButton("run_forms", "Confirm"),
                         tableOutput("all_time_comparison")
                         ),
                     mainPanel(
                         plotOutput("teams_form_plot")
                         )
                     )
                 ),
        
        #### Analiza arbitr?w ####
        
        tabPanel("Referees",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("fourth_page_select_statistic", "Statistic:",
                                     choices = c("Fouls (Avg)" = "fouls",
                                                 "Yellow Cards (Avg)" = "yellowcards",
                                                 "Red Cards (Avg)" = "redcards",
                                                 "Refreed Matches" = "refsmatches"),
                                     selected = "fouls"),
                         actionButton("run_referees_comparison", "Confirm"),
                         selectInput("fourth_page_select_referee", "Referee:",
                                     choices = referees,
                                     selected = min(referees)),
                         selectInput("fourth_page_select_team", "Team:",
                                     choices = teams,
                                     selected = min(teams)),
                         actionButton("run_referee_and_team", "Confirm")
                         ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Statistics",
                                      plotOutput("referee_stat")),
                             tabPanel("Referee-Team",
                                      fluidRow(
                                          plotOutput("referee_team")),
                                      fluidRow(
                                          dataTableOutput("referee_team_tab")))
                             )
                         )
                     )
                 ),
        #### info o skrotach ####
        
        tabPanel("Informations",
                 column(width = 6,
                        h3("Notation"),
                        p("FTHG and HG = Full Time Home Team Goals"), 
                        p("FTAG and AG = Full-Time Away Team Goals"), 
                        p("FTR and Res = Full-Time Result (H=Home Win, D=Draw, A=Away Win)"),
                        p("HTHG = Half Time Home Team Goals"),
                        p("HTAG = Half Time Away Team Goals"),
                        p("HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)"),
                        p("Referee = Match Referee"),
                        p("HS = Home Team Shots"),
                        p("AS = Away Team Shots"),
                        p("HST = Home Team Shots on Target"),
                        p("AST = Away Team Shots on Target"),
                        p("HF = Home Team Fouls Committed"),
                        p("AF = Away Team Fouls Committed"),
                        p("HC = Home Team Corners"),
                        p("AC = Away Team Corners"),
                        p("HY = Home Team Yellow Cards"),
                        p("AY = Away Team Yellow Cards"),
                        p("HR = Home Team Red Cards"),
                        p("AR = Away Team Red Cards")),
                 column(width = 6,
                        h3("Authors"),
                        p("Banaszek Stanisław"),
                        p("Drobina Mateusz"),
                        p("Mika Dominik"),
                        p("Płoszczyca Adrian"),
                        p("Sobkowiak Jakub"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h3("Data"),
                        p("The data in the application comes from the seasons from 2008/2009 to 2017/2018."))))
)


#### server ####

server <- function(input, output) {
    
    #### Analiza Sezonu ####
    
    table_for_season <- eventReactive(input$run_table_for_season_button, {
        req(input$second_page_selected_season_input)
        table <- summary_season(input$second_page_selected_season_input)[, 1:8]
        table
    })
    plot_for_stat_season <- eventReactive(input$run_plot_for_stat_button, {
        req(input$second_page_selected_stat_input)
        season_stat_viz(input$second_page_selected_season_input, input$second_page_selected_stat_input)
    })
    
    output$season_table <- renderDataTable({ datatable(table_for_season(), options = list(paging = FALSE, searching = FALSE)) })
    output$season_stat <- renderPlot({ plot_for_stat_season()}, height = 780, width = "auto" )
    
    #### pre - match ####

    expected_goals_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            showNotification(paste("You have chosen the same home team and away team.
                                   Change one of them and confirm your choice again."),
                             type = "error", duration = 10)
            return()
        }
        table <- as.data.frame(predict_match(input$first_page_select_home_team,
                                             input$first_page_select_away_team)$`Expected goals`)
        colnames(table) <- "Expected goals"
        table
    })
    results_distribution_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- as.data.frame(predict_match(input$first_page_select_home_team,
                                             input$first_page_select_away_team)$`Probability`)
        colnames(table) <- "Probability"
        table
    })
    results_distribution_plot <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team, input$first_page_select_away_team), input$first_page_last_matches)
        if(is.na(table)) {
            return()
        }
        teams_result_barplot(input$first_page_select_home_team,
                             input$first_page_select_away_team,
                             input$first_page_last_matches)
    })
    results_face_to_face_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team, input$first_page_select_away_team), input$first_page_last_matches)
        if(is.na(table)) {
            showNotification(paste("You picked the teams that didn't play in the Premier League 2008-2018 against each other."),
                             type = "error", duration = 15)
            return()
        }
        table
    })
    home_team_last_results <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team), input$first_page_last_matches)
        table
    })
    away_team_last_results <- eventReactive(input$run_prediction, {
        req(input$first_page_select_away_team)
        req(input$first_page_select_home_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_away_team), input$first_page_last_matches)
        table
    })
    home_team_results_distribution <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        result_barplot(input$first_page_select_home_team,
                             input$first_page_last_matches)
    })
    away_team_results_distribution <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        result_barplot(input$first_page_select_away_team, input$first_page_last_matches)
    })
    
    output$expected_goals <- renderTable({ expected_goals_table() }, rownames = TRUE, colnames = TRUE, width = 300)
    output$results_distribution <- renderTable({ results_distribution_table() }, rownames = TRUE, colnames = TRUE, width = 300)
    output$face_to_face_results_table <- renderDataTable({ datatable(results_face_to_face_table(), options = list(paging = FALSE, searching = FALSE)) })
    output$plot_results_distribution <- renderPlot({ results_distribution_plot()}, height = 350, width = "auto" )
    output$home_team_last_results_table <- renderDataTable({ datatable(home_team_last_results(), options = list(paging = FALSE, searching = FALSE)) })
    output$away_team_last_results_table <- renderDataTable({ datatable(away_team_last_results(), options = list(paging = FALSE, searching = FALSE)) })
    output$home_team_results_distribution_plot <- renderPlot({ home_team_results_distribution()}, height = 300, width = "auto" )
    output$away_team_results_distribution_plot <- renderPlot({ away_team_results_distribution()}, height = 300, width = "auto" )
    
    #### porownanie druzyn ####
    
    teams_form <- eventReactive(input$run_forms, {
        req(input$third_page_select_home_team)
        req(input$third_page_select_away_team)
        req(input$third_page_date_range)
        if(input$third_page_date_range[1] > input$third_page_date_range[2]) {
            showNotification(paste("The selected date range is invalid."),
                             type = "error", duration = 15)
            return()
        }
        if(input$third_page_select_home_team == input$third_page_select_away_team) {
            showNotification(paste("You picked the same team twice."),
                             type = "warning", duration = 15)
            return()
        }

        if(dim(period_matches(input$third_page_select_home_team, input$third_page_date_range[1], input$third_page_date_range[2], data))[1] != dim(period_matches(input$third_page_select_away_team, input$third_page_date_range[1], input$third_page_date_range[2]))[1]) {
            showNotification(paste("The teams you selected have played a different number of matches within the given date range."),
                             type = "warning", duration = 15)
            return()
        }
        plot_to_date_team_form(input$third_page_select_home_team, input$third_page_select_away_team, input$third_page_date_range[1], input$third_page_date_range[2])
    })
    
    output$all_time_comparison <- renderTable({ comparison <- cbind(all_time_stats(input$third_page_select_home_team), all_time_stats(input$third_page_select_away_team))
                                                colnames(comparison) <- c(input$third_page_select_home_team, input$third_page_select_away_team)
                                                comparison }, rownames = TRUE, colnames = TRUE, width = "100%")
    output$teams_form_plot <- renderPlot({ teams_form() }, width = "auto", height = 700)
    
    #### sedziowie ####
    
    stat_referee <- eventReactive(input$run_referees_comparison, {
        req(input$fourth_page_select_statistic)
        ref_results(input$fourth_page_select_statistic)
    })
    team_referee <- eventReactive(input$run_referee_and_team, {
        req(input$fourth_page_select_referee)
        req(input$fourth_page_select_team)
        if(dim(team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee))[1] == 0) {
            showNotification(paste(input$fourth_page_select_referee, "has never refereed a", 
                                   input$fourth_page_select_team, "match."),
                             type = "warning", duration = 15)
            return()
        }
        team_and_ref_results(input$fourth_page_select_team, input$fourth_page_select_referee)
    })
    team_referee_tab <- eventReactive(input$run_referee_and_team, {
        req(input$fourth_page_select_referee)
        req(input$fourth_page_select_team)
        if(dim(team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee))[1] == 0) {
            return()
        }
        table <- team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee)
        table
    })
    
    output$referee_stat <- renderPlot({ stat_referee() }, width = "auto", height = 700)
    output$referee_team <- renderPlot({ team_referee() }, width = "auto", height = 350)
    output$referee_team_tab <- renderDataTable({ datatable(team_referee_tab(), options = list(paging = TRUE, pageLength = 5, searching = FALSE)) })
}


shinyApp(ui = ui, server = server)

