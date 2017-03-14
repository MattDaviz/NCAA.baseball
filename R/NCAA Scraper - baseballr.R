require(baseballr)
require(dplyr)
require(rvest)
require(xml2)

# Real code-----------

# find individual player college wOBA

school_id_lu("Texas A&M ")
test <- ncaa_scrape(812, 2016, "batting")

year <- c(2015)
school_id <- 116
division <- 1
v_table <- expand.grid(school_id, year)
test <- v_table %>%
  group_by(Var1, Var2) %>%
  do(ncaa_scrape(.$Var1, .$Var2, "batting"))
stekl <- rbind(y, x)
test <- test[,c(3:35)]

zack <- test[which(test$Player == "Sutton, Sean"),]
zack <- zack[,c(22,23,16,17,18,20,15,24,26,1)]
zack <- lapply(zack, as.numeric)
zack <- as.data.frame(do.call(cbind, zack))
colnames(zack) <- c("uBB","HBP","X1B","X2B","X3B","HR","AB",'SF','SO','season')
zack$season <- as.character(zack$season)
zack[is.na(zack)] <- 0
y <- woba_plus(zack)
y$season <- 'Fr'
x <- woba_plus(zack)
x$season <- 'So'
x$BABIP <- round((x$X1B + x$X2B + x$X3B -x$HR)/(x$AB-x$SO-x$HR+x$SF),3)
y$BABIP <- round((y$X1B + y$X2B + y$X3B -y$HR)/(y$AB-y$SO-y$HR+y$SF),3)
#x$wOBA_scale <- 1.212
#x$wOBA_conf <- 1.00
#x$wRAA <- ((x$wOBA - x$wOBA_conf) / x$wOBA_scale) * x$AB

# calculate rafters season wOBA

setwd("/users/mattd/Desktop")
a <- read.csv("a.csv")
str(a)
a <- a[,c(1,12,13,7,8,9,10,5,15,14,21)]
a$season <- 2016
colnames(a) <- c("Player","uBB",'HBP','X1B','X2B','X3B','HR','AB','SF','SO','col_wOBA','season')
x <- woba_plus(a)
x <- x[which(x$AB > 90),]
cor(x$col_wOBA, x$wOBA, use = "complete.obs")

x$BABIP <- round((x$X1B + x$X2B + x$X3B -x$HR)/(x$AB-x$SO-x$HR+x$SF),3)
cor(x$wOBA, x$BABIP, use = "complete.obs")

# All NCAA Stats----------

conference_table <- function(year, div) {
  url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=", year, "&conf_id=-1&division=", div, "&sport_code=MBA")
  read <- read_html(url)
  links <- html_nodes(read, "a") %>%
    html_attr("href")
  link_names <- html_nodes(read, "a") %>%
    html_text()
  table <- as.data.frame(cbind(link_names, links))
  names(table) <- c("conference", "links")
  table$conference <- as.character(table$conference)
  links_conferences <- table %>%
    filter(grepl("changeConference", links))
  conference_ids <- sub("\\).*", "", sub(".*\\(", "", links_conferences$links))
  conference_ids <- as.data.frame(conference_ids)
  names(conference_ids) <- "conference_id"
  table <- cbind(links_conferences, conference_ids)
  table <- table %>%
    mutate(year = year, division = div, conference_id = as.numeric(as.character(conference_id))) %>%
    select(year, division, everything(), -links) %>% .[-1,]
  table
}

year <- c(2016)
division <- c(1,2,3)
div_yr <- expand.grid(year, division)

conference_code_lu <- div_yr %>% 
  group_by(Var1, Var2) %>%
  do(conference_table(.$Var1, .$Var2))

teams <- function(year, conference, div) {
  url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=", year, "&conf_id=", conference, "&division=", div, "&sport_code=MBA")
  read <- read_html(url)
  links <- html_nodes(read, "a") %>%
    html_attr("href")
  link_names <- html_nodes(read, "a") %>%
    html_text()
  table <- as.data.frame(cbind(link_names, links))
  table$links <- as.character(table$links)
  table$link_names <- as.character(table$link_names)
  table <- table %>%
    filter(grepl("team", links)) %>%
    filter(!grepl("inst_team", links)) %>%
    filter(!grepl("schedule", links))
  table$links <- gsub("/team/", "", table$links)
  table$links <- sub("/.*", "", table$links)
  table$year <- year
  table$division <- div
  table$conference_id <- conference
  names(table) <- c("school", "school_id", "year", "division", "conference_id")
  table
}

master_ncaa_team_lu <- conference_code_lu %>% 
  group_by(year, division, conference, conference_id) %>% 
  do(teams(.$year, .$conference_id, .$division)) %>%
  ungroup() %>%
  select(school, conference, everything()) %>%
  mutate(school_id = as.numeric(school_id)) %>%
  arrange(school)

numbers <- master_ncaa_team_lu %>%
  filter(conference_id > 0) %>%
  distinct(school_id)

numbers <- unlist(numbers)
numbers <- numbers[!numbers == 30083]
numbers <- numbers[!numbers == 481]
numbers <- numbers[1:309]
numbers <- numbers[724:910]
numbers

# create the list to hold the tables
tables <- vector("list", length(numbers))

for(i in numbers) {
  # create the url for the day and if it exists, read it - if not, NULL
  
  tables[[i]] <- if(!is.null(ncaa_scrape(i, 2016, "batting"))) ncaa_scrape(i, 2016, "batting")
                    else NULL
}

#put the data together
data <- do.call(rbind,tables) 

colnames(data) <- c('season', 'school', 'conference', 'division', 'jersey', 'player', 'yr', 'pos', 'GP', 
                    'GS', 'BA', 'OBPct', 'SlgPct', 'R', 'AB', 'X1B', 'X2B', 'X3B', 'TB', 'HR',
                    'RBI', 'uBB', 'HBP', 'SF', 'SH', 'SO', 'DP', 'CS', 'Picked', 'SB', 'RBI2out', 'teamid',
                    'conference_id')
data$AB <- as.numeric(data$AB)
data[is.na(data)] <- 0

woba <- woba_plus(data)
woba <- woba[!grepl("Totals", woba$player),]
save4 <- woba
#ncaa_scrape(797, 2013, "batting")
#rm(save4)

alldata <- rbind(save, save2, save3, save4)

setwd("/users/mattd/Desktop")
write.csv(alldata, "2013_college_hitting.csv")

available <- alldata %>%
  filter(AB > 100, yr != "Sr") %>%
  arrange(desc(wOBA))

woba %>%
  filter(AB >100, yr != "Sr") %>%
  summarise(mean(wOBA))

conf_woba<- available %>%
  group_by(conference) %>%
  summarise(mean(wOBA)) %>%
  arrange(desc(`mean(wOBA)`))

available <- left_join(available, conf_woba, by="conference")
available$wOBA_scale <- 1.212
colnames(available)[36] <- "wOBA_conf"
available$wRAA <- ((available$wOBA - available$wOBA_conf) / available$wOBA_scale) * available$AB

available <- available %>%
  arrange(desc(wRAA))

rpa <- alldata %>%
  group_by(conference) %>%
  summarise(sum(R)/sum(AB))

available <- left_join(available, rpa, by = "conference")
colnames(available)[39] <- "r_pa_conf"
available$wRC <- (((available$wOBA - available$wOBA_conf) / available$wOBA_scale) + (available$r_pa_conf)) * available$AB

diamonds <- available %>%
  filter(wRAA > 10, BA < .3, yr != "Jr", division == 1) %>%
  arrange(desc(wRAA))

a <- read.csv("a.csv")

# Plotting----------
library(ggplot2)
library(ggthemes)
library(ggrepel)

library(extrafont)

loadfonts(device="win")

ggplot(available, aes(x = wRAA, y = BA)) +
  geom_point(alpha = 0.25) +
 # geom_point(data = rafters, aes(x = wRAA, y = BA), color = "gold", fill = "darkred", size = 3, pch = 21) +
  geom_label_repel(data = rafters, aes(x = wRAA, y = BA, label = player), color = "gold", fill = "darkred", size = 3, segment.size = 1.5, segment.color = "darkred", family = "Franklin Gothic Book") +
  #geom_label(label = available$player, size = 2) +
  theme_fivethirtyeight() +
  #labs(title = "Wisconsin Rapids Rafters 2016 roster") +
  theme(axis.title = element_text(family = "Franklin Gothic Book"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas", face = "bold"))

# Plot all players
plotdata <- alldata %>%
  filter(AB > 100)

conf_woba<- plotdata %>%
  group_by(conference) %>%
  summarise(mean(wOBA)) %>%
  arrange(desc(`mean(wOBA)`))

plotdata <- left_join(plotdata, conf_woba, by="conference")
plotdata$wOBA_scale <- 1.212
colnames(plotdata)[36] <- "wOBA_conf"
plotdata$wRAA <- ((plotdata$wOBA - plotdata$wOBA_conf) / plotdata$wOBA_scale) * plotdata$AB

plotdata <- plotdata %>%
  arrange(desc(wRAA))

rpa <- alldata %>%
  group_by(conference) %>%
  summarise(sum(R)/sum(AB))

plotdata <- left_join(plotdata, rpa, by = "conference")
colnames(plotdata)[39] <- "r_pa_conf"
plotdata$wRC <- (((plotdata$wOBA - plotdata$wOBA_conf) / plotdata$wOBA_scale) + (plotdata$r_pa_conf)) * plotdata$AB

ggplot(plotdata, aes(x = wOBA, y = BA)) +
  geom_point(alpha = 0.25) +
  #geom_point(data = rafters, aes(x = wRAA, y = BA), color = "gold", fill = "darkred", size = 3, pch = 21) +
  #geom_label_repel(data = rafters, aes(x = wRAA, y = BA, label = player), color = "gold", fill = "darkred", size = 3, segment.size = 1.5, segment.color = "darkred", family = "Franklin Gothic Book") +
  #geom_label(label = available$player, size = 2) +
  theme_fivethirtyeight() +
  geom_smooth(method = "loess", se = FALSE) +
  #labs(title = "Wisconsin Rapids Rafters 2016 roster") +
  theme(axis.title = element_text(family = "Franklin Gothic Book"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas", face = "bold"))

plotdata %>%
  filter(BA < .3, wOBA > .575)

# Read Data -------------
setwd("/users/mattd/Desktop")

twenty_thirteen <- read.csv("2013_college_hitting.csv")
twenty_fourteen <- read.csv("2014_college_hitting.csv")
twenty_fifteen <- read.csv("2015_college_hitting.csv")
twenty_sixteen <- read.csv("2016_college_hitting.csv")

alldata <- rbind(twenty_thirteen, 
                 twenty_fourteen, 
                 twenty_fifteen, 
                 twenty_sixteen)

pf <- read.csv("pf.csv")

alldata <- left_join(alldata, pf, by = c("school"= "NCAA.Team"))
alldata <- alldata[,2:37]
alldata$Park.Factor[is.na(alldata$Park.Factor)] <- 100

alldata <- alldata %>%
  filter(AB > 50)

#alldata <- alldata %>%
#  filter(AB > 100, yr != "Sr") %>%
#  arrange(desc(wOBA))

conf_woba<- alldata %>%
  group_by(conference, season) %>%
  summarise(mean(wOBA)) %>%
  arrange(desc(`mean(wOBA)`))

alldata <- left_join(alldata, conf_woba, by=c("conference","season"))
alldata$wOBA_scale <- 1.212
colnames(alldata)[37] <- "wOBA_conf"

rpa <- alldata %>%
  group_by(conference, season) %>%
  summarise(sum(R)/sum(AB))
colnames(rpa) <- c("conference", "season", "r_pa_conf")
alldata <- left_join(alldata, rpa, by = c("conference","season"))

alldata$wRC <- (((alldata$wOBA - alldata$wOBA_conf) / alldata$wOBA_scale) + (alldata$r_pa_conf)) * alldata$AB

wrc <- alldata %>%
  group_by(conference, season) %>%
  summarise(sum(wRC)/sum(AB))
colnames(wrc) <- c("conference", "season", "wrc_pa_conf")
alldata <- left_join(alldata, wrc, by = c("conference","season"))

# wRAA 
alldata$wRAA <- round(((alldata$wOBA - alldata$wOBA_conf) / alldata$wOBA_scale) * alldata$AB,1)

# BABIP (H - HR) / (AB - K - HR + SF)
alldata$BABIP <- round(((alldata$X1B + alldata$X2B + alldata$X3B)-alldata$HR) / (alldata$AB - alldata$SO - alldata$HR + alldata$SF),3)

# wRC+ (((wRAA / PA + League R/PA) + (League R/PA - Park Factor * League R/PA)) / (League wRC/PA)) * 100
alldata$wRCplus <- round((((alldata$wRAA / alldata$AB) + (alldata$r_pa_conf - (alldata$Park.Factor/100) * alldata$r_pa_conf))/
                      (alldata$wrc_pa_conf)) * 100,1)

#data <- alldata %>%
#  filter(player %in% c("Bryant, Kris", "Koth, Taylor", "Schwarber, Kyle"), season == 2013)
#
#data <- data[,c(1:9,34,42:44)]

prospects <- alldata %>%
  filter(division == 1, season = 2016) %>%
  arrange(BABIP)

babip <- alldata %>%
  filter(division == 1, wRAA > 0, pos != "P", yr == "Fr", BABIP < .42) %>%
  arrange(desc(wRAA))

diamonds <- prospects %>%
  filter(yr == "Fr", wRAA > 0) %>%
  arrange(BABIP)

diamonds <- prospects %>%
  filter(BABIP < .425, wRCplus > 100)

ggplot(diamonds, aes(x = wRAA, y = BABIP)) +
  geom_point(alpha = 0.25) +
  #geom_point(data = rafters, aes(x = wRAA, y = BA), color = "gold", fill = "darkred", size = 3, pch = 21) +
  geom_label_repel(data = diamonds[which(diamonds$wRAA > 6 & diamonds$BABIP < .41),], aes(x = wRAA, y = BABIP, label = player), color = "gold", fill = "darkred", size = 3, segment.size = 1.5, segment.color = "darkred", family = "Franklin Gothic Book") +
  #geom_label(label = available$player, size = 2) +
  theme_fivethirtyeight() +
  #geom_smooth(method = "lm", se = FALSE) +
  #labs(title = "All college baseball runs are not created equal",
  #     subtitle = "Division 1 college baseball prospects wRC compared to BA") +
  theme(axis.title = element_text(family = "Franklin Gothic Book"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium"),
        plot.subtitle = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas", face = "bold"))

prospects %>%
  summarise(mean(wRCplus))

prospects <-  alldata %>%
  filter(yr != "Sr", BABIP < .425, wRAA > 0, season == 2016, pos != "P") %>%
  arrange(BABIP)

cor(prospects$wRAA, prospects$BABIP)

alldata %>%
  filter(player == "Ota, Scott")

rafters2016 <- alldata %>%
  filter(player %in% c("Lumley, Jake", "Fisher, Ben", "Woodcock, Dustin", "Mattix, Logan", "Sims, Demetrius",
                       "Polizzi, Brandon", "Morgan, Joey", "Kapers, Scott", "McGowan, Jacson", "Palacios, Richie",
                       "Koutsoyanopulos, Aldo")) %>%
  arrange(player, season)

# Rafters 2016 ------------

rafters <- read.csv("a.csv")

rafters <- rafters %>%
  filter(player != '')

rafters <- left_join(rafters, prospects, by = "player")

rafters <- rafters %>%
  arrange(player, season)

rafters %>%
  summarise(mean(wRC))

colnames(rafters) <- c('Player', 'player', 'P', 'AVG', 'G', 'AB', 'R', 'X1B', 'X2B', 'X3B', 'HR',
                       'RBI', 'uBB', 'HBP', 'SO', 'SF', 'SH', 'SB', 'CS', 'DP', 'E', 'col_wOBA')

rafters <- rafters %>%
  filter(AB > 50)
rafters$season <- 2016

#data$AB <- as.numeric(data$AB)
#data[is.na(data)] <- 0

woba <- woba_plus(rafters)
woba <- woba[!grepl("Totals", woba$player),]

woba$BABIP <- round(((woba$X1B + woba$X2B + woba$X3B)-woba$HR) / (woba$AB - woba$SO - woba$HR + woba$SF),3)

bucks <- read.csv("b.csv")

northwoods <- read.csv("nw.csv")
str(northwoods)
northwoods$season <- 2016
colnames(northwoods) <- c('Player', 'team', 'P', 'AVG', 'G', 'AB', 'R', 'X1B', 'X2B', 'X3B', 'HR',
                       'RBI', 'uBB', 'HBP', 'SO', 'SF', 'SH', 'SB', 'CS', 'DP', 'E', 'season')

northwoods <- woba_plus(northwoods)
northwoods$BABIP <- round(((northwoods$X1B + northwoods$X2B + northwoods$X3B)-northwoods$HR) / (northwoods$AB - northwoods$SO - northwoods$HR + northwoods$SF),3)

northwoods <- northwoods[,c(1:23,25)]
northwoods <- northwoods %>%
  arrange(BABIP)

northwoods %>%
  group_by(team) %>%
  summarise(mean(wOBA)) %>%
  arrange(desc(`mean(wOBA)`))

prospects <- alldata %>%
  filter(season == 2016, yr != "Sr")