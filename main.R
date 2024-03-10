library(tidyverse)
library(ggplot2)
library(viridis)
library(stringr)


x_survey <- read_csv("Spring Survey.csv")
headings <- names(x_survey)
names(x_survey) <- make.names(names(x_survey), unique=TRUE)
all_teams <- c("all",unique(x_survey$I.consider.myself.....))

Ext <- c("Strongly Disagree", "Disagree", "Don't know", "Agree", "Strongly Agree")

finish <- 15
start <- 8
sections <- names(x_survey)[start:finish]
#teams_selection <- names(x_survey)[22:24]

liekart_calc <- function(x_survey,team_select, team="all",section) {
  #print(nrow(x_survey))
  #subtit <- section[1]
  if(team != "all") {
    #print("***team")
    x_survey <- filter(x_survey, get({{team_select}}) == team)
  }
  begins <- as.integer(section[9])
  ends <- as.integer(section[10])
  colm <- tail(section,5)
  col_start <- 1
  col_end <- ends-begins+1
  #print(col_start)
  #print(col_end)
  size <- 30/(ends - begins)
  width <- 45
  
  dept_liekart <- x_survey %>% 
    select(begins:ends) %>%
    pivot_longer(cols = col_start:col_end,names_to = "question", values_to = "response") %>%
    #mutate(response = ifelse(response == "Not applicable", "Don't know", response))%>%
    mutate(response = factor(response,
                             ordered = TRUE,
                             levels = colm))
  
  dept_liekart <- filter(dept_liekart, !is.na(response))
  #print(dept_liekart)
  
  #return(dept_liekart) }
  missing_values <- c(0,0,0,0,0)
  names(missing_values) <- colm
  a <- as.data.frame(t(as.data.frame(missing_values)))
  
  dept_liekart_2 <- dept_liekart %>% group_by(question, response) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = response,
                values_from = count,
                values_fill = list(`Not applicable` = 0)) %>%
      left_join(a) %>%
      replace(is.na(.),0) %>%
    #  select(-`Not applicable`) %>%
    #  mutate(total = Extensively + Moderately + `A little` + `Not at all` + `Don't know`)
    select(c("question",colm)) %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm=TRUE))
  
  #print(dept_liekart_2)
  
  hd <- names(dept_liekart_2)
  
  liekart_end <- dept_liekart_2 %>%
    mutate(!!hd[6] := .[[6]]/total,
           !!hd[5] := .[[5]]/total,
           !!hd[4] := .[[4]]/total,
           !!hd[3] := .[[3]]/total,
           !!hd[2] := .[[2]]/total)
  
  liekart_start <- liekart_end %>%
    mutate(!!hd[2] := 0 - 
             liekart_end[[2]] - 
             liekart_end[[3]] - 
             0.5 * liekart_end[[4]]) %>%
    mutate(!!hd[3] := 0 - liekart_end[[3]] -
             0.5 * liekart_end[[4]]
    ) %>%
    mutate(!!hd[4]  := 0 - 0.5 * liekart_end[[4]]) %>%
    mutate(!!hd[5]  := 0 + 0.5 * liekart_end[[4]]) %>%
    mutate(!!hd[6]  := 0 + 0.5 * liekart_end[[4]] + liekart_end[[5]])
  
  #print(liekart)
  
  liekart_end_percent <- liekart_end %>%
    mutate(!!hd[6]  := .[[6]] * 100,
           !!hd[5]  := .[[5]] * 100,
           !!hd[4]  := .[[4]] * 100,
           !!hd[3]  := .[[3]] * 100,
           !!hd[2]  := .[[2]] * 100
    )
  
  liekart_start_percent <- liekart_start %>%
    mutate(!!hd[6]  := .[[6]] * 100,
           !!hd[5]  := .[[5]] * 100,
           !!hd[4]  := .[[4]] * 100,
           !!hd[3]  := .[[3]] * 100,
           !!hd[2]  := .[[2]] * 100
    )
  
  
  
  liekart_end_percent_pivot <- liekart_end_percent %>%
    pivot_longer(cols = colm,
                 names_to = "response") %>%
    mutate(end = value) %>%
    select(-value)
  
  liekart_start_percent_pivot <- liekart_start_percent %>%
    pivot_longer(cols = colm,
                 names_to = "response") %>%
    mutate(start = value) %>%
    select(-value)
  
  liekart_data_percent <- liekart_start_percent_pivot %>%
    left_join(liekart_end_percent_pivot, by = c("question", "response")) %>%
    mutate(response = factor(response, order = TRUE, levels = 
                               colm)) %>%
    mutate(question = str_wrap(gsub("\\."," ", question),width))
  
  pal <- c("#DF4949", "#E27A3F", "#BEBEBE", "#45B29D", "#334D5C")
  
  #print(liekart_data_percent %>% filter(response == "Not applicable"))
  #print(unique(liekart_data_percent$response))
  plot1<- ggplot(liekart_data_percent) + 
    geom_segment(aes(x = question, y = start, xend = question, yend = start + end, 
                     colour = response),linewidth = size) +
    geom_hline(yintercept = 0) +
    coord_flip()+
    theme_bw()+
    scale_colour_manual("response", values = pal, guide = "legend")+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          #legend.position = "bottom",
          legend.position = c(0.1, -.05),
          legend.direction = "horizontal",
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank()) +
    labs(title = team,
         #subtitle = str_wrap(subtit,60),
         #caption = str_wrap(team,15),
         x = "",
         y="")
  
  return(plot1)
  #return(plot1)
  #return(dept_liekart)
}

liekart <- function(x_survey,team_select, teams,section) {
  for(team in teams) {
    print(team)
    plot1 <- liekart_calc(x_survey,team_select, team, section)
  return(plot1)
  }
}

