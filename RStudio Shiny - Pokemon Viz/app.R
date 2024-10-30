# Loading in Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ds4psy)
library(showtext)

font_add('Pokemon', 'www/pokemon-ds-font.ttf')
showtext_auto()

# DATA
poke <- read.csv('Data/pokemon.csv')
poke <- poke %>% 
  rename('Sp. Attack' = Sp..Atk,
         'Sp. Defense' = Sp..Def)

# Lists containing information on whether a pokemon is a pseudo_legendary, sub_legendary, 
# legendary, or mythical
# Pseudo-legendary is small enough that I can just type it out
pseudo <- read.csv('Data/pseudolist.csv')[['X0']]
sub_leg <- read.csv('Data/sub_leglist.csv')[['X0']]
leg <- read.csv('Data/leglist.csv')[['X0']]
mythical <- read.csv('Data/mythicallist.csv')[['X0']]

# Data used in this project enclosed within two links
url1 <- a('pokemondb.net,', href = 'https://pokemondb.net/pokedex/all')
url2 <- a('wikipedia.org,', href = 'https://en.wikipedia.org/wiki/List_of_Pok%C3%A9mon')
url3 <- a('serebii.net,', href = 'https://serebii.net/')
url4 <- a('bulpedia.bulbagarden.net', href = 'https://bulbapedia.bulbagarden.net/wiki/Main_Page')

# List of all unique typings in dataset
typings <- append('All Types', unique(text_to_words(poke$Type)))
# List of all stats in dataset
stats <- colnames(poke)[3:9]

# Function to filter based on types
to_be_types <- function(df, group) {
  if (group == 'All Types') 
    df
  else
    filter(df, grepl(group, Type))
}

# Function to filter out based on pokemon to exclude
to_be_pseudo <- function(df, group) {
  if ('Pseudo Legendaries' %in% group) {
    filter(df, !Name %in% pseudo)
  } else df
}
to_be_sub <- function(df, group) {
  if ('Sub Legendaries' %in% group) {
    filter(df, !Name %in% sub_leg)
  } else df
}
to_be_leggy <- function(df, group) {
  if ('Legendaries' %in% group) {
    pattern = paste(leg, collapse = '|')
    filter(df, !grepl(pattern, Name))
  } else df
}
to_be_mythical <- function(df, group) {
  if ('Mythicals' %in% group) {
    pattern = "Mew\\b|Celebi|Jirachi|Deoxys|Phione|Manaphy|Darkrai|Shaymin|Arceus|Victini|Keldeo|Meloetta|Genesect|Diancie|Hoopa|Volcanion|Magearna|Marshadow|Zeraora|Meltan|Melmetal|Zarude|Pecharunt"
    filter(df, !grepl(pattern, Name))
  } else df
}
to_be_mega <- function(df, group) {
  if ('Megas' %in% group) {
    filter(df, !grepl(' Mega ', Name, fixed = TRUE))
  } else df
}

# Color vector for each type
type_colors <- c(
  "All Types" = '#3b3b3b',
  "Normal" = "#A8A77A",
  "Fire" = "#EE8130",
  "Water" = "#6390F0",
  "Electric" = "#F7D02C",
  "Grass" = "#7AC74C",
  "Ice" = "#96D9D6",
  "Fighting" = "#C22E28",
  "Poison" = "#A33EA1",
  "Ground" = "#E2BF65",
  "Flying" = "#A98FF3",
  "Psychic" = "#F95587",
  "Bug" = "#A6B91A",
  "Rock" = "#B6A136",
  "Ghost" = "#735797",
  "Dragon" = "#6F35FC",
  "Dark" = "#705746",
  "Steel" = "#B7B7CE",
  "Fairy" = "#D685AD"
)

# Bounds definer vector for each stat total
bounds <- list(
  'Total' = list(seq(0, 700, by = 50), c(0, 700)),
  'HP' = list(seq(0, 150, by = 20), c(0, 150)),
  'Attack' = list(seq(0, 150, by = 20), c(0, 150)),
  'Defense' = list(seq(0, 150, by = 20), c(0, 150)),
  'Sp. Attack' = list(seq(0, 150, by = 20), c(0, 150)),
  'Sp. Defense' = list(seq(0, 150, by = 20), c(0, 150)),
  'Speed' = list(seq(0, 150, by = 20), c(0, 150))
)


#-------------------------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Pokemon Stats Over the Generations"),
    tags$style(HTML("
      /* Changing font to type one I have stored */
      @font-face {
        font-family: 'Pokemon';
        src: url('pokemon-ds-font.ttf') format('truetype');
      }
      
      /* Changing font of regular text */
      body {
        font-family: 'Pokemon';
        font-size: 14px;
        background-image: url('background.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
        background-position: 70px center;
      }
      
      #all_typings, #stat, #leggy label {
        color: black;
        text-shadow: 1px 1px 0px white, 
                     -1px -1px 0px white, 
                     1px -1px 0px white, 
                     -1px 1px 0px white;
        font-size: 14px;
      }
      
      /* Making inputs have less space in between them */
      .shiny-input-container > label {margin-bottom: -30px;}
      }
    "))
  ),
  
  textOutput("selectedValue"),
  
  titlePanel(
    tags$div('Pokemon Stats Over the Generations', style = 'font-size: 50px')
    ),
  
  # Adding output to display the links in the UI
  uiOutput("tab", style = 'margin-top: -20px'),
  
  fluidRow(
    column(width = 8, 
           mainPanel(
             plotOutput('lineplot', width = '710px')
           )
    ),
    column(width = 3,
           style = 'margin-top: -50px;',
           selectizeInput('all_typings', label = 'Select a typing to explore',
                          choices = typings,
                          selected = 'All Types'),
           radioButtons('stat', label = 'Select a statistic mean to look at',
                        choices = colnames(poke)[3:9],
                        selected = 'Total'),
           checkboxGroupInput('leggy', label = 'Select a group of pokemon to exclude',
                              choices = c('None', 'Pseudo Legendaries', 
                                          'Sub Legendaries', 'Legendaries',
                                          'Mythicals', 'Megas'),
                              selected = 'None')
    
    )
  )
)

server <- function(input, output, session) {
  # Updating selection to make sure that while None is selected, nothing else can be
  # selected for the "leggy" input
  observeEvent(input$leggy, {
    if ("None" %in% input$leggy) {
      updateCheckboxGroupInput(session, "leggy", selected = "None", 
                               choices = c("None", "Pseudo Legendaries", 
                                           "Sub Legendaries", "Legendaries", 
                                           "Mythicals", 'Megas'))
    } else {
      # Exclude "None" if any other option is selected
      updateCheckboxGroupInput(session, "leggy", selected = input$leggy, 
                               choices = c("None", "Pseudo Legendaries", 
                                           "Sub Legendaries", "Legendaries", 
                                           "Mythicals", 'Megas'))
    }
  })
  
  # Function to change data based on type 
  all_poke <- reactive({poke %>% 
      to_be_types(group = input$all_typings) %>% 
      to_be_leggy(group = input$leggy) %>% 
      to_be_mega(group = input$leggy) %>% 
      to_be_mythical(group = input$leggy) %>% 
      to_be_pseudo(group = input$leggy) %>% 
      to_be_sub(group = input$leggy)
      })
  
  
  # Main Panel Tab
  output$tab <- renderUI({
    tagList('The data used in this application is thanks to', url1, url2, url3, 'and', url4)
  })
  
  # Rendering the plot
  output$lineplot <- renderPlot({
    data <- all_poke() %>% 
      group_by(Generation) %>% 
      summarise(Total = mean(Total),
                HP = mean(HP),
                Attack = mean(Attack),
                Defense = mean(Defense),
                'Sp. Attack' = mean(`Sp. Attack`),
                'Sp. Defense' = mean(`Sp. Defense`),
                Speed = mean(Speed)
                )
    
    ggplot(data, aes(Generation, .data[[input$stat]])) +
      geom_line(linewidth = 1.5, aes(color = input$all_typings)) + 
      geom_point(size = 2, stroke = 1) +
      labs(y = input$stat, color = 'Type',
           title = paste("Average of the", input$stat, 'Stat(s)')) + 
      scale_color_manual(values = type_colors) + 
      scale_x_continuous(breaks = 1:9, labels = c('I', 'II', 'III', 'IV', 'V',
                                                  'VI', 'VII', 'VIII', 'IX')) +
      scale_y_continuous(breaks = bounds[input$stat][[1]][[1]],
                         limits = bounds[input$stat][[1]][[2]]) +
      theme_minimal() +
      theme(plot.title = element_text(size = 30, face = 'bold', family = 'Pokemon'),
            axis.title.x = element_text(size = 25, family = 'Pokemon'),
            axis.title.y = element_text(size = 25, family = 'Pokemon'),
            axis.text.y = element_text(size = 15, family = 'Pokemon'),
            axis.text.x = element_text(size = 15, family = 'Pokemon'),
            legend.text = element_text(size = 15, family = 'Pokemon'),
            legend.title = element_text(size = 18, family = 'Pokemon'),
            legend.position = c(0.95, 0.075)) 
    
    
  })
  
  
}

# Running shiny app
shinyApp(ui = ui, server = server)

