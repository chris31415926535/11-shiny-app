#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidytext)
library(stringr)
library(dplyr)

load("model_specs.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Predict Yelp Ratings from Review Text"),
    
    
   
    fluidRow(
        column(3, 
               shiny::h4("Controls & Results"),
               shiny::br(),
               actionButton("button", "Predict Rating"),
               shiny::br(),
               shiny::br(),
               textOutput("prediction"),
               textOutput("probability")),
        column(9, 
               shiny::h4("Input Review Text"),
               textAreaInput("reviewtext", "", "This is a sample restaurant review. I hated this place.", cols = 160, rows = 10)
        )
    )
    
)

# Define server logic to rate restaurant reviews
server <- function(input, output) {
    prob <- eventReactive(input$button, {
        prob_text(input$reviewtext, model_coefs, qtiles) %>%
            base::round(digits = 3)
        
    })

    pred <- reactive({
        prob() %>%
            pred_text()
    })
    output$prediction <- renderText({ paste0("Prediction: ",pred())})
    
    output$probability <- renderText({ paste0("Probability: ", prob())})

}

# function to get probability of classification
get_prob <- function (input_data, coefs){
    # first get log odds
    log_odds <- coefs["(Intercept)"] + coefs["afinn_mean"] * input_data$afinn_mean + coefs["buts_nots"] * input_data$buts_nots %>%
        unname()
    
    # then get prob
    prob <- 1 / (1 + exp(-log_odds)) %>%
        unname()
    
    return (prob)
}

# function to prepare a text vector and return a prepared tibble with afinn_mean and buts_nots
prepare <- function(text) {
    input_data <- tibble(text = text) 
    
    input_data <- input_data %>%
        tidytext::unnest_tokens(output = word, input = text) %>%
        left_join(afinn, by="word") %>%
        summarise(afinn_mean = mean(value, na.rm = T)) %>%
        mutate(afinn_mean = if_else(is.na(afinn_mean) | is.nan(afinn_mean), 0, afinn_mean)) %>%
        bind_cols(input_data) %>%
        mutate(buts = stringr::str_count(text, "but "),
               nots = stringr::str_count(text, "not "),
               buts_nots = buts + nots)
    
    return(input_data)
}

# function to get quintile
get_qtile <- function(text, qtiles = qtiles){
    # count words: count the number of spaces and add 1
    words <- stringr::str_count(text, " ") + 1
    
    qtile <- case_when(
        words %in% qtiles[1]:qtiles[2] ~ 1,
        words %in% qtiles[2]:qtiles[3] ~ 2,
        words %in% qtiles[3]:qtiles[4] ~ 3,
        words %in% qtiles[4]:qtiles[5] ~ 4,
        words > qtiles[5] ~ 5
    )  
    
    return(qtile)
}

prob_text <- function(text, model_coefs, qtiles){
    # get quintile for text based on word length  
    qtile <- get_qtile(text, qtiles = qtiles)
    
    # prepare the text by getting afinn sentiment and counting buts/nots
    prepped_text <- prepare(text)
    
    # get the probability this text is positive
    prob <- get_prob(prepped_text, model_coefs[[qtile]])
    
    # return the probability
    return(prob)
}

pred_text <- function(prob, threshold = 0.5){
    if (prob >= threshold) result <- "POS"
    if (prob <= threshold) result <- "NEG"
    return (result)
}


# Run the application 
shinyApp(ui = ui, server = server)
