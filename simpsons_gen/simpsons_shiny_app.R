#install packages
if (!require(shiny)) install.packages('shiny')
if (!require(dplyr)) install.packages('dplyr')
library(shiny)
library(dplyr)

## Load Simpsons Data
simpsons_script <- read.csv("simpsons_script.csv",  sep=",")

## add row number to simpsons_script
simpsons_script$rn <- seq.int(nrow(simpsons_script))
simpsons_script3 <- subset(simpsons_script,  speaking_line == "true" & spoken_words != "/" & word_count != "0" & word_count != "1" & word_count != "2" & word_count != "3" & word_count != "4"& word_count != "5")

##narrow character list to 500 or more quotes
character_options <- simpsons_script %>% 
    select(raw_character_text) %>%
    count(raw_character_text, sort = TRUE, name = "quote_count")   %>% 
    group_by(raw_character_text)  %>% 
    filter(quote_count > 100 , raw_character_text != "")

#quotegen function

quote_gen <- function(simpsons_char,env = parent.frame()) {
    filtered_char <<- filter(simpsons_script, raw_character_text == as.character(simpsons_char))
    quoteoftheday <<- sample_n(filtered_char, size = 1)
    quote_display <<- quoteoftheday$spoken_words
   
}

ui <- fluidPage(
    ##title
    titlePanel("Simpsons Quote Generator"),
    
    sidebarLayout(
        sidebarPanel(
            ##inputfunction
            selectInput(inputId = "simp_char", 
                        label = "Choose a Simpsons Character", 
                        choices = character_options$raw_character_text
            )
        ),
        
        
        ##outputfunction  
        mainPanel(
            textOutput(outputId = "quote_simp"),
            br(),
        
            textOutput(outputId = "episode"),
            br(),
            strong(em(textOutput(outputId = "quote")))
            
        )
        
    )
)

server <- function(input, output) {

     output$quote <- renderText({ 
        paste(quote_gen(input$simp_char))
    })
    output$quote_simp <- renderText({ 
        paste("You have selected", input$simp_char)
    })
    output$episode <-  renderText({ 
        paste("Episode", quoteoftheday$episode_id)
    })
    
}
shinyApp(ui = ui, server = server)


