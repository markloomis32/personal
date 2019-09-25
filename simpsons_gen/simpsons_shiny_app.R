#install packages
if (!require(shiny)) install.packages('shiny')
library(shiny)

## Load Simpsons Data
simpsons_script <- read.csv("simpsons_script_clean.csv",  sep=",")

## Character Options
character_options <- unique(simpsons_script$raw_character_text) 

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
                        choices = character_options
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




