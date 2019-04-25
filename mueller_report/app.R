library(shiny)
library(tidyverse)
library(wordcloud2)
library(tm)
library(SnowballC)

create_wordcloud<- 
    
    function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
        corpus <- Corpus(VectorSource(data))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        data <- sort(rowSums(tdm), decreasing = TRUE)
        data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
        num_words <- 3
    }  
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
        return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
}


mueller_report_txt <- read_rds("data.rds")

# Define UI for the application
ui <- fluidPage(
    h1("Creating a Word Cloud of the Mueller Report"),
    # Add the word cloud output placeholder to the UI
    wordcloud2Output(outputId = "cloud")
)

# Define the server logic
server <- function(input, output) {
    # Render the word cloud and assign it to the output list
    output$cloud <- renderWordcloud2({
        # Create a word cloud object
        create_wordcloud(mueller_report_txt, num_words = 100, background = "white")
    })
}

# Run the application
shinyApp(ui = ui, server = server)