
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

# This is the server file -- it is run on the server side an asychronously called whenever an input changes
# The `reactive` statement is essentially a state-aware function that runs when inputs change

library(shiny)
library(scales)

shinyServer(function(input, output) {
  getTeaRecommendations <- reactive({
    dt.teas <- recommendTea(input$likedTeas, input$dislikedTeas, dt.similarity)
    return(dt.teas)
  })
  # Show the user which teas they said they liked.
  output$likedTeasDisplay <- renderTable({
    if(!is.character(input$likedTeas) || length(input$likedTeas) == 0) return(NULL)
    dt.liked <- dt.data[link %in% input$likedTeas]
    dt.liked <- dt.liked[, list(
      name = paste0('<a target="_blank", href="',link,'">',vendor,": ",name,'</a>'), 
      image = paste0('<img height = 100, src="',image_link,'">'), 
      description)]

    setnames(dt.liked, c('name','image','description'), c('Name','Image','Description'))
    return(dt.liked)
  }, sanitize.text.function = function(x) x)
  
  # Show the disliked teas too
  output$dislikedTeasDisplay <- renderTable({
    if(!is.character(input$dislikedTeas) || length(input$dislikedTeas) == 0) return(NULL)
    dt.disliked <- dt.data[link %in% input$dislikedTeas]
    
    dt.disliked <- dt.disliked[, list(
      name = paste0('<a target="_blank", href="',link,'">',vendor,": ",name,'</a>'), 
      image = paste0('<img height = 100, src="',image_link,'">'), 
      description)]
    
    
    setnames(dt.disliked, c('name','image','description'), c('Name','Image','Description'))
    return(dt.disliked)
  }, sanitize.text.function = function(x) x)
  
  # These are the recommendations; determined using the getTeaRecommendations function. 
  output$teaRecommendations <- renderTable({
    dt.result <- getTeaRecommendations()
    if(!is.data.table(dt.result) || nrow(dt.result) == 0) return(NULL)
    dt.recommendations <- merge(dt.result[rank <= 10, list(most_similar_tea, least_similar_tea, link = recommendation, rank)], dt.data, 'link')
    dt.recommendations[, most_similar_tea:=names(chr.choices)[match(most_similar_tea, chr.choices)]]
    dt.recommendations[, least_similar_tea:=names(chr.choices)[match(least_similar_tea, chr.choices)]]
    dt.output <- dt.recommendations[, list(
      rank,
      name = paste0('<a target="_blank", href="',link,'">',vendor, ': ', name,'</a>'),
      image = paste0('<img height = 100, src="',image_link,'">'),
      description,
      most_similar_tea,
      least_similar_tea
    )][order(rank)]
    setnames(dt.output, 
             c('rank','name','image','description','most_similar_tea','least_similar_tea'),
             c('Rank','Name','Image','Description','Most Similar Tea','Least Similar Tea'))
    return(dt.output)
  }, sanitize.text.function = function(x) x)
})
