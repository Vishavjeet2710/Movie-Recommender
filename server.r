library(shiny)

shinyServer(function(input, output) {
  
  library(neuralnet)
  library(recommenderlab)
  library(class)
  library(data.table)
  library(keras)
  library(reshape2)
  movie_data<-read.csv(file.choose(),TRUE)
  movie_ratings<-read.csv(file.choose(),TRUE)
  rating_sparse<-dcast(ratings,userId~movieId,value.var="rating", na.rm=FALSE)
  rating_sparse<-as.matrix(rating_sparse[,-1])
  rating_sparse_transpose<-t(rating_sparse)
  similarity_matrix<-as.matrix(dist (rating_sparse_transpose,method = "euclidean"))
  
  k_nearest_function<-function(i,similarity_matrix,k){
    Nearest_neighbours<-order(similarity_matrix[i,])
    Recommendtions<-Nearest_neighbours[2:(k+1)]
  }
  score  <- fread('C:/Users/HP/Documents/Project/ml-20m/genome-scores.csv', sep=',')
  movies <- fread('C:/Users/HP/Documents/Project/ml-20m/movies.csv', sep=',')
  tag    <- fread('C:/Users/HP/Documents/Project/ml-20m/genome-tags.csv', sep=',')
  
  find_tags <- function(movie_number){
    setkey(score, movieId)
    target <- score[movieId == movie_number]
    top_tags <- target[order(relevance, decreasing = TRUE)[1:10], tagId]
    print(tag[tagId %in% top_tags])
    return(list(top_tags = top_tags, origin_number = movie_number))
  }
  find_recommendations <- function(tags, n){
    possi <- score[tagId %in% tags$top_tags]
    possi_rele <- possi[, .(rele_mean = mean(relevance)), by = movieId]
    possi_id <- possi_rele[order(rele_mean, decreasing = TRUE)[1:(n+1)], movieId]
    possi_id_minus_origin <- setdiff(possi_id, tags$origin_number)[1:n]
    return(movies[movieId %in% possi_id_minus_origin, ])
  }
  
  generate_item <- function(id = 1, rec.table){
    paste0(
      "
      <div class=\"grid-item\">
      <div class=\"grid-info\">
      <h3>",
      rec.table$title[id],
      "</h3>
      </div>
      </div>"
    )
  }
  #####################################################################################
  output$input <- renderText({
    return(   
      paste0(
        "
        <div class=\"info-container\">
        <div class=\"info-item\">
        <h1>",
        movies[movieId == input$id, title],
        "</h1>
        <h3>Genres: ",
        movies[movieId == input$id, genres],
        "</h3>
        </div>
        </div>"
      )
    )
    
  })
  
  
  output$test <- renderText({
    if(input$type=="K_nearest_item_based"){
      Recommendtions <- k_nearest_function(input$id,similarity_matrix, input$number)
      rec.table<-movie_data[Recommendtions,]
    }
    else{
      tags <- find_tags(input$id)
      rec.table <- find_recommendations(tags, input$number)
    }
    
    return(
      paste0(
        "<div class=\"grid-container\">",
        paste(sapply(1:input$number, function(x) generate_item(x, rec.table)), collapse = ""),
        "</div>"
      )
    )
  })
  
  #########################################Searching Purpose#########################################
  output$candidates <- renderTable({
    if(input$search != ""){
      return(
        movies[grep(input$search, movies[, title], ignore.case = TRUE)]
      )
    } else {
      return()
    }
  })
  
  
})