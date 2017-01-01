library(shiny)
# library(ggplot2)
# library(dplyr)

#setwd("/home/boole2/coursera/CapstonePrj/week5/KneserNayPredictor/")
kn.bi.gramma <- readRDS("data/bi_kneiser_nay0004.Rds")
kn.tri.gramma <- readRDS("data/tri_kneiser_nay0004.Rds")
kn.four.gramma <- readRDS("data/four_kneiser_nay0004.Rds")
no.fifth.gramma <- readRDS("data/fifth_no_kneiser_nay007.Rds")

names(no.fifth.gramma) <- names(kn.four.gramma )

prepare.stream <-function( text.str){
        # text.line = "I'm going to my home. is it true? they're stupid and they'll had truble. He didn't understand I don't want go there. they've been so strong once. it's so crasy!"
        temp <- text.str
        temp <- tolower(temp)
        
        source.particel <- c( "'ll",  "'ve", "'d", "n't", "'s","'m","'re")
        target.particel <- c(" will"," have", " had", " not", " is", " am", " are")
        
        for(i in seq_along(source.particel)) temp <- gsub(source.particel[i], target.particel[i], temp , fixed = TRUE)
        
        temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
        
        # Shrink down to just one white space
        temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
        temp <- stringr::str_trim(temp)
      
        return(temp)
        
}


attach.delimiter.one <-function( text.str){
       
                temp <-   paste0("<s> ",  stringr::str_trim(as.character(text.str) ) )
                
                 return(temp)
                }      

attach.delimiter.double  <-function( text.str){
        temp <-   paste0("<s> <s> ",  stringr::str_trim(as.character(text.str) ) )

        return(temp)
}      

compose.gram <- function(n.tok, w.array){
        
        w.pointer <- length(w.array)
        gram <- w.array[w.pointer]
        if(n.tok == 2){
                gram <-  paste(w.array[w.pointer-1], gram)
        }else if( n.tok == 3){
                gram <-  paste(w.array[w.pointer-1], gram)
                gram <-  paste(w.array[w.pointer-2], gram)
        }else if(n.tok == 4){
                gram <-  paste(w.array[w.pointer-1], gram)
                gram <-  paste(w.array[w.pointer-2], gram)
                gram <-  paste(w.array[w.pointer-3], gram)
        }
        return(gram)
                
}

searchBestPrediction <- function(x2, compose.gram){
        #lookup iniziale sul trigramma
        # se non trovo degrado a bigramma (ma -1)
        # se non trovo degrado a unigramma (ma -1)
        # se non trovo do un mesaggio ukn?
        # proviamo prima con il trigramma
        n.token = 4
        predictor <- compose.gram(n.tok = n.token, w.array = x2)
        candidate <- no.fifth.gramma[no.fifth.gramma$predictor == predictor,  ]
        if(nrow(candidate) == 0){
                
                n.token = 3 
                predictor <- compose.gram(n.tok = n.token, w.array = x2)
                candidate <- kn.four.gramma[kn.four.gramma$predictor == predictor,  ]
                if(nrow(candidate) == 0){
                        n.token = 2 
                        predictor <- compose.gram(n.tok = n.token, w.array = x2)
                        candidate <- kn.tri.gramma[kn.tri.gramma$predictor == predictor,  ]
                        if( nrow(candidate) == 0){
                                n.token = 1 
                                predictor <- compose.gram(n.tok = n.token, w.array = x2)
                                candidate <- kn.bi.gramma[kn.bi.gramma$predictor == predictor,  ]
                                if( nrow(candidate) == 0){
                                        
                                        return(data.frame(predictor = c("predictor"), Pkn = c(0.0), outcome = c("words unknown or no input")) )
                                }
                                
                        }
                }
        }
        candidate <- candidate[with(candidate,order(-Pkn)),]
        return(candidate)
        
        
}

predict <- function( sentence,  compose.gram ){
        
        if( length(grep("[?!,.;:]$", sentence, value = TRUE)) > 0  ){ 
                # settare la sessione?? 
                #return( data.frame(predictor = c("end of sentence"), Pkn = c("na"), outcome = c("wait") ) )
                return(NULL)      
                }
                
        words <- prepare.stream( sentence )
        
        x2 <- unlist( strsplit(words, " ", fixed=TRUE))
        number.of.words <- length( x2)
        
        if(number.of.words == 1 ) {
                words <-  attach.delimiter.double(words)
                x2 <- unlist( strsplit(words, " ", fixed=TRUE))
                
        }else if(number.of.words == 2 ){
                words <- attach.delimiter.one(words)  
                x2 <- unlist( strsplit(words, " ", fixed=TRUE))
                
        }
        
       
        
        return( searchBestPrediction(x2,  compose.gram)   )
}

predict.oneword <- function( sentence,  compose.gram ){
        
       
        cadidate.df <-  predict(sentence,  compose.gram )
        predicted.word <- as.character(  cadidate.df[1,]$outcome)
        if( predicted.word == "</s>"){ predicted.word <- "!" }
        
        return( predicted.word  )
        
}

predict.full <- function( sentence,  compose.gram ){
        if(is.null(sentence)| length(sentence) == 0){
                return(data.frame(predictor = c("predictor"), Pkn = c(0.0), outcome = c("no input")))
               
        }
        
        cadidate.df <-  predict(sentence,  compose.gram )
        if(nrow( cadidate.df) < 1 | is.null(cadidate.df)){
                return(data.frame(predictor = c("predictor"), Pkn = c(0.0), outcome = c("no input")))
               
        }
        
        return(cadidate.df  )
        
}



check.if.formed  <- function(x){
        
        if( length(grep("[?:!,.; ]$", x, value = TRUE)) > 0  ){
                return(TRUE)        
        }
        return(FALSE)
        
        
}

shinyServer(
        function(input, output) {
                #output$table <- renderTable({data.frame()})
                x <-  reactive({ 
                         check.if.formed( input$sentence )
                        
                        })
               
                output$value <- renderText({ 
                        if(is.null(x() )){return(NULL) }
                        if(x()){
                               res <- predict.oneword( input$sentence,compose.gram )
                              return( paste(input$sentence,"<font color=\"#00FF00\"><b>", res,"</b></font>" ))
                        }else( return("wait complition with space ..."  ) )
                      
                        
                })
                
                
                output$alternatives <- renderText({ 
                        if(is.null(x() )){return(NULL) }
                        if(x()){
                                more.predict <-predict.full( input$sentence,compose.gram )
                                if( nrow( more.predict) > 1) {
                                        return(  paste("<font color=\"#FFFF00\"><b>", more.predict[2,]$outcome,"</b></font>" ))
                                }else{
                                        return("no alternatives found")
                                }
                                
                        }else( return("wait complition with space ..."  ) )
                        
                        
                })
               
                
                observeEvent(input$goButton, {
                        if(is.null(x() )){return(NULL) }
                        
                        output$table <- renderTable({ predict.full( input$sentence,compose.gram )})
                        
                        
                       
                })
                
                output$table <- renderTable({ 
                        if(is.null(x() )){return(NULL) }
                        
                        return( data.frame(predictor = c("predictor"), Pkn = c(0.0), outcome = c("no input")))
                        })
                
                
                                           
        }
)
