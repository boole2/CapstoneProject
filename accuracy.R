

library(parallel)
setwd("/home/boole2/coursera/CapstonePrj/week6")
kn.bi.gramma <- readRDS("../week5/bi_kneiser_nay0004.Rds")
kn.tri.gramma <- readRDS("../week5/tri_kneiser_nay0004.Rds")
kn.four.gramma <- readRDS("../week5/four_kneiser_nay0004.Rds")
test.set <- readRDS("../week6/gram_test_2start.Rds")



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

searchBestPrediction <- function(x2, compose.gram, kn.bi.gramma, kn.tri.gramma, kn.four.gramma){
        #lookup iniziale sul trigramma
        # se non trovo degrado a bigramma (ma -1)
        # se non trovo degrado a unigramma (ma -1)
        # se non trovo do un mesaggio ukn?
        # proviamo prima con il trigramma
       
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
                                
                                return(data.frame(predictor = c(predictor), Pkn = c(0.0), outcome = c("<UKN>") ) )
                        }
                        
                }
        }
        candidate <- candidate[with(candidate,order(-Pkn)),]
        return(candidate)
        
        
}



match_count <- function(i, test.set, searchBestPrediction, compose.gram,
                        kn.bi.gramma, kn.tri.gramma, kn.four.gramma){
  
  res <- FALSE
  x2 <- unlist( strsplit(as.character(test.set[i,]$predecessor), " ", fixed=TRUE))
  predicts <- searchBestPrediction(x2,  compose.gram,
                                   kn.bi.gramma = kn.bi.gramma, kn.tri.gramma = kn.tri.gramma, kn.four.gramma = kn.four.gramma)
  
  out <-  as.character( predicts[1,]$outcome)
  actual <- as.character( test.set[i,]$successor)
  if( out  ==    actual){  res <- TRUE}
  else if(nrow(predicts) > 1 ){
    out <-  as.character( predicts[2,]$outcome)
    if( out  ==    actual){  
      res <- TRUE}
    else if(nrow(predicts) > 2 ){
      out <-  as.character( predicts[3,]$outcome)
      if( out  ==    actual){  
        res <- TRUE}
      
    }
  }
  
  return(list(out,actual, res))
  
}




no_cores <- detectCores() - 1
pointer <- array(1:nrow(test.set))
cl <- makeCluster(no_cores)
tot <-  parApply(cl, pointer, 1, FUN = match_count,  test.set = test.set, searchBestPrediction, compose.gram,
                     kn.bi.gramma = kn.bi.gramma, kn.tri.gramma = kn.tri.gramma, kn.four.gramma = kn.four.gramma)
                     
stopCluster(cl)


# tot.df <- data.frame( unlist(tot))
# names(tot.df ) <- "res"

cl <- makeCluster(no_cores)
x2 =  parLapply(cl, tot , function(l) l[c(1,2,3)])
stopCluster(cl)

accuracy.res <- data.frame( matrix(unlist(x2), ncol = 3, byrow = TRUE) )
names(accuracy.res) <-   c("out", "actual","match")



(sum(as.numeric( accuracy.res$match)-1)/nrow(test.set))*100




