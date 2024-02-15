library(quantmod)
library(dygraphs)

sym_bols <- c("LUMN")

dateWindow <- c("2024-01-01", paste(Sys.Date()))

data_env <- new.env()

get_s <- function(symb){
  if (is.atomic(symb)){
    getSymbols(paste(sym_bols), from="2018-01-01") # wenn sym_bols über Vektor
  }
  else {
    getSymbols(paste(sym_bols$symb), from="2018-01-01") # wenn sym_bols über tibble
  }
}

get_s(sym_bols)


getSymbols("ADCT")



ADCT$d_HLDiff <- (ADCT$ADCT.High-ADCT$ADCT.Low)
ADCT$d_HLDiff_pr <- ((ADCT$ADCT.High-ADCT$ADCT.Low)/ADCT$ADCT.Low)*100

ADCT$d_OCDiff <- (ADCT$ADCT.Close-ADCT$ADCT.Open)
ADCT$d_OCDiff_pr <- ((ADCT$ADCT.Close-ADCT$ADCT.Open)/ADCT$ADCT.Open)*100



library(dygraphs)


dygraph(ADCT[, c(1:4, 8)], group = "swissquote", 
        main=paste(gsub("\\..*","",colnames(ADCT)[1])),
        width=800, height=800) %>% dygraphs::dyCandlestick() %>%
  dyRangeSelector(dateWindow = dateWindow) %>%
  dySeries("d_HLDiff_pr", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "red") %>% 
  dyAxis("y", label = "Price") %>%
  dyAxis("y2", label = paste(colnames(ADCT)[8]), valueRange = c(0, max(ADCT$d_HLDiff_pr)), independentTicks = TRUE)



library(htmltools)
dy_graph <- list(
  dygraph(ADCT[, c(1:4, 8)], group = "swissquote", 
          main=paste(gsub("\\..*","",colnames(ADCT)[1])),
          width=800, height=800) %>% dygraphs::dyCandlestick() %>%
    dyOptions(digitsAfterDecimal=5, drawXAxis = F) %>% 
    dySeries("d_HLDiff_pr", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "red") %>% 
    dyAxis("y", label = "Price") %>%
    dyAxis("y2", label = paste(colnames(ADCT)[8]), valueRange = c(0, max(ADCT$d_HLDiff_pr)), independentTicks = TRUE),
  
  dygraph(ADCT$ADCT.Volume, group = "swissquote", width=800, height=200) %>% 
    dyOptions(maxNumberWidth = 100000) %>% 
    dyRangeSelector(dateWindow = dateWindow) %>% 
    dySeries("ADCT.Volume", axis = "y2", stepPlot = TRUE, fillGraph = TRUE, color = "red") %>% 
    dyAxis("y2", label = " ")
)  # end list

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))
