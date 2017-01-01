library(shiny)


shinyUI(
        fluidPage(
                theme = "bootstrap.css",
                title = "Kneser Ney based Words predictor",
                HTML(" <font color=\"#FFFFF\"><b> Word Predictor base on Kneser Ney Interpolation </b></font> "),
                HTML("<hr>"),
                br(),
                fluidRow(
                        column( "Instractions", width = 4, wellPanel(
                                HTML("You can enter sentence by  <font color=\"#00BBF\"><b>  \'Input Sentence Area\'  </b></font> . <br> ",
                                       "Use space between words to  obtain a forseen sentence. <br> ",
                                       "The first sudjested word is displayed in <font color=\"#00FF00\"><b> green </b></font> after the original sentence your had enter. <br>",
                                       "Alternative word, if found, is displayed in yellow in the <font color=\"#00BBF\"><b> \'Other Sudjested Words  Area\'</b></font> .<br> ",
                                       "End of sentence are predicted and displayed by \'!\' point. <hr>",
                                     "To look at the underline analysis, push the button:  <font color=\"#00BBF\"><b> \'Enable Analysis\'</b></font>.",
                                     "This action enable the trace  of the Kneser Ney probability found for  3-gram,2-gram,1-gram interpolation.<br>",
                                     "The table display three columns:  <font color=\"#00BBFA\"><b> Predictor </b></font>  is the N-gram used as a predictor by the algorithm,  ",
                                     "  <font color=\"#00BBFA\"><b> Pkn </b></font>  is the  Kneser Ney probability computed by interpolating",
                                     " N-gram probability with the  N-i+1 Sub gram, <font color=\"#00BBFA\"><b> outcome </b></font> the correspondig predicted (next) word.",
                                   
                                     "<hr> Note: end of sentence, considered to be one of the punctuations mark: \' . : ; ? ! ,\'  is also predicted by the generic symbol &lt;\\/s &gt;",
                                        "Beging of sentence are indicated in the predictor by symbols &lt; s &gt; and is used in multipe cadinality depending by the N-gram involved as predictor"
                                     
                                     
                                     )
                                )
                             
                        ),
                        column("Apps Area", width = 4, wellPanel(
                               textInput("sentence", "Input Sentence", value = "", width = '400px', placeholder = "this is an example"),
                               HTML("<hr> <font color=\"#FFFFFF\"><b> Forseen  Sentence </b></font>"),
                               br(),
                               htmlOutput("value"),  
                               HTML("<font color=\"#FFFFF\"><b> Others Sudjested Words </b></font>"), 
                               htmlOutput("alternatives"), 
                               HTML("<hr>"),
                               br(),
                               actionButton("goButton", "Enable Analysis"),
                               tableOutput("table")
                            
                               
                                )
                        )
                       
                        
                )
        )
        
)
