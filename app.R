require(pairsD3)
x = iris
group = iris[,5]
subset=NULL
labels=NULL

shinyApp(
  ui=fluidPage(
    titlePanel(""),
    fluidRow(
      column(3,
             wellPanel(
               tabsetPanel(
                 tabPanel("Iris",br(),
                          p("Default example using the famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width for 50 flowers from each of 3 species of iris (setosa, versicolor and virginica).")),
                 tabPanel("Upload data",
                          br(),
                          fileInput('csvfile', 'Choose CSV File',
                                    accept=c('text/csv',
                                             'text/comma-separated-values,text/plain',
                                             '.csv')),
                          checkboxInput('header', 'Header', TRUE),
                          radioButtons('sep', 'Separator',
                                       c(Comma=',',
                                         Semicolon=';',
                                         Tab='\t'),
                                       ','))
               )
             ),
             wellPanel(
               uiOutput("varselect"),
               uiOutput("facselect1"),
               uiOutput("facselect2"),
               sliderInput("cex","Size of plotting symbol",3,min=1,max=10),
               sliderInput("opacity","Opacity of plotting symbol",0.9,min=0,max=1),
               radioButtons("theme", "Colour theme",
                            choices = c("Colour"= "colour",
                                        "Monochrome"="bw")),
               #sliderInput("fontsize","Font size",12,min=6,max=24),
               sliderInput("width","Width and height",600,min=200,max=1200),
               radioButtons("table_data_logical", label="Table of data?",
                            choices = c("No" = 0,
                                        "Yes" = 1)),
               conditionalPanel("input.table_data_logical==1",
                                selectInput(inputId="table_data_vars",label="Include all variables in table?",
                                            choices=c("No" = 0,
                                                      "Yes" = 1))
               )
             ),
             wellPanel(
               downloadButton("export",label="Download html file"),
               br(),br(),
               strong("Recreate using this code:"),
               verbatimTextOutput("code")
             ),
             wellPanel(
               icon("warning"),
               tags$small("The pairsD3 package is under active development."),
               tags$small("Get the latest version here: "),
               HTML(paste("<a href=http://github.com/garthtarr/pairsD3>")),
               icon("github"),
               HTML(paste("</a>"))
             )
      ),
      column(9,
             uiOutput("pairsplot"),
             br(),br(),
             dataTableOutput(outputId="outputTable")
      )
    )
  ),
  shinyServer(function(input, output) {
    datain <- reactive({
      if(is.null(input$csvfile)) {
        return(iris)
      } else {
        x = read.csv(input$csvfile$datapath, header=input$header, sep=input$sep)
      }
    })
    output$varselect <- renderUI({
      cols = colnames(datain())
      selectInput("choose_vars", "Select variables to plot:",
                  choices=cols, selected=cols[1:3], multiple=T)
    })
    output$facselect1 <- renderUI({
      radioButtons("factor_var_logical", label="Is there a factor variable?",
                   choices = c("Yes" = 1,
                               "No" = 0),
                   selected = selectedfac())
    })
    output$facselect2 <- renderUI({
      conditionalPanel(
        condition = "input.factor_var_logical == 1",
        selectInput(inputId="factor_var",label="Factor variable:",
                    choices=names(datain()),multiple=FALSE,selected = selectedfacvar())
      )
    })
    
    output$pairsplot = renderUI({
      pairsD3Output("pD3",width = input$width,height=input$width)
    })
    
    output$export = downloadHandler(
      filename = "pairsD3.html",
      content = function(file){
        savePairs(pairsD3(datain(),group=group(), subset=subset, labels = labels,
                          theme = input$theme,
                          width=input$width,
                          opacity = input$opacity,
                          cex = input$cex),
                  file=file)
      }
    )
    
    output$pD3 <- renderPairsD3({
      pairsD3(datain()[,choices()],group=group(), subset=subset, labels = labels,
              theme = input$theme, opacity = input$opacity, cex = input$cex)
    })
    
    output$code = renderText({
      if(is.null(input$csvfile)){
        paircall = "data(iris) \nx = iris \n"
      } else {
        paircall = paste("x = read.csv('",input$csvfile$name,
                         "', header=",input$header,", sep='",input$sep,"') \n",sep="")
      }
      if(length(choices())==dim(datain())[2]){
        paircall = paste(paircall,"pairsD3(","x",sep="")
      } else {
        paircall = paste(paircall,"pairsD3(","x","[,c(",paste(match(choices(),names(datain())),collapse=","),")]",sep="")
      }
      if(!is.null(input$factor_var_logical)){
        if(input$factor_var_logical==1){
          paircall = paste(paircall,", group = ","x[,",match(input$factor_var,names(datain())),"]",sep="")
        }
      }
      if(input$theme=="bw"){
        paircall = paste(paircall,", theme = 'bw'",sep="")
      }
      paircall = paste(paircall,", opacity = ", input$opacity,sep="")
      paircall = paste(paircall,", cex = ", input$cex,sep="")
      return(paste(paircall, ", width = ",input$width,")",sep=""))
    })
    
    group = reactive({
      if(input$factor_var_logical==1){
        return(datain()[,input$factor_var])
      } else return(NULL)
    })
    
    selectedfac = reactive({
      if(is.null(input$csvfile)) {
        return(1)
      } else return(0)
    })
    
    selectedfacvar = reactive({
      if(is.null(input$csvfile)) {
        return("Species")
      } else { # tries to identify the most likely factor variable
        n.fac = function(x){length(levels(as.factor(x)))}
        nfacs = apply(datain(),2,n.fac)
        nfacs = nfacs[nfacs>1] # exclude any vars that are all
        return(names(which.min(nfacs))[1])
      }
    })
    
    choices<-reactive({
      input$choose_vars
    })
    
    output$outputTable = renderDataTable({
      data = datain()
      if(input$table_data_logical==1){
        displayDF <- as.matrix(data) # baseData$df #data sent to d3.js
        n=dim(displayDF)[1]
        dfFilter <- input$selectedobs[1:n] # passed from the web interface
        if (is.null(dfFilter)){
          # no selection has been made
          dfFilter = rep(TRUE,n)
        }
        displayDF <- as.data.frame(cbind(names=row.names(displayDF),
                                         displayDF))
        dfFilter[dfFilter==''] = TRUE
        dfFilter[dfFilter=='greyed'] = FALSE
        if(input$table_data_vars==0){
          return(as.matrix(displayDF[dfFilter == TRUE,choices(),drop=FALSE]))
        } else if(input$table_data_vars==1){
          return(as.matrix(displayDF[dfFilter == TRUE,,drop=FALSE]))
        }
      } else {
        return(NULL)
      }
    },
    options = list(dom = 't<lp>',pageLength = 20,
                   autoWidth = TRUE,
                   lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                   searching = FALSE)
    )
    
  })
)
