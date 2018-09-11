#'rmls
#' @param df A dataframe input
#' @param Y.var Specify value dependent variable 
#' @param Time.var Specify time variables such as Year, Quarter, and Month
#' @param Current.time Specify current time such as 2018
#' @export


rmls <- function(df=.Last.value,
                 Y.var = "",
                 Time.var = "",
                 Current.time = "",
                 launch.browser = False) {
  
  

################################################################################################################
#Non-reactive functions
################################################################################################################
  
options(scipen=999)
  
#Characters to factors
char_vars <- lapply(df, class) == "character"
df[, char_vars] <- lapply(df[, char_vars], as.factor) 
  
#Bring Y vairable name to the first column
X.var <- names(df)[!(names(df) %in% c(Y.var))]
names <- append(Y.var, X.var) 
  
#Make names
df1 <- df %>% dplyr::select(names)
names(df1) <- make.names(names(df1))
Y.var <- make.names(Y.var)
all.elements <- "Show All"
factor.names <- names(df1[,sapply(df1, is.factor)]) # factors to filter
names(df1)[names(df1) == Time.var]<-capitalize(names(df1)[names(df1) == Time.var])


################################################################################################################
#UI.R
################################################################################################################



ui <- dashboardPage(
  
  dashboardHeader(title = "Multiple Regression"), 
  dashboardSidebar(
    br(),
    fluidRow(
      #      column(
      #        width = 12,
      #        uiOutput("ui_update_data")
      #      ),
      column(
        div(
          style = "padding: 15px",
          h4("Data Filter: ")
        ),
        width = 12,
        uiOutput("selects")
      )
    ),
    br(),
    hr()
  ),
  
  dashboardBody(
    tags$style(HTML(".box.box-info {background: #ffffff}
                    .box.box-primary {background: rgba(236,240,245,1)}")),
    tags$style(".span12 {color: black;}"),
    tags$head(tags$style(HTML(".skin-blue .main-sidebar label{color: white;}
                              .skin-blue .main-header .navbar {background-color: #006791;}
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #006791;}
                              .skin-blue .main-header .logo:hover {background-color: #006791;}
                              .skin-blue .main-header .logo {background-color: #006791;}
                              .skin-blue .main-sidebar {background-color:  #434b4e;}
                              "))),
    #    tags$style(type="text/css",
    #               ".shiny-output-error { visibility: hidden; }",
    #               ".shiny-output-error:before { visibility: hidden; }"
    #    ),
    tabsetPanel(
      tabPanel("Multiple Regression",
               icon = shiny::icon("cog"),
               br(),
               box(status = "info", title = "Dashboard", height = "100%",width = "12",solidHeader = F, 
                   fluidRow(
                     box(height = "650px",width = "12",solidHeader = T, 
                         column(width = 12,
                                plotlyOutput("plotly", width = "100%", height = "600px")
                         )
                     )
                   ),
                   br(),
                   fluidRow(
                     box(title = "Data Output", height = "100%",width = "10",solidHeader = T, 
                         column(
                           width = 12,
                           list(
                             div(style = 'overflow-x: scroll',DT::dataTableOutput("df.dt",width = "100%"))
                           ) 
                         )
                     ),
                     br(), 
                     br(),
                     div(
                       style = "padding: 15px",
                       downloadButton(outputId = "df.dt_mydownload", label = "Download Data")
                     )
                   )),
               br(),
               fluidRow(
                 box(status = "primary", height = "250px",width = "12",solidHeader = F,
                     h5(strong("Base Model Formula")),
                     textOutput("Model.formula"),
                     br(),
                     textInput("New.formula", "Enter your new formula to run the model", "Type here"), 
                     actionButton("New.formula.upload", "Upload")
                 ),
                 box(status = "primary", height = "100%",width = "6",solidHeader = F,
                     h5(strong("Model Summary")),
                     h5(strong(verbatimTextOutput("Model.Summary")))
                 ),
                 box(status = "primary", height = "100%",width = "6",solidHeader = F,
                     
                     h5(strong("VIF scores")),
                     p("Interpreting the Variance Inflation Factor: Variance inflation factors range from 1 upwards. The numerical value for VIF tells you (in decimal form) what percentage the variance (i.e. the standard error squared) is inflated for each coefficient."),
                     p("A rule of thumb for interpreting the variance inflation factor:"),
                     p("1 = not correlated / Between 1 and 5 = moderately correlated / Greater than 5 = highly correlated."),
                     h5(strong(verbatimTextOutput("best.variables"))),
                     plotlyOutput("plotly.var", width = "100%", height = "600px")
                 )
               ),
               br(),
               fluidPage(
                 tags$style(HTML(".tabbable > .nav > li > a                           {background-color: #006791; color: white}
                            .tabbable > .nav > li > a[data-value='Time.Series'] {background-color: #006791; color: white}
                            .tabbable > .nav > li > a[data-value='Bass.Diffusion'] {background-color: #006791; color: white}
                           "))
                 
                 
               ))))
  
    )

################################################################################################################
#Server.R
################################################################################################################

server <- function(input, output, session) {
  
  dat0 <- reactive({
    dat <- df1
    return(dat)
  })
  
  
  #Factor filtering
  output$selects <- renderUI({
    req(dat0())
    #Loop through each dimension to build a filter
    lapply(seq_along(factor.names), function(i){
      dat <- dat0()
      
      #Treat all series the same... time, nonagg, etc...
      choice.list <- c(all.elements, unique(dat0()[, factor.names[i]]))
      #Choose that total
      choice.selected <- choice.list[1]
      #Multiple allowed
      choice.mult <- TRUE
      
      
      # Build the Menu for each dimension
      selectInput(
        inputId = paste0("Sel", i),
        label = paste0(factor.names[i]),
        choices = choice.list,
        selected = choice.selected,
        multiple = choice.mult
      )
    })
  })
  
  
  ###
  # Action Button
  ###
  output$ui_update_data <- renderUI({
    actionButton("update_data", label = "Refresh Data", icon = shiny::icon("refresh"),
                 style = "background-color:#17B3D1; color:#ffffff;")
  })
  
  
  
  ###
  #Regression.Formula
  ###
  
  #base.formula
  base.formula <- reactive({
    df1 <- dat0()
    
    
    ########################
    ### Data Exploration ###
    ########################
    pacman::p_load(MASS, clusterGeneration, ROCR, usdm, psych, nFactors, FactoMineR, imputeTS, tidyr, dplyr, plyr, stargazer, dynlm, dyn, zoo, lmtest, clusterSim, car, plm, car, foreign, Hmisc, reshape2)
    
    
    mydata.factor <- df1 %>% dplyr::select(factor.names)
    mydata <- as_tibble(lapply(df1, as.numeric)) #characters -> factor -> numeric
    
    #Correlation
    mydata.cor<-cor(mydata)
    melting.mydata.cor<-arrange(melt(mydata.cor), -abs(value))
    
    Rank.mydata.cor<-dplyr::filter(melting.mydata.cor, value > .5) %>%
      filter(!(value == 1)) 
    
    subY<-subset(Rank.mydata.cor, Var1 %in% Y.var)
    sub.best<- as.character(subY$Var2)
    best.cor <- data.frame(mydata[,sub.best])
    best.cor <- cbind(mydata[1], best.cor)
    
    
    #Cross Validation
    names.best.cor <- names(best.cor)
    y <-names.best.cor[1]
    y.model <- paste(y, '~', '.')
    AIC.lm<-lm(y.model, data=best.cor)
    AIC.cv<-stepAIC(AIC.lm, trace=FALSE)
    AIC.call<-AIC.cv$anova
    AIC.call<-AIC.call[-c(1),]
    
    AIC<-as.character(gsub("-", "", AIC.call$Step))
    AIC<-as.character(gsub(" ", "", AIC))
    
    worst.cv <- data.frame(mydata[,AIC])
    best.cv <- best.cor[,!(colnames(best.cor) %in% c(names(worst.cv)))]
    
    best.CrossValidation <- names(best.cv)
    
    
    
    #Multicollinearity
    
    number.best.cv<-length(best.cv)
    
    i=1
    while(i < number.best.cv){
      
      Y <- best.cor[1]
      mc=lm(y.model, data=best.cv) #vif score changes based on the given data.'while loop' updates best.cv to run on.
      vif<-vif(mc)
      worst.mc<-vif[which(vif == max(vif))]
      best.mc<-vif[-which(vif == max(vif))]
      
      if (worst.mc > 9) {
        best.cv <- best.cor[,(colnames(best.cor) %in% c(names(best.mc)))] 
        best.cv <- cbind(Y, best.cv)
      } else {
        break
      }
      
      i<-i+1
      
    }
    
    
    mc.score<-c(best.mc, worst.mc)
    
    
    best.var<- tibble::rownames_to_column(as.data.frame(mc.score))
    best.var <- best.var %>% arrange(mc.score)
    best.var
    
    
    
    ################################
    ####### Regression Model #######
    ################################
    
    num.best.var<-as.numeric(nrow(best.var[1]))
    
    formula <- paste(sapply(1:num.best.var, function(x){paste(best.var$rowname[x], '+')}))
    last.var <- gsub('\\+', '', formula[num.best.var])
    
    y.names <- names.best.cor[1]
    formula <- formula[-num.best.var]
    formula <- paste(paste(formula, collapse = ""), last.var, collapse = "")
    formula = paste(y, '~', formula)
    formula
    
    
    
    
    return(formula)
  })
  
  #Output: base formula
  output$Model.formula <- renderText({
    mlr.formula <- base.formula()
    mlr.formula
  })
  
  #New.formula
  reactive.formula <- eventReactive(input$New.formula.upload,
                                    ignoreNULL = FALSE, {
                                      formula <- base.formula()
                                      reactive.formula <- if(input$New.formula == "Type here"){formula}else{input$New.formula}
                                      return(reactive.formula)
                                    })
  
  
  #Output: best variables
  
  best.var <- reactive({
    df1 <- dat0()
    
    
    mydata.factor <- df1 %>% dplyr::select(factor.names)
    mydata <- as_tibble(lapply(df1, as.numeric)) #characters -> factor -> numeric
    
    #Correlation
    mydata.cor<-cor(mydata)
    melting.mydata.cor<-arrange(melt(mydata.cor), -abs(value))
    
    Rank.mydata.cor<-dplyr::filter(melting.mydata.cor, value > .5) %>%
      filter(!(value == 1)) 
    
    subY<-subset(Rank.mydata.cor, Var1 %in% Y.var)
    sub.best<- as.character(subY$Var2)
    best.cor <- data.frame(mydata[,sub.best])
    best.cor <- cbind(mydata[1], best.cor)
    
    
    #Cross Validation
    names.best.cor <- names(best.cor)
    y <-names.best.cor[1]
    y.model <- paste(y, '~', '.')
    AIC.lm<-lm(y.model, data=best.cor)
    AIC.cv<-stepAIC(AIC.lm, trace=FALSE)
    AIC.call<-AIC.cv$anova
    AIC.call<-AIC.call[-c(1),]
    
    AIC<-as.character(gsub("-", "", AIC.call$Step))
    AIC<-as.character(gsub(" ", "", AIC))
    
    worst.cv <- data.frame(mydata[,AIC])
    best.cv <- best.cor[,!(colnames(best.cor) %in% c(names(worst.cv)))]
    
    best.CrossValidation <- names(best.cv)
    
    
    
    #Multicollinearity
    
    number.best.cv<-length(best.cv)
    
    i=1
    while(i < number.best.cv){
      
      Y <- best.cor[1]
      mc=lm(y.model, data=best.cv) #vif score changes based on the given data.'while loop' updates best.cv to run on.
      vif<-vif(mc)
      worst.mc<-vif[which(vif == max(vif))]
      best.mc<-vif[-which(vif == max(vif))]
      
      if (worst.mc > 9) {
        best.cv <- best.cor[,(colnames(best.cor) %in% c(names(best.mc)))] 
        best.cv <- cbind(Y, best.cv)
      } else {
        break
      }
      
      i<-i+1
      
    }
    
    
    mc.score<-c(best.mc, worst.mc)
    
    
    best.var<- tibble::rownames_to_column(as.data.frame(mc.score))
    best.var <- best.var %>% arrange(mc.score)
    best.var
    
    return(best.var)
  })
  
  output$best.variables <- renderPrint({
    best.variables <- best.var()
    best.variables
  })
  
  ###
  #Regression.Model 
  ###
  
  #Regression Model
  df2 <- reactive({
    df1 <- dat0()
    
    #Model
    m1 <-lm(reactive.formula(), data = df1[df1$Year<= Current.time,])
    
    #Prediction
    df1$pred.m1 <- predict(m1, newdata = df1)
    last.col <- as.numeric(ncol(df1))
    df1 <- df1 %>% dplyr::select(c(1, last.col, everything()))
    return(as.data.frame(df1))
    
  })
  
  #Output: Model Summary
  Model.Summary <- reactive({
    df1 <- dat0()
    m1 <-lm(reactive.formula(), data = df1[df1$Year<= Current.time,])
    summary(m1)
  })
  
  output$Model.Summary <- renderPrint({
    mlr.summary <- Model.Summary()
    mlr.summary
  })
  
  
  
  
  #Factor filters 
  df.filter <- reactive({
    
    dat <- df2()
    datF <- dat
    
    for(i in seq_along(factor.names)){
      get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check
      
      #If no items are selected or the Select All is selected, show ALL items
      if(length(get_input) == 0 || all.elements %in% get_input){
        get_series <- tibble::as.tibble(dat[, factor.names[i]]) %>% dplyr::distinct() %>% dplyr::pull()
        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(factor.names[i])) #If a Filter is selected....
      } else {
        get_series <- as.character(get_input)
        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(factor.names[i])) #If a Filter is selected....
      }
      
      #.... Do this
      datF <- datF %>%
        dplyr::filter_(filter_criteria_T)
      
    } #End for
    
    return(as.data.frame(datF))
  })
  
  
  
  df.filtered <- reactive({
    df.filtered <- df.filter()
    
    df.filtered <- df.filtered %>%
      dplyr::select(-one_of(factor.names))%>%  ## Remove Factors 
      dplyr::group_by(Year) %>%  ## Y variable
      dplyr::summarize_all(sum)
    
    return(as.data.frame(df.filtered))
  })
  
  
  #Output DataTable
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
  }
  
  
  
  output$df.dt <- renderDataTable({
    df.dt <- df.filtered() 
    datatable(round_df(df.dt, digits =2),
              options = list(searching = FALSE, paging = FALSE))
  })
  
  
  #Output Download data
  output$df.dt_mydownload <-downloadHandler(
    filename = "Data_Output.csv", 
    content = function(file){
      write.csv(df.filtered(), file)
    })
  
  
  #plotly Output
  output$plotly <-renderPlotly({
    
    df.plot <- df.filtered()
    
    #plots
    
    x <- c(1:nrow(df.plot))
    
    plotly <- plot_ly(df.plot, x = ~Year) %>%
      add_trace(y = df.plot$pred.m1, name = 'MLR', type = 'scatter', mode = 'lines', width = 2, line = list(dash = 3, color = '#919EAD'))%>%  
      add_trace(y = unlist(df.plot[2]), name = 'Value', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#1f78b4'))%>%
      layout(title = "Multiple Linear Regression",
             margin = list(t= 30, b = 70), xaxis = list(title = ""), yaxis = list (title = ""), showlegend = TRUE)
    
    plotly
    
    
  })
  
  
  #Plot - variables
  output$plotly.var <-renderPlotly({
    
    best.var <- best.var()
    num.best.var<-as.numeric(nrow(best.var[1]))
    best.var.names <- unlist(best.var$rowname)
    
    df.var.plot <- df.filtered()
    
    
    
    #plots
    
    x <- c(1:nrow(df.var.plot))
    
    plotly.var <- df.var.plot %>% 
      dplyr::select(Time.var, best.var.names) %>%
      tidyr::gather(variable, value, -Time.var) %>%
      plot_ly(x = ~Year, y = ~value, color = ~ variable) %>%
      layout(legend = list(x = 0.02, y = 0.98, bgcolor = "transparent"))%>%
      add_lines() 
    
    plotly.var
    
    
  })
  
  
  
}

runGadget(ui, server, viewer = browserViewer())

}
