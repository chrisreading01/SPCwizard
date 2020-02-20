library(shiny)
#library(infoPack) ##locally created package which I usually call the SPC function from
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(DT)

## Create SPC function
spc <- function(
    #function arguments
    data
    ,dates
    ,improvement.direction = "increase"
    ,target = NULL
    ,trajectory = NULL
    ,rebase = FALSE
    ,data.as.percentages = FALSE
    ,main.title = "SPC Chart"
    ,x.title = "Date"
    ,y.title = "Volume"
    ,x.axis.breaks = length(data)/4
    ,point.size = 4
    ,return.table = FALSE
    ,display.legend = FALSE
) {
    # Colour Palette ----
    .darkgrey = "#7B7D7D"
    .orange = "#fab428"
    .skyblue = "#289de0"
    .purple = "#361475"
    .red = "#de1b1b"
    
    # Variables ----
    x <- data
    limitbreak <- 2.66
    limitbreakclose <- (limitbreak/3)*2
    xl <- length(x)
    xn <- length(x) - 1
    improvementdirection <- case_when(improvement.direction == "Decrease" ~ -1, TRUE ~ 1)
    target <- if(!(is.null(target))) as.numeric(target) else rep(NA,xl)
    trajectory <- if(!(is.null(trajectory))) trajectory else rep(NA,xl)
    plottitle <- main.title
    xlabel <- x.title
    ylabel <- y.title
    xaxis <- as.Date(dates,"%d/%m/%Y")
    
    # Create Data Frame ----
    df <- data.frame(
        n = c(1:xl)
        ,xaxislabels = xaxis
        ,data = x
        ,movingrange = abs(x - c(NA,x[1:xn]))
        ,movingrangeaverage = mean(abs(x - c(NA,x[1:xn])),na.rm = TRUE)
        ,mean = mean(x)
        ,lpl = mean(x) - (limitbreak * mean(abs(x - c(NA,x[1:xn])),na.rm = TRUE))
        ,upl = mean(x) + (limitbreak * mean(abs(x - c(NA,x[1:xn])),na.rm = TRUE))
        ,outsidelimits = case_when(x > mean(x) + (limitbreak * mean(abs(x - c(NA,x[1:xn])),na.rm = TRUE)) ~ 1
                                   ,x < mean(x) - (limitbreak * mean(abs(x - c(NA,x[1:xn])),na.rm = TRUE)) ~ 1
                                   ,TRUE ~ 0
        )
        ,relativetomean = case_when(x > mean(x) ~ 1
                                    ,x < mean(x) ~ -1
                                    ,TRUE ~ 0
        )
        ,target = target
        ,trajectory = as.numeric(trajectory)
        ,upper2sigma = mean(x) + (limitbreakclose * mean(abs(x - lag(x,1)),na.rm = TRUE))
        ,lower2sigma = mean(x) - (limitbreakclose * mean(abs(x - lag(x,1)),na.rm = TRUE))
    )
    
    runrebase <- FALSE
    if(rebase != FALSE) {rebase <- strsplit(rebase,";")[[1]]
    runrebase <- TRUE}
    if(runrebase)
    {
        lr <- length(rebase)
        for(n in 1:lr) {
            print(rebase[n])
            newdata <- df$data[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")]
            print(newdata)
            df$mean[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- mean(newdata)
            df$lpl[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- mean(newdata) - (limitbreak * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE))
            df$upl[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- mean(newdata) + (limitbreak * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE))
            df$outsidelimits[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- case_when(newdata > mean(newdata) + (limitbreak * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE)) ~ 1
                                                                                                   ,newdata < mean(newdata) - (limitbreak * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE)) ~ 1
                                                                                                   ,TRUE ~ 0
            )
            df$relativetomean[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- case_when(newdata > mean(newdata) ~ 1
                                                                                                    ,newdata < mean(newdata) ~ -1
                                                                                                    ,TRUE ~ 0
            )
            df$upper2sigma[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- mean(newdata) + (limitbreakclose * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE))
            df$lower2sigma[xaxis >= as.Date.character(rebase[n],format="%d/%m/%Y")] <- mean(newdata) - (limitbreakclose * mean(abs(newdata - lag(newdata,1)),na.rm = TRUE))
        }
    }
    
    df <- df %>%
        mutate(closetolimits = case_when(data > upper2sigma & data <= upl ~ 1
                                         ,data < lower2sigma & data >= lpl ~ -1
                                         
        )
        ) %>%
        mutate(sevenpointtrend = case_when(relativetomean == lag(relativetomean,1)
                                           & relativetomean == lag(relativetomean,2)
                                           & relativetomean == lag(relativetomean,3)
                                           & relativetomean == lag(relativetomean,4)
                                           & relativetomean == lag(relativetomean,5)
                                           & relativetomean == lag(relativetomean,6)
                                           ~ 1
                                           ,TRUE ~ 0
        )
        ) %>%
        mutate(partoftrend = case_when(sevenpointtrend == 1
                                       | lead(sevenpointtrend,1) == 1
                                       | lead(sevenpointtrend,2) == 1
                                       | lead(sevenpointtrend,3) == 1
                                       | lead(sevenpointtrend,4) == 1
                                       | lead(sevenpointtrend,5) == 1
                                       | lead(sevenpointtrend,6) == 1
                                       ~ 1
                                       ,TRUE ~ 0
        )
        ) %>%
        mutate(sixpointgrowth = case_when(data > lag(data,1)
                                          & lag(data,1) > lag(data,2)
                                          & lag(data,2) > lag(data,3)
                                          & lag(data,3) > lag(data,4)
                                          & lag(data,4) > lag(data,5)
                                          & lag(data,5) > lag(data,6)
                                          ~ 1
                                          ,data < lag(data,1)
                                          & lag(data,1) < lag(data,2)
                                          & lag(data,2) < lag(data,3)
                                          & lag(data,3) < lag(data,4)
                                          & lag(data,4) < lag(data,5)
                                          & lag(data,5) < lag(data,6)
                                          ~ -1
                                          ,TRUE ~ 0
        )
        ) %>%
        mutate(partofgrowth = case_when(abs(sixpointgrowth) == 1
                                        | abs(lead(sixpointgrowth,1)) == 1
                                        | abs(lead(sixpointgrowth,2)) == 1
                                        | abs(lead(sixpointgrowth,3)) == 1
                                        | abs(lead(sixpointgrowth,4)) == 1
                                        | abs(lead(sixpointgrowth,5)) == 1
                                        #| lead(sevenpointgrowth,6) == 1
                                        ~ 1
                                        ,TRUE ~ 0
        )
        ) %>%
        mutate(twointhree = case_when(closetolimits == 1
                                      & (
                                          lead(closetolimits,1) == 1
                                          | lead(closetolimits,2) == 1
                                      ) ~ 1
                                      ,closetolimits == -1
                                      & (
                                          lead(closetolimits,1) == -1
                                          | lead(closetolimits,2) == -1
                                      )
                                      ~ 1
                                      ,closetolimits == 1
                                      & (
                                          lag(closetolimits,1) == 1
                                          | lag(closetolimits,2) == 1
                                      ) ~ 1
                                      ,closetolimits == -1
                                      & (
                                          lag(closetolimits,1) == -1
                                          | lag(closetolimits,2) == -1
                                      )
                                      ~ 1
        )
        ) %>%
        #mutate(partoftwointhree = case_when(twointhree == 1 && lag(twointhree,1) == 1
        #                              && (
        #                                lead(closetolimits,1) == 1
        #                                || lead(closetolimits,2) == 2
        #                              ) ~ 1
        #)
        #) %>%
        mutate(specialcauseflag = case_when(abs(outsidelimits) == 1
                                            | abs(partoftrend) == 1
                                            | abs(partofgrowth) == 1
                                            | twointhree == 1
                                            ~ 1
        )
        ) %>%
        mutate(specialcauseconcern = case_when(specialcauseflag == 1
                                               & relativetomean == (improvementdirection * -1)
                                               ~ data
        )
        
        ) %>%
        mutate(specialcauseimprovement = case_when(specialcauseflag == 1
                                                   & relativetomean == improvementdirection
                                                   ~ data
        )
        ) %>%
        mutate(commoncause = case_when(specialcauseflag != 1
                                       ~ data
        )
        )
    xaxis <- as.Date(xaxis)
    xaxisdisplay <- xaxis[seq(1,length(xaxis),x.axis.breaks)]
    
    # Create Plot ----
    plot <- ggplot(df,aes(x=xaxis,y=data)) +
        theme_classic() +
        geom_line(color=.darkgrey,size=point.size/2.666666) +
        geom_point(color=.darkgrey,size=point.size) +
        geom_line(aes(y=upl),linetype = "dashed",size=point.size/2.666666,color=.darkgrey) +
        geom_line(aes(y=lpl),linetype = "dashed",size=point.size/2.666666,color=.darkgrey) +
        geom_line(aes(y=as.numeric(target)),linetype = "dashed",size=point.size/2.666666,color=.purple) +
        geom_line(aes(y=trajectory),linetype = "dashed",size=point.size/2.666666,color=.red) +
        geom_line(aes(y=mean)) +
        #geom_line(aes(y=lower2sigma)) +
        geom_point(aes(x=xaxis,y=df$specialcauseimprovement),color=.skyblue,size=point.size) +
        geom_point(aes(x=xaxis,y=df$specialcauseconcern),color=.orange,size=point.size) +
        ggtitle(label = plottitle) +
        xlab(label = xlabel) +
        ylab(label = ylabel) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_date(breaks=xaxisdisplay) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(display.legend == TRUE) {
        plot <- plot +
            geom_text(aes(y = mean
                          ,label = c("Mean",rep(NA,length(mean)-1)))
                      ,vjust = -0.5
                      ,hjust = 0
                      ,size = 4
                      ,colour = "grey"
                      ,family = "Arial") +
            geom_text(aes(y = upl
                          ,label = c("Upper Control Limit",rep(NA,length(mean)-1)))
                      ,vjust = -0.5
                      ,hjust = 0
                      ,size = 4
                      ,colour = "grey"
                      ,family = "Arial") +
            geom_text(aes(y = lpl
                          ,label = c("Lower Control Limit",rep(NA,length(mean)-1)))
                      ,vjust = -0.5
                      ,hjust = 0
                      ,size = 4
                      ,colour = "grey"
                      ,family = "Arial")
    }
    
    if(!(is.null(target)) && display.legend == TRUE) {
        plot <- plot +
            geom_text(aes(y = target
                          ,label = c("Target",rep(NA,length(mean)-1)))
                      ,vjust = -0.5
                      ,hjust = 0
                      ,size = 4
                      ,colour = .purple
                      ,family = "Arial")
    }
    
    if(data.as.percentages == TRUE){
        upperlimit <- max(data,na.rm = TRUE)*2
        if(is.na(upperlimit)) {upperlimit <- 1}
        upper <- max(x,na.rm = TRUE)
        lower <- min(x,na.rm = TRUE)
        interval <- plyr::round_any((upper-lower)/10,0.01)
        if(interval <= 0){interval <- 0.01}
        #print(upperlimit)
        plot <- plot +
            scale_y_continuous(labels = scales::percent,breaks = seq(from = 0,to = upperlimit,by = interval))
    }
    
    if(return.table == FALSE) {return(plot)}
    
    if(return.table == TRUE) {
        data.frame(
            Index = df$n
            ,Date = df$xaxislabels
            ,Data = df$data
            ,Mean = df$mean
            ,"Upper Process Limit" = df$upl
            ,"Lower Process Limit" = df$lpl
        )
    }
    
}


## Matrix lkup for small multiples
grobsizes <- matrix(
    data = c(
        rep(1,2)
        ,rep(2,4)
        ,rep(3,3)
        ,rep(4,2)
        ,3
        ,rep(4,4)
        ,rep(5,3)
        ,4
        ,rep(5,5)
        ,rep(6,4)
        ,5
        ,rep(6,6)
        ,rep(7,6)
        ,rep(8,6)
        ,rep(9,6)
        ,rep(10,6)
        ,rep(11,6)
        ,rep(12,6)
        ,rep(13,6)
        ,rep(14,6)
        ,rep(15,6)
        ,rep(16,6)
        ,rep(17,6)
        ,rep(18,6)
        ,rep(19,6)
        ,rep(20,6)
        ,rep(21,6)
        ,rep(22,6)
        ,rep(23,6)
        ,rep(24,6)
        ,rep(25,6)
        ,rep(26,6)
        ,rep(27,6)
        ,rep(28,6)
        ,rep(29,6)
        ,rep(30,6)
        ,1
        ,rep(2,3)
        ,rep(3,7)
        ,rep(4,8)
        ,rep(5,10)
        ,rep(6,151)
    )
    ,ncol = 2
    ,nrow = 180
)


ui <- dashboardPage(
    dashboardHeader(title = "WAHT SPC Wizard")
    ####Sidebar ----
    ,dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Instructions", tabName = "notes")
            ,menuItem(text = "Standard SPC Chart", tabName = "singlespc")
            ,menuItem(text = "Small Multiples SPC Chart", tabName = "multispc")
        )
    )
    ####Tabs ----
    ,dashboardBody(
        tabItems(
            tabItem(tabName = "notes"
                ,fluidRow(box(
                    title = "Introduction"
                    ,solidHeader = TRUE
                    ,status = "primary"
                    ,collapsible = FALSE
                    ,textOutput("intro1")
                    ,width = 12
                ))
                ,fluidRow(box(
                    title = "Instructions"
                    ,solidHeader = TRUE
                    ,status = "primary"
                    ,collapsible = FALSE
                    ,div(textOutput("intro2"))
                    ,tags$p(textOutput("intro3"))
                    ,width = 12
                ))  
                ,fluidRow(box(
                    title = "Options"
                    ,solidHeader = TRUE
                    ,status = "primary"
                    ,collapsible = FALSE
                    ,tags$p(textOutput("intro4"))
                    ,tags$ul(
                        tags$li(textOutput("intro4a"))
                        ,tags$li(textOutput("intro4b"))
                        ,tags$li(textOutput("intro4c"))
                        ,tags$li(textOutput("intro4d"))
                        ,tags$li(textOutput("intro4e"))
                        ,tags$li(textOutput("intro4f"))
                        ,tags$li(textOutput("intro4g"))
                        ,tags$li(textOutput("intro4h"))
                        ,tags$li(textOutput("intro4i"))
                        ,tags$li(textOutput("intro4j"))
                        ,tags$li(textOutput("intro4k"))
                    )
                    #,tags$p(textOutput("intro3"))
                    ,width = 12
                ))  
                ,fluidRow(box(
                    title = "Dataset Guidance"
                    ,solidHeader = TRUE
                    ,status = "primary"
                    ,collapsible = FALSE
                    ,tags$p("The dataset CSV file provided should follow the following guidelines:")
                    ,tags$ul(
                        tags$li("Each data series included should be of the same length, and should be in its own column,")
                        ,tags$li("Each data series column should have its own unique title,")
                        ,tags$li("If a data series has a trajectory which should be included on the SPC output, this should also be in its own column, with a column title in the format '[MetricName]_trajectory',")
                        ,tags$li("If dates are provided in the CSV, these should be in their own column with a title of 'Date'.")
                    )
                    #,tags$p(textOutput("intro3"))
                    ,width = 12
                ))  
            )
            ,tabItem(tabName = "singlespc"
                 ,fluidRow(box(
                     title = "Control Panel"
                     ,solidHeader = TRUE
                     ,status = "warning"
                     ,collapsible = TRUE
                     ,fluidRow(
                         box(
                            fileInput("file1", "Choose CSV File",
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
                            )
                            ,selectInput(inputId = "dataset", label = "Select Dataset",choices = "")
                            ,selectInput(inputId = "improv", label="Improvement Direction",choices=c("Increase","Decrease"),selected="Increase")  
                            ,numericInput(inputId="target",label="Target",value=NA)  
                            ,textInput(inputId = "rebase", label = "Rebase Dates (split with ;)")
                            ,width = 4
                            ,solidHeader = TRUE
                        )
                        ,box(
                            checkboxInput(inputId="dataaspercentages",label="Data in Percentage Format",value=FALSE)
                            ,checkboxInput(inputId="sectionTitles",label="Customise Titles",value=FALSE)
                            ,  conditionalPanel(
                                condition = "input.sectionTitles == 1"
                                ,  textInput(inputId="title",label="Chart Title",value="SPC Chart")
                                ,  textInput(inputId="ytitle",label="Y Axis Title",value="Value")
                                ,  textInput(inputId="xtitle",label="X Axis Title",value="Interval")
                            )
                            ,width = 4
                            ,solidHeader = TRUE
                            ,radioButtons(
                                inputId="dateMethod"
                                ,label="Date Method:"
                                ,choices=c("Inherit from CSV" = "Inherit from CSV","Set Manually" = "Set Manually")
                                ,selected = "Inherit from CSV"
                            )
                            , conditionalPanel(
                                condition = 'input.dateMethod == "Set Manually"'
                                ,dateInput(inputId="xdate",label="Start Date",value=Sys.Date())
                                ,  selectInput(inputId = "inttype", label="Interval Type",choices=c("Month","Week","Day"),selected="Month")  
                                ,  numericInput(inputId="int",label="Interval Size",1,max=50) 
                            )
                            ,actionButton(inputId = "interventions",label = "Add system interventions")
                        )
                        ,box(
                            sliderInput(inputId="pointsize",label="Plotting Point Size",4,min=1,max=10)  
                            ,sliderInput(inputId="breaks",label="X Axis Tick Frequency",3,min=1,max=12) 
                            ##,sliderInput(inputId="plotHeight",label = "Plot Area Height",300,min = 100, max = 1200, step = 50)
                            ,width = 4
                            ,solidHeader = TRUE
                        )
                     )
                     ,width = 12
                 ))
                 ,fluidRow(box(
                     title = "SPC Chart"
                     ,solidHeader = TRUE
                     ,status = "primary"
                     ,collapsible = FALSE
                     ,box(addSpinner(plotOutput("plot1",height = "600px") ,spin = "cube-grid", color = "#33FFBD")
                        ,width = 11.5
                        ,solidHeader = TRUE
                     )
                     #,dataTableOutput("interventionsummary")
                     ,width = 12
                 )  
            ))
            ,tabItem(tabName = "multispc"
                 ,fluidRow(box(
                     title = "Control Panel"
                     ,solidHeader = TRUE
                     ,status = "warning"
                     ,collapsible = TRUE
                     ,fluidRow(
                         box(
                             fileInput("file2", "Choose CSV File",
                                       accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                             )
                             ,selectInput(inputId = "improv2", label="Improvement Direction",choices=c("Increase","Decrease"),selected="Increase")  
                             ,numericInput(inputId="target2",label="Target",value=NA)  
                             ,textInput(inputId = "rebase2", label = "Rebase Dates (split with ;)")
                             ,width = 4
                             ,solidHeader = TRUE
                         )
                         ,box(
                             checkboxInput(inputId="dataaspercentages2",label="Data in Percentage Format",value=FALSE)
                             ,checkboxInput(inputId="sectionTitles2",label="Customise Titles",value=FALSE)
                             ,  conditionalPanel(
                                 condition = "input.sectionTitles2 == 1"
                                 ,  textInput(inputId="ytitle2",label="Y Axis Title",value="Value")
                                 ,  textInput(inputId="xtitle2",label="X Axis Title",value="Interval")
                             )
                             ,width = 4
                             ,solidHeader = TRUE
                             ,radioButtons(
                                 inputId="dateMethod2"
                                 ,label="Date Method:"
                                 ,choices=c("Inherit from CSV" = "Inherit from CSV","Set Manually" = "Set Manually")
                                 ,selected = "Inherit from CSV"
                             )
                             , conditionalPanel(
                                 condition = 'input.dateMethod2 == "Set Manually"'
                                 ,dateInput(inputId="xdate2",label="Start Date",value=Sys.Date())
                                 ,  selectInput(inputId = "inttype2", label="Interval Type",choices=c("Month","Week","Day"),selected="Month")  
                                 ,  numericInput(inputId="int2",label="Interval Size",1,max=50) 
                             )
                         )
                         ,box(
                             sliderInput(inputId="pointsize2",label="Plotting Point Size",4,min=1,max=10)  
                             ,sliderInput(inputId="breaks2",label="X Axis Tick Frequency",3,min=1,max=12) 
                             ,sliderInput(inputId="plotHeight2",label = "Plot Area Height",750,min = 100, max = 4000, step = 100)
                             ,width = 4
                             ,solidHeader = TRUE
                         )
                     )
                     ,width = 12
                 ))
                 ,fluidRow(box(
                     title = "SPC Chart"
                     ,solidHeader = TRUE
                     ,status = "primary"
                     ,collapsible = FALSE
                     ,box(addSpinner(uiOutput("plot2UI"),spin = "cube-grid", color = "#33FFBD")
                          ,width = 11.5
                          ,solidHeader = TRUE
                     )
                     ,width = 12
                 )  
                 )
            )
        )
    )
)


server <- function(input, output, session) {

    #### Intro Sheet ----
    output$intro1 <- renderText("Welcome to the Worcestershire Acute NHS Trust SPC wizard. This tool has been designed by the Worcestershire Acute Information Department to assist NHS Information and BI teams in creating SPC charts in accordance with NHSE/I's 'Making Data Count' SPC methodology. This tool is powered by R Shiny.")
    
    output$intro2 <- renderText("To use this wizard, you will need a CSV file with a column(s) containing your data series, with a series title in the first row. An optional 'Date' column is recommended, to provide the date interval for each data point. If not supplied, the date range and intervals can be manually provided using the wizard.")
    
    output$intro3 <- renderText("Charts can be created using the Standard SPC Chart tab, or the Small Multiples SPC Chart tab. The Standard tab will produce a single SPC chart, for a chosen metric from the dataset CSV provided. The Small Multiples tab will produce a small multiples SPC chart for all metrics contained within the dataset CSV.")
    
    output$intro4 <- renderText("Charts are designed and customised using the following options:")
    
    output$intro4a <- renderText("Choose CSV File - select a CSV file containing data (see guidance below);")
    
    output$intro4b <- renderText("Select Dataset - for a Standard SPC, select which metric from your CSV file to chart;")
    
    output$intro4c <- renderText("Improvement Direction - specify whether an increase or decrease in the measured metric is an improvement in performance;")
    
    output$intro4d <- renderText("Target (optional) - specify a fixed target for the measured metric, which will be represented as a horizontal line on the chart (for percentage targets, enter in decimal format);")
    
    output$intro4e <- renderText("Rebase Dates (optional) - for longer time series, you may enter dates in the format dd/mm/yyyy. Multiple dates should be separated with by ';'. These dates will then be used to rebase the control limits for periods between dates;")
    
    output$intro4f <- renderText("Data in Percentage Format - if selected, series values will be represented as percentages;")
    
    output$intro4g <- renderText("Customise Titles - if selected, chart, x axis and y axis titles can be customised;")
    
    output$intro4h <- renderText("Date Method - choose whether to manually specify dates for x axis, or whether to inherit from a 'Date' column in the dataset CSV (if a 'Date' column is present, this will be used by default);")
    
    output$intro4i <- renderText("Plotting Point Size - vary the point size and line widths used on the chart;")
    
    output$intro4j <- renderText("X Axis Tick Frequency - choose at what intervals on the x axis to display values;")
    
    output$intro4k <- renderText("Plot Area Height - vary the height (in pixels) of the chart.")
    
    
    #### Single SPC Sheet ----
    
    observeEvent(input$file1
                 ,{
                     defaultRadio <- if ("date" %in% tolower(names(read.csv(input$file1$datapath)))) "Inherit from CSV" else "Set Manually"
                     updateRadioButtons(session,"dateMethod",selected = defaultRadio)
                     
                     datasetNames <- names(read.csv(input$file1$datapath))
                     
                     remap <- c(1:length(datasetNames))[tolower(datasetNames)=="date"]
                     
                     datasetNames <- datasetNames[tolower(datasetNames) != "date"]
                     updateSelectInput(session,"dataset",choices = datasetNames[!(datasetNames %like% "_trajectory")])
                 }
    )
    
    timeline <- reactiveValues(df = data.frame(
            caption = character(0)
            ,date = integer(0)
        )
    )

    observeEvent(input$dataset,{
        
        req(input$file1,input$dataset)
        dataset <- read.csv(input$file1$datapath)
        
        datasetNames <- names(read.csv(input$file1$datapath))
        
        remap <- c(1:length(datasetNames))[tolower(datasetNames)=="date"]
        dataset[,remap] <- as.Date(dataset[,remap],"%d/%m/%Y")
        
        names(dataset)[remap] <- "Date"
        
        x <- dataset[,input$dataset]
        if(x[1] %like% "%"){x <- as.numeric(sub("%", "", x))/100
        }
        xl <- length(x)
        xn <- length(x) - 1
        startdate <- input$xdate
        dateinterval <- input$inttype
        dateintervaltype <- input$int
        xaxis <- if(input$dateMethod == "Set Manually") {as.character(seq(from = startdate,by=paste(dateintervaltype,tolower(dateinterval),sep=" "),length.out = xl)) 
        } else if (input$dateMethod == "Inherit from CSV" && !is.null(dataset$Date)){
            as.Date(dataset$Date,"%d/%m/%Y")
        } else {
            as.Date(rep(NA,xl))
        }
        
        df <- data.frame(
            caption = rep(NA,length(xaxis))
            ,date = xaxis
        )

        timeline$df <- rbind(timeline$df,df)
        
    })
    
    observeEvent(input$interventions,{
            showModal(
                modalDialog(
                    renderUI({
                          textInput("newcap","Intervention Caption")
                        })
                    ,renderUI({
                        dateInput("newcapdate","Intervention Date (dd/mm/yyyy)")
                    })
                        ,renderUI({actionButton("changedata","Add Intervention")
                        })
                    ,size = "s"
                    ,easyClose = TRUE
                )
            )
        })
    
    output$interventionsummary <- renderDataTable({
        datatable(timeline$df)
    })
    
    observeEvent(input$changedata,{
        d <- max(timeline$df$date[timeline$df$date <= input$newcapdate])
        timeline$df$caption[timeline$df$date == d] <- input$newcap
    })
    
    output$plot1 <- renderPlot({
        
        req(input$file1,input$dataset)
        
        dataset <- read.csv(input$file1$datapath)
        datasetNames <- names(read.csv(input$file1$datapath))

        remap <- c(1:length(datasetNames))[tolower(datasetNames)=="date"]
        names(dataset)[remap] <- "Date"
        
        x <- dataset[,input$dataset]
        if(x[1] %like% "%"){x <- as.numeric(sub("%", "", x))/100
        updateCheckboxInput(session = session, inputId = "dataaspercentages",value = TRUE)
        }
        xl <- length(x)
        xn <- length(x) - 1
        plottitle <- input$title
        xlabel <- input$xtitle
        ylabel <- input$ytitle
        startdate <- input$xdate
        dateinterval <- input$inttype
        dateintervaltype <- input$int
        xaxis <- if(input$dateMethod == "Set Manually") {as.character(seq(from = startdate,by=paste(dateintervaltype,tolower(dateinterval),sep=" "),length.out = xl)) 
        } else if (input$dateMethod == "Inherit from CSV" && !is.null(dataset$Date)){
            as.Date(dataset$Date,"%d/%m/%Y")
        } else {
            as.Date(rep(NA,xl))
        }
        
        trajectory <- rep(NA,xl)
        if(paste0(input$dataset,"_trajectory") %in% names(dataset)){
            trajectory <- dataset[,paste0(input$dataset,"_trajectory")]
        }
        
        rebase <- if(input$rebase == ""){
            rebase <- FALSE
        } else {
            rebase <- input$rebase
        }
        
        updateSliderInput(session,"breaks",max=xn)
        
         labels <- timeline$df$caption 
         mask <- !(is.na(labels))
         arrows <- labels
         arrows[mask] <- x[mask]
         arrows <- as.numeric(arrows)
         
            print(labels)
            print(arrows)
         
         nudge_factor <- max(x) - min(x)
         nudge_factor1 <- nudge_factor/25
         nudge_factor2 <- nudge_factor/15
         
         just <- function(x) {
             (sign(x - mean(x)) + 1) / 2
         }
        
        spc(
            data = x
            ,dates = as.Date(xaxis,"%Y-%m-%d")
            ,improvement.direction = input$improv
            ,target = if(input$dataaspercentages == TRUE) {input$target/100} else {input$target}
            ,trajectory = trajectory
            ,rebase = rebase
            ,main.title = plottitle
            ,x.title = xlabel
            ,y.title = ylabel
            ,x.axis.breaks = input$breaks
            ,point.size = input$pointsize
            ,data.as.percentages = input$dataaspercentages
            ,display.legend = TRUE
        ) +
           geom_point(aes(y = arrows),position = position_nudge(y = -1 * nudge_factor1), pch = 24, size = 4, color = "#39db6a", fill = "#39db6a") +
           geom_text(aes(label = labels),position = position_nudge(y = -1 * nudge_factor2), hjust = 1, angle = 90, size = 5)
                       
    }
    )
    
    #### Small Multiple SPC Sheet ----
    
    observeEvent(input$file2
                 ,{
                     defaultRadio <- if ("date" %in% tolower(names(read.csv(input$file2$datapath)))) "Inherit from CSV" else "Set Manually"
                     updateRadioButtons(session,"dateMethod",selected = defaultRadio)
                }
    )

    output$plot2 <- renderPlot({
        
        req(input$file2)
        
        dataset2 <- read.csv(input$file2$datapath)
  
        datasetNames <- names(dataset2)
        remap <- c(1:length(datasetNames))[tolower(datasetNames)=="date"]
        names(dataset2)[remap] <- "Date"
        
        plotnames <- names(dataset2)
        plotnames <- plotnames[!(plotnames %like% "_trajectory" | tolower(plotnames) == "date")]
        nplot <- length(plotnames)
        
        makeplot <- function(plotnameN){
        x <- dataset2[,plotnameN]
        xl <- length(x)
        xn <- length(x) - 1
        plottitle <- plotnameN
        xlabel <- input$xtitle2
        ylabel <- input$ytitle2
        startdate <- input$xdate2
        dateinterval <- input$inttype2
        dateintervaltype <- input$int2
        xaxis <- if(input$dateMethod2 == "Set Manually") {as.character(seq(from = startdate,by=paste(dateintervaltype,tolower(dateinterval),sep=" "),length.out = xl)) 
        } else if (input$dateMethod2 == "Inherit from CSV" && !is.null(dataset2$Date)){
            as.Date(dataset2$Date,"%d/%m/%Y")
        } else {
            as.Date(rep(NA,xl))
        }
        
        trajectory <- rep(NA,xl)
        if(paste0(plotnameN,"_trajectory") %in% names(dataset2)){
            trajectory <- dataset2[,paste0(plotnameN,"_trajectory")]
        }
        
        rebase <- if(input$rebase2 == ""){
            rebase <- FALSE
        } else {
            rebase <- input$rebase2
        }
        
        updateSliderInput(session,"breaks2",max=xn)
        
        spc(
            data = x
            ,dates = xaxis
            ,improvement.direction = input$improv2
            ,target = input$target2
            ,trajectory = trajectory
            ,rebase = rebase
            ,main.title = plottitle
            ,x.title = xlabel
            ,y.title = ylabel
            ,x.axis.breaks = input$breaks2
            ,point.size = input$pointsize2
            ,data.as.percentages = input$dataaspercentages2
            ,display.legend = TRUE
        ) 
        }
        
        plotlist <- as.list(lapply(plotnames,FUN = makeplot))
        gridExtra::grid.arrange(grobs=plotlist,ncol = grobsizes[nplot,2],nrow = grobsizes[nplot,1])
    }
    )

    output$plot2UI <- renderUI({
        plotOutput("plot2",height = paste0(input$plotHeight2,"px"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
