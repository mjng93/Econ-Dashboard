

#source("econ_data.R")

library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(quantmod)
library(rmarkdown)
# library(TTR)
# library(gridExtra)
# library(grid)
library(DT)
#library(kableExtra)
#library(tables)
#library(knitr)
library(rsconnect)
library(plotly)
#library(ggthemes)
#library(stargazer)
library(data.table)

sandbox2.UI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Interactive",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("Financial Data"),
                                 radioButtons(ns("chart_type"),
                                              label = "Select Chart Type", 
                                              choices = c("Bar Chart" = "bar","Line Chart"="line"),
                                              selected = "line"),

                                 selectInput(ns("ystat"),
                                             label = "Select statistic:",
                                             choices = list("Stock Prices"=c(colnames(stocks_all_w)[-1]),
                                                            "Crypto" = c(colnames(crypto_all_w[,-1])),
                                                            "Treasuries" = c(colnames(t_yields[,-1]))),
                                                                    
                                                            
                                             selected = c("AMZN","Bitcoin"),
                                             multiple = TRUE
                                 ),
                                 radioButtons(ns("radio"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels","Change"="diff","Percent Change"="single.pct","Percent Change YoY" = "yoy","Z-score"="z","Date Index"="index","Annualized Growth"="annual","Rolling Average YoY Changes (12-month)"="ra.yoy","YTD"="ytd"), #"Percent Change (10-day)" = "mom"
                                              selected = "levels"),
                                 
                                 dateRangeInput(ns('dateRange'),
                                                label = 'Select Date Range',
                                                start = "2017-01-01", end = max(stocks_all_w$date,na.rm=T)
                                 ),
                             
                                 
                                 downloadButton(ns("FinData"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot"))),
                                  tabPanel("Summary", tableOutput(ns("summary"))),
                                  tabPanel("Correlations", tableOutput(ns("corr"))),
                                 
                                  tabPanel("Table", tableOutput(ns("table")))),
                      
                      
                      p("Data is sourced from a variety of sources, particularly",
                        span(a("FRED",target="_blank",href="https://fred.stlouisfed.org/")),
                        ". Data is currently updated through",
                        span(as.character(format(as.Date(max(fred$date,na.rm=T)),"%B %d, %Y"))),". All code written by Michael Ng, available on",
                        span(a("Github",target="_blank",href="https://github.com/mjng93/Econ-Dashboard")),"."
                      )
                      
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
  )
  
  
  
}




sandbox.server2 <- function(input, output, session,data){
  

  
  module_data=data
  module_data$date=as.Date(module_data$date)

  data_input <- reactive({
    if (input$radio=="levels"){
     module_data.levels=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      df <- melt(module_data.levels,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Levels"
    }
    
    if (input$radio=="yoy"){
     
      module_data.yoy=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      module_data.yoy=module_data.yoy[,c("date",input$ystat)]
      module_data.yoy=na.omit(module_data.yoy)
      
      module_data.freq=module_data[module_data$date>="2020-01-01" & module_data$date<="2020-12-31",]
      module_data.freq=module_data.freq[,c("date",input$ystat)]
      module_data.freq=na.omit(module_data.freq)
      freq=length(unique(module_data.freq$date))
      
      for (i in 2:ncol(module_data.yoy)){
        module_data.yoy[,i]=as.numeric(as.vector(Delt(module_data.yoy[,i],k=freq)))*100
      }
      df <- melt(module_data.yoy,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Percent"
    }
    
    if (input$radio=="ra.yoy"){
      
      module_data.ra.yoy=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      module_data.ra.yoy=module_data.ra.yoy[,c("date",input$ystat)]
      module_data.ra.yoy=na.omit(module_data.ra.yoy)
      
      module_data.freq=module_data[module_data$date>="2020-01-01" & module_data$date<="2020-12-31",]
      module_data.freq=module_data.freq[,c("date",input$ystat)]
      module_data.freq=na.omit(module_data.freq)
      freq=length(unique(module_data.freq$date))
      
      for (i in 2:ncol(module_data.ra.yoy)){
        module_data.ra.yoy[,i]=rollapply(as.numeric(as.vector(Delt(module_data.ra.yoy[,i],k=freq)))*100,width=freq,FUN=function(x) mean(x,na.rm=T),fill=NA,by.column=FALSE,align="right")
      }
      df <- melt(module_data.ra.yoy,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Percent"
    }
    
    if (input$radio=="single.pct"){
      module_data.single.pct=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      module_data.single.pct=module_data.single.pct[,c("date",input$ystat)]
      module_data.single.pct=na.omit(module_data.single.pct)
      for (i in 2:ncol(module_data.single.pct)){
        module_data.single.pct[,i]=as.numeric(as.vector(Delt(module_data.single.pct[,i],k=1)))*100
      }
      df <- melt(module_data.single.pct,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Percent"
    }
    
    if (input$radio=="diff"){
      module_data.diff=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      module_data.diff=module_data.diff[,c("date",input$ystat)]
      module_data.diff=na.omit(module_data.diff)
      for (i in 2:ncol(module_data.diff)){
        module_data.diff[,i]=c(NA,(diff(module_data.diff[,i],lag=1)))
      }
      df <- melt(module_data.diff,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Percent"
    }
    
    if (input$radio=="z"){
      module_data.z=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      for (i in 2:ncol(module_data.z)){
        module_data.z[,i]=(module_data.z[,i]-mean(module_data.z[,i],na.rm=T))/sd(module_data.z[,i],na.rm=T)
      }
      df <- melt(module_data.z,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      units="Standard Deviation from the Mean"
    }
    
    if (input$radio=="index"){
      module_data.index=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      
      min = min(module_data.index$date,na.rm=T)
      
      for (i in 2:ncol(module_data.index)){
        module_data.index[,i]=(module_data.index[,i])/module_data.index[module_data.index$date==min,i]
      }
      df <- melt(module_data.index,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      units="Index"
    }
    
    if (input$radio=="ytd"){
      module_data.index=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      
      #min = min(module_data.index$date,na.rm=T)
      
      for (i in 2:ncol(module_data.index)){
        module_data.index[,i]=(module_data.index[,i])/module_data.index[module_data.index$date=="2021-01-02",i]
      }
      df <- melt(module_data.index,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      units="Index"
    }
    
    if (input$radio=="annual"){
      module_data.annual=module_data[module_data$date>=input$dateRange[1] & module_data$date<=input$dateRange[2],]
      module_data.annual=module_data.annual[,c("date",input$ystat)]
      module_data.annual=na.omit(module_data.annual)
      
      module_data.freq=module_data[module_data$date>="2020-01-01" & module_data$date<="2020-12-31",]
      module_data.freq=module_data.freq[,c("date",input$ystat)]
      module_data.freq=na.omit(module_data.freq)
      freq=length(unique(module_data.freq$date))
      
      for (i in 2:ncol(module_data.annual)){
        module_data.annual[,i]=(((module_data.annual[,i]/shift(module_data.annual[,i],n=1))^(1/(1/freq)))-1)*100
      }
      df <- melt(module_data.annual,id.vars='date')
      df <- df[df$variable %in% input$ystat,]
      df=na.omit(df)
      units="Percent"
    }
    
    df
    
  })
  
  unit_input <- reactive({
    if (input$radio=="levels"){units="Level"}
    if (input$radio=="yoy"){units="Percent"}
    if (input$radio=="z"){units="Z-Score (Difference from Mean/SD)"}
    if (input$radio=="index"){units="Index (Initial Date==1)"}
    if (input$radio=="diff"){units="New Cases"}
    if (input$radio=="qoq"){units="Percent Change (1=1%)"}
    if (input$radio=="annual"){units="Percent"}
    if (input$radio=="ra.yoy"){units="Percent"}
    if (input$radio=="single.pct"){units="Percent"}
    
    units
    
  })
  
  output$plot <- renderPlotly({
    
    plot.data=data_input()
    print(tail(plot.data))
    plot.data=na.omit(plot.data)
    
    if (input$chart_type=="line"){
      plot_ly(plot.data, x = ~date,y= ~value, color = ~variable, type = 'scatter', mode = 'lines', colors = rainbow(length(unique(plot.data$variable)))) %>%
        layout(title = "Financial Data",
               titlefont = list(color="white"),
               legend = list(font = list(
                 color='white')
               ),
               xaxis = list(title = "Date",color='white'),
               yaxis = list (title = unit_input(),color='white'),
               paper_bgcolor='black',
               plot_bgcolor = 'black'
               )
    }
    
    else if (input$chart_type=="bar"){
      plot_ly(plot.data, x = ~date,y= ~value, color = ~variable, type = 'bar', colors = rainbow(length(unique(plot.data$variable)))) %>%
        layout(title = "Financial Data",
               titlefont = list(color="white"),
               legend = list(font = list(
                    color='white')
                    ),
               xaxis = list(title = "Date",color='white'),
               yaxis = list (title = unit_input(),color='white'),
               paper_bgcolor='black',
               plot_bgcolor = 'black'
        )
    }
    
    #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
    
    #print(df.ggplot)
    
    
  }#,height=400,width=1000
  )
  
  output$summary <- renderTable({
    
    df.sum=data_input()
    #df.sum=na.omit(df.sum)
    
    summary=data.frame(series=numeric(length(input$ystat)),l1=numeric(length(input$ystat)),l2=numeric(length(input$ystat)),l3=numeric(length(input$ystat)),min=numeric(length(input$ystat)),max=numeric(length(input$ystat)),sd=numeric(length(input$ystat)))
    
    for (i in 1:length(input$ystat)){
      summary[i,'series']=input$ystat[i]
      summary[i,'l1']=subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i])),"value"]
      summary[i,'l2']=subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i]))-1,"value"]
      summary[i,'l3']=subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i]))-2,"value"]
      summary[i,'min']=min(subset(df.sum,variable==input$ystat[i])[,"value"],na.rm=T)
      summary[i,'max']=max(subset(df.sum,variable==input$ystat[i])[,"value"],na.rm=T)
      summary[i,'sd']=sd(subset(df.sum,variable==input$ystat[i])[,"value"],na.rm=T)
      
    }

      colnames(summary)=c("Series",as.character(as.Date(subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i])),"date"])),as.character(as.Date(subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i]))-1,"date"])),as.character(as.Date(subset(df.sum,variable==input$ystat[i])[nrow(subset(df.sum,variable==input$ystat[i]))-2,"date"])),"Min","Max","Stdev.")
    
    summary
    
  },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$corr <- renderTable({
    
    df.corr=data_input()
    df.corr=dcast(df.corr,date~variable,value.var="value",mean)
    df.corr1=cor(na.omit(as.matrix(df.corr[,-c(1)])))
    rownames(df.corr1)=colnames(df.corr)[-c(1)]
    df.corr1
    
  },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
  
  output$table <- renderTable({
    df1=data_input()
    #df1$GameID=as.character(df1$GameID)
    df1=dcast(df1,date~variable,value.var='value',mean)
    df1$date=as.character(df1$date)
    df1
  })
  
  
  output$EconData <- downloadHandler(
    filename = function() {
      paste('financial_data', 'csv', sep='.')
    },
    content = function(file) {
      
      write.csv(data_input(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
}