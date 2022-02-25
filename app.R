# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(ggplot2)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(magrittr)
library(scales)
library(shinydashboard)
library(data.table)
library(DT)
library(ggthemes)
library(cowplot)


theme_set(theme_bw())

zone_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4")

dat_l <- read.csv("./Data/DatMat_LL.csv", header = TRUE)
dat_l_00 <- read.csv("./Data/DatMat_LL_00.csv", header = TRUE)

#dat_t <- read.csv("./Data/DatMat_LLTR.csv", header = TRUE)
#dat_t_00 <- read.csv("./Data/DatMat_LLTR_00.csv", header = TRUE)

dat_recent <- read.csv("./Data/Catch_History_Calcs/Catch_Summaries_RecentAvg.csv", header = TRUE)



#____________________________________________________________________________________________________________
# User interface

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    titlePanel(title = div(img(height = 120, width = 1500, src = "HeaderBar1.png")), windowTitle = "MAGA - Make Albacore Great Again"),
    
    setBackgroundColor(
        color = c("white", "white"),
        gradient = "linear",
        direction = c("bottom","left")
    ),
    
    fluidRow(
        column(2,
               
               br(),
               h3("Choose your scenario"),
               h5(" Tick scenario/s to include and shift slider"),
               h5("to modfify the weightings (%'s). Note that"),
               h5("values shown are those suggested in paper"),
               br(),
               
               
               #__________________
               
               checkboxInput("Scen1", "Avg. highest 3 catch 2010-19", TRUE),
               
               conditionalPanel(condition = "input.Scen1 == true",
                                sliderInput("slider1", NULL,  min = 0, max = 100, value = 26, step  =  1, width = "170px", ticks = TRUE)),
               
               #__________________
               
               checkboxInput("Scen2", "Avg. highest 5 catch 200X-19", TRUE),
               
               conditionalPanel(condition = "input.Scen2 == true",
                                sliderInput("slider2", NULL,  min = 0, max = 100, value = 16, step  =  1, width = "170px", ticks = TRUE)),
               
               #__________________
               
               checkboxInput("Scen3", "Interim TKA limits", TRUE),
               
               conditionalPanel(condition = "input.Scen3 == true",
                                sliderInput("slider3", NULL,  min = 0, max = 100, value = 14, step = 1, width = "170px", ticks = TRUE)),
               
               #__________________
               
               checkboxInput("Scen4", "Estimated biomass in EEZ", TRUE),
               
               conditionalPanel(condition = "input.Scen4 == true",
                                sliderInput("slider4", NULL,  min = 0, max = 100, value = 18, step = 1, width = "170px", ticks = TRUE)),
               
               #__________________
               
               checkboxInput("Scen5", "Index of fisheries dependence", TRUE),
               
               conditionalPanel(condition = "input.Scen5 == true",
                                sliderInput("slider5", NULL,  min = 0, max = 100, value = 26, step = 1, width = "170px", ticks = TRUE))
               
               #__________________          

        ),
        
        column(1, 
               br(),br(),br(),br(),
               h3("Weighting (%'s)"),
               br(),br(),
               h2(htmlOutput("slide1out")),
               br(),br(),
               h2(htmlOutput("slide2out")),
               br(),br(),
               h2(htmlOutput("slide3out")),
               br(),br(),
               h2(htmlOutput("slide4out")),
               br(),br(),
               h2(htmlOutput("slide5out")),
               br()
               
        ),
        
        
        column(2, #style = "background-color:#4d3a2d;",
                
        br(),
        h3("Set your options"),
        h5("Choose a total FFA bloc TAC, either a"),
        h5("reference period or a custom value (mt),"),
        h5("and select whether to include troll catch"),
        br(),
        
        selectInput("variable", h4("Total bloc:"),
                    c("Avg FFA total 2020" = "av20",
                      "Avg FFA total 2019" = "av19",
                      "Custom" = "custm"), width = "200px"),
        
        conditionalPanel(condition = "input.variable == 'custm'",
                         numericInput("num1", h4("FFA bloc TAC"), value = 30000, step = 1000, width = "180px")),
        
        
        radioButtons("timeswitch", label = h4("Average of best years from:"),
                     choices = list("Early period (e.g. 2001-15)" = 1, "Recent period (e.g. 2005-19)" = 2), 
                     selected = 2),
        
        br(),
        h3(htmlOutput("TACtext")),
        br()
          
        ),
        
        column(4,  plotlyOutput("IndPlot")),
        column(2, br(), br(), dataTableOutput("AllocTab"))
        
    ),
    
    fluidRow(
      column(1, br()),
      column(4, br(), plotOutput("Barplot")),
      #column(1, div(style = "height:100px")),
      column(5, br(), plotOutput("Alloplot"), offset = 2, style='padding-bottom:100px')
    ),
        
)


#____________________________________________________________________________________________________________
# The server

server <- function(input, output) {
    
  
    wgtVec <- reactive({
        paste(c(ifelse(input$Scen1 == TRUE, input$slider1, 0), ifelse(input$Scen2 == TRUE, input$slider2, 0), ifelse(input$Scen3 == TRUE, input$slider3, 0),
                ifelse(input$Scen4 == TRUE, input$slider4, 0), ifelse(input$Scen5 == TRUE, input$slider5, 0)))
    })
    
    output$slide1out <- renderText({ 
      
      tmp.wgt <- as.numeric(wgtVec())
      tmp.wgt <- tmp.wgt/sum(tmp.wgt)
      
      paste(round(tmp.wgt[1]*100,1), "%")
    })
    
    output$slide2out <- renderText({ 
      
      tmp.wgt <- as.numeric(wgtVec())
      tmp.wgt <- tmp.wgt/sum(tmp.wgt)
      
      paste(round(tmp.wgt[2]*100,1), "%")
    })
    
    output$slide3out <- renderText({ 
      
      tmp.wgt <- as.numeric(wgtVec())
      tmp.wgt <- tmp.wgt/sum(tmp.wgt)
      
      paste(round(tmp.wgt[3]*100,1), "%")
    })

    
    output$slide4out <- renderText({ 
      
      tmp.wgt <- as.numeric(wgtVec())
      tmp.wgt <- tmp.wgt/sum(tmp.wgt)
      
      paste(round(tmp.wgt[4]*100,1), "%")
    })
    
    output$slide5out <- renderText({ 
      
      tmp.wgt <- as.numeric(wgtVec())
      tmp.wgt <- tmp.wgt/sum(tmp.wgt)
      
      paste(round(tmp.wgt[5]*100,1), "%")
    })

    
    cattab <- reactive({
      
      
      if(input$timeswitch == "1"){

          dat <- dat_l_00
      
      } else {
          
          dat <- dat_l
      }
      
      
      wgt.scalars <- as.numeric(wgtVec())
      
      tmp <- as.matrix(dat[,-c(1,2)])
      tmp.p <- t(t(tmp)/apply(tmp,2,sum))   # This probably nicer apply(tmp[-c(1:2)],2,function(x){x/sum(x)})
      tmp.p.w <- t(t(tmp.p)*(wgt.scalars/sum(wgt.scalars)))
      
      tmp.tab <- apply(tmp.p.w, 1, sum)
    
    })
    
    
    pertab <- reactive({
      
      
      if(input$timeswitch == "1"){
        
        dat <- dat_l_00
        
      } else {
        
        dat <- dat_l
      }
      
      
      wgt.scalars <- as.numeric(wgtVec())
      
      tmp <- as.matrix(dat[,-c(1,2)])
      tmp.p <- t(t(tmp)/apply(tmp,2,sum))   # This probably nicer apply(tmp[-c(1:2)],2,function(x){x/sum(x)})
      tmp.p.w <- t(t(tmp.p)*(wgt.scalars/sum(wgt.scalars)))
      
      tab <- as.data.frame(tmp.p.w)
      tab$Cnt <- dat$Cnt
      
      all_tab <- tab %>% pivot_longer(cols = -Cnt, names_to = "Criteria", values_to = "Allocation")
      
    })
    
    
    TACset <- reactive({
      
      if(input$variable == 'custm'){
        
        tac = input$num1
        
      }  else {

        if(input$variable == 'av20'){
          
            tac = 23584
          
        } else {
           
            tac = 28118
            
        }
      }

    })
    
    
    output$TACtext <- renderText({ 
      paste("FFA bloc TAC: <b>", formatC(TACset(), format = "d", big.mark = ','))
    })
    
    
    dattab <- reactive({

        test <- data.frame(Zone = dat_l$Cntnm, Percent = round(cattab()*100, 1), Allocation = round(cattab()*TACset()), "Catch_18_20" = round(dat_recent[,2]), TAC = TACset())

    })
    
    
    output$AllocTab <- renderDataTable({
      
      tactab <- dattab() %>% select(-TAC) %>% arrange(desc(Percent))
      
      tactab <- datatable(tactab, options = list(pageLength = 15, searching = FALSE, dom = "t")) %>% formatStyle(columns = c(2:3), 'text-align' = 'center')
                                       
    })


    output$IndPlot <- renderPlotly({

        plot_ly(dattab(), labels = ~Zone, values = ~Percent, type = "pie", marker = list(colors = zone_cols), sort = FALSE,
                textposition = "inside", textinfo = "label+value", width = 620, height = 620) %>% config(displayModeBar = F) %>%
            layout(title = list(text = "Zone - Allocations (Percentages)", y = 0.99), plot_bgcolor = 'transparent', paper_bgcolor = 'transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), showlegend = FALSE)


    })
    
  
    output$Barplot <- renderPlot({
      
      dat <- dattab()
      
      dat %<>% select(Zone, Allocation)
      
      dat$xref <- as.factor("Proposed")
      
      dat$Zone <- rep("FFA", dim(dat)[1])
      
      dat1 <- data.frame(xref = as.factor("2019"), Zone = c("FFA","HS","Troll - NZ","Troll - Oth","Oth EEZ's"), Allocation = c(28118,31355,1907,873,6510))
      
      dat2 <- data.frame(xref = as.factor("2020"), Zone = c("FFA","HS","Troll - NZ","Troll - Oth","Oth EEZ's"), Allocation = c(23584,28199,2859,1913,5221))
      
      dat <- rbind(dat2, dat1, dat)
      dat$Zone <- factor(dat$Zone, levels = rev(c("FFA","Troll - NZ","Troll - Oth","Oth EEZ's","HS")))
      
      ggplot(dat, aes(x = xref, y = Allocation, fill = Zone)) + geom_hline(yintercept = 40500, colour = alpha("grey", 0.7), size = 1) +
        geom_hline(yintercept = 46700, colour = alpha("steelblue", 0.7), size = 1) +
        geom_bar(stat = "identity", position = "stack", width = 0.7) +
        ylab("Catch (mt)") + coord_flip() +
        scale_y_continuous(expand = c(0, 0), labels = comma) + scale_fill_manual(values = alpha(c("black","grey","steelblue","palevioletred3","firebrick1"), 0.8)) +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 18), axis.title.y = element_blank(),
              legend.position = "right", legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(0.8, "cm"),
              legend.spacing.x = unit(0.2, "cm"), panel.border = element_blank(), axis.line.x = element_line(size = 0.5, colour = "black"),
              panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
      
    }, height = 160, width = 900)
    
    
    output$Alloplot <- renderPlot({

      dat <- pertab()

      pl1 <- ggplot(dat, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity") +
                    scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                    theme_cowplot() + theme(legend.position = "top", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank())
      
      
      pl2 <- ggplot(dat, aes(x = Cnt, y = Allocation, fill = Criteria)) + geom_bar(stat = "identity", position = "fill") +
                    scale_fill_manual(values = alpha(c("firebrick","burlywood3","dodgerblue","navy","darkorchid"), 0.8)) +
                    theme_cowplot() + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                            axis.title.x = element_blank())

      
      plot_grid(pl1, pl2, labels = "", ncol = 1)

    }, height = 550, width = 700)
    
    
}


shinyApp(ui = ui, server = server)
