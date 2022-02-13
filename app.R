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

#setwd("C:/GitHub/WHATapp/WHATapp_web")

theme_set(theme_bw())

zone_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4")

dat_l <- read.csv("./Data/DatMat_LL.csv", header = TRUE)

dat_t <- read.csv("./Data/DatMat_LLTR.csv", header = TRUE)


#____________________________________________________________________________________________________________
# User interface

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    titlePanel(title = div(img(height = 120, width = 1500, src = "HeaderBar2.png")), windowTitle = "Let's make albacore great again"),
    
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
               
               checkboxInput("Scen2", "Avg. highest 5 catch 2005-19", TRUE),
               
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
        
        
        column(2, #style = "background-color:#4d3a2d;",
                
        br(),
        h3("Set your options"),
        h5("Choose a total FFA bloc TAC, either a"),
        h5("reference period or a custom value (mt),"),
        h5("and select whether to include troll catch"),
        br(),
        #br(),
        
        selectInput("variable", h4("Total bloc:"), # Put the choice between PS and LL here
                    c("Avg FFA total 2020" = "av20",
                      "Avg FFA total 2019" = "av19",
                      #"Avg FFA total 2016-20" = "av5Y",
                      "Custom" = "custm"), width = "200px"),
        
        conditionalPanel(condition = "input.variable == 'custm'",
                         numericInput("num1", h4("FFA bloc TAC"), value = 30000, step = 1000, width = "180px")),
        
        #numericInput("num1", h4("Allocation TAC"), value = 10000, width = "180px"),
        
        radioButtons("datswitch", label = h4("Gears to include:"),
                     choices = list("Just longline" = 1, "Longline + troll" = 2), 
                     selected = 1),
        
        br(),
        h3(htmlOutput("TACtext")),
        br()
          
        ),
        
        column(5,  plotlyOutput("IndPlot")),
        #column(1, br()),
        column(2, br(), br(), dataTableOutput("AllocTab"))
        
    ),
    
    fluidRow(
      column(2, br()),
      column(8,  plotOutput("Barplot"))
    )
        
)


#____________________________________________________________________________________________________________
# The server

server <- function(input, output) {
    
    wgtVec <- reactive({
        paste(c(ifelse(input$Scen1 == TRUE, input$slider1, 0), ifelse(input$Scen2 == TRUE, input$slider2, 0), ifelse(input$Scen3 == TRUE, input$slider3, 0),
                ifelse(input$Scen4 == TRUE, input$slider4, 0), ifelse(input$Scen5 == TRUE, input$slider5, 0)))
    })

    
    datuse <- reactive({

        if(input$datswitch == 1){

            dat <- dat_l

        } else {

            dat <- dat_t

        }
    })

    
    cattab <- reactive({

      if(input$datswitch == "1"){

        dat <- dat_l

      } else {

        dat <- dat_t

      }
      
      wgt.scalars <- as.numeric(wgtVec())
      
      tmp <- as.matrix(dat[,-c(1,2)])
      tmp.p <- t(t(tmp)/apply(tmp,2,sum))
      tmp.p.w <- t(t(tmp.p)*(wgt.scalars/sum(wgt.scalars)))
      
      tmp.tab <- apply(tmp.p.w, 1, sum)
    
    })
    
    
    TACset <- reactive({
      
      if(input$variable == 'custm'){
        
        tac = input$num1
        
      }  else {

        if(input$variable == 'av20'){
          
          
          if(input$datswitch == "1"){
            
            tac = 23584
            
          } else {
            
            tac = 26443
            
          }
          
        } else {
           
          
          if(input$datswitch == "1"){
            
            tac = 28118
            
          } else {
            
            tac = 30025
            
          }
          
          
         }
      }

    })
    
    
    output$TACtext <- renderText({ 
      paste("FFA bloc TAC: <b>", formatC(TACset(), format = "d", big.mark = ','))
    })
    
    
    dattab <- reactive({

        test <- data.frame(Zone = dat_l$Cntnm, Percent = round(cattab()*100, 1), Allocation = round(cattab()*TACset()), TAC = TACset()) #%>% arrange(desc(Percent))

    })
    
    
    output$AllocTab <- renderDataTable({
      
      tactab <- dattab() %>% select(-TAC) %>% arrange(desc(Percent))
      
      tactab <- datatable(tactab, options = list(pageLength = 15, searching = FALSE, dom = "t")) %>% formatStyle(columns = c(2:3), 'text-align' = 'center')
      
      #,                                       options=list(pageLength = 15, searching = FALSE, dom = "ltp")
                                       
    })


    output$IndPlot <- renderPlotly({

        plot_ly(dattab(), labels = ~Zone, values = ~Percent, type = "pie", marker = list(colors = zone_cols), sort = FALSE,
                textposition = "inside", textinfo = "label+value", width = 650, height = 650) %>% config(displayModeBar = F) %>%
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
      
    }, height = 200, width = 1200)
    
    
}


shinyApp(ui = ui, server = server)