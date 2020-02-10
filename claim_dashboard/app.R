#options(shiny.maxRequestSize=200*1024^2)

# global -------------------------------------------------------------------
#options(width=50)
knitr::opts_chunk$set(cache=TRUE,tidy=TRUE)
options(scipen = 9999)
rm(list=ls())

library(RODBC)
library(readr)
library(lubridate)
library(formattable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(plotly)
library(scales)
library(shinythemes)

# dataset ------------------------------------------------------------------
#local_conn = odbcConnect("local")
#claim_database <- sqlQuery(local_conn,"SELECT * from lbb_algoritma.dbo.view_claim")

#write.csv(claim_database, "data/claim.csv", sep = "|", row.names = FALSE, quote = FALSE)

claim <- read_csv("data/claim.csv", quote = "|")

# Preparing additional value -----------------------------------------------
claim$Y_register_date <- year(claim$register_date)
claim$Q_register_date <- quarter(claim$register_date)
claim$M_register_date <- month(claim$register_date, label = T, abbr = T)
claim$m_register_date <- month(claim$register_date)
claim$YYYY_mm_register_date <- format(as.Date(claim$register_date, "%Y-%m-%d"), "%Y-%m")
claim$MM_YYYY_register_date <- paste(claim$M_register_date, claim$Y_register_date, sep="-")
claim$YQ_register_date <- quarter(claim$register_date, with_year = TRUE)

# Define UI for application that draws a histogram
header <- dashboardHeader(
    title = span(img(src = "claim.svg", height = 35), "Claim Insurance"),
    titleWidth = 300
)

sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
        menuItem("Year on Year", tabName = "yearonyears", icon = icon("signal")),
        menuItem("Claim Profile", tabName = "claimprofiles", icon = icon("cog")),
        menuItem("Spreading", tabName = "spreading", icon = icon("globe")),
        menuItem("SLA Performance", tabName = "slaperformances", icon = icon("align-center")),
        menuItem("What If Analysis", tabName = "whatifanalysis", icon = icon("question"))
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(
            rel = "stylesheet", 
            type = "text/css", 
            href = "claim.css"),
        tags$style(
            HTML("
               #myScrollBox{
               overflow-y: scroll;
               overflow-x: scroll;
               height:260px;
               #}")
        )
    ),
    tabItems(
        tabItem(tabName = "yearonyears",
                height = 20,
                tabBox(
                    width = 9,
                    title = tagList(shiny::icon("signal"), "Year on Year"),
                    id = "tabset1",
                    tabPanel("Claim Number", 
                             fluidRow(
                                 valueBoxOutput(width = 5, "claimnumber1")
                             ),
                             plotlyOutput(
                                 outputId = "plot11"
                             )
                    ),
                    tabPanel("Claim Amount",
                             fluidRow(
                                 valueBoxOutput(width = 5, "claimamount1")
                             ),
                             plotlyOutput(
                                 outputId = "plot12"
                             ) 
                    )
                ),
                box(
                    width = 3,
                    title = "Data Filters",
                    dateRangeInput(inputId = "registerdate1", 
                                   label = "Register Date",
                                   start = min(claim$register_date), end = max(claim$register_date)),
                    pickerInput("claimtype1", "Claim Type", 
                                choices=c(unique(claim$claim_type)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$claim_type))),
                    pickerInput("category1", "Category", 
                                choices=c(unique(claim$category)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$category))),
                    pickerInput("region1", "Region", 
                                choices=c(unique(claim$region)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$region)))                    
                )            
        ),
        tabItem(tabName = "slaperformances",
                height = 20,
                tabBox(
                    width = 9,
                    title = tagList(shiny::icon("align-center"), "SLA Performance"),
                    id = "tabset2",
                    tabPanel("Current SLA is 7 Days",
                             fluidRow(
                                 valueBoxOutput(width = 2, "claimmeetsla2"),
                                 valueBoxOutput(width = 2, "claimbelowsla2"),
                                 valueBoxOutput(width = 3, "claimnumber2"),
                                 valueBoxOutput(width = 5, "claimamount2")
                             ),
                             plotlyOutput(
                                 outputId = "plot21"
                             )
                    )
                ),
                box(
                    width = 3,
                    title = "Data Filters",
                    dateRangeInput(inputId = "registerdate2", 
                                   label = "Register Date",
                                   #diset manual karena hanya ingin mengecek dari tahun 2018 s.d. terakhir
                                   start = "2018-07-01", end = max(claim$register_date)),
                    pickerInput("claimtype2", "Claim Type", 
                                choices=c(unique(claim$claim_type)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$claim_type))),
                    pickerInput("category2", "Category", 
                                choices=c(unique(claim$category)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$category))),
                    pickerInput("region2", "Region", 
                                choices=c(unique(claim$region)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$region)))                    
                )            
        ),
        tabItem(tabName = "whatifanalysis",
                height = 20,
                tabBox(
                    width = 9,
                    title = tagList(shiny::icon("question"), "What If Analysis"),
                    id = "tabset3",
                    tabPanel(textOutput("sla_x_value"),
                             fluidRow(
                                 valueBoxOutput(width = 2, "claimmeetsla3"),
                                 valueBoxOutput(width = 2, "claimbelowsla3"),
                                 valueBoxOutput(width = 3, "claimnumber3"),
                                 valueBoxOutput(width = 5, "claimamount3")
                             ),
                             plotlyOutput(
                                 outputId = "plot31"
                             )
                    )
                ),
                box(
                    width = 3,
                    title = "Data Filters",
                    dateRangeInput(inputId = "registerdate3", 
                                   label = "Register Date",
                                   #diset manual karena hanya ingin mengecek dari tahun 2018 s.d. terakhir
                                   start = "2018-07-01", end = max(claim$register_date)),
                    numericInput("sla_x", "Choose Some SLA Value", 7, min = 1, max = 100),
                    pickerInput("claimtype3", "Claim Type", 
                                choices=c(unique(claim$claim_type)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$claim_type))),
                    pickerInput("category3", "Category", 
                                choices=c(unique(claim$category)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$category))),
                    pickerInput("region3", "Region", 
                                choices=c(unique(claim$region)), 
                                options = list(`actions-box` = TRUE), 
                                multiple = T, 
                                selected = c(unique(claim$region)))                    
                )            
        )
    )
)

# Put them together into a dashboardPage

ui <- dashboardPage(
    skin = "green",
    title = "Claim Insurance",
    header,
    sidebar,
    body
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #---- Year On Year ---------------------------------------------------------------------------------------------------
    
    observe({
        print(input$claimtype1)
        print(input$category1)
        print(input$region1)
    })
    
    output$claimnumber1 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate1[1], register_date <= input$registerdate1[2],
                    claim_type %in% c(input$claimtype1),
                    category %in% c(input$category1),
                    region %in% c(input$region1))                    
        # claim_type %in% c(input$claim_type1),
        # claim_status %in% c(input$claim_status1))
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Total Case"
        )
    })
    output$claimamount1 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate1[1], register_date <= input$registerdate1[2],
                    claim_type %in% c(input$claimtype1),
                    category %in% c(input$category1),
                    region %in% c(input$region1))                     
        
        valueBox(value = comma(sum(claim$claim_amount)),
                 subtitle = "in IDR")
    })
    
    output$plot11 <- renderPlotly({
        yoy_by_claim_number <-  
            claim %>% 
            filter( register_date >= input$registerdate1[1], register_date <= input$registerdate1[2],
                    claim_type %in% c(input$claimtype1),
                    category %in% c(input$category1),
                    region %in% c(input$region1)) %>% 
            group_by(Y_register_date, M_register_date) %>% 
            summarise(total_claim_number = n()) %>% 
            ungroup() %>% 
            mutate(text = glue(
                "Register Date:  {M_register_date} - {Y_register_date}
          Total Claim Number: {total_claim_number}"
            ))
        
        plot_yoy_by_claim_number <- ggplot(data = yoy_by_claim_number, 
                                           aes(fill=M_register_date, 
                                               y=total_claim_number, 
                                               x=interaction(Y_register_date, M_register_date))) +
            geom_segment(aes(x=interaction(Y_register_date, M_register_date), 
                             xend=interaction(Y_register_date, M_register_date), 
                             y=0, 
                             yend=total_claim_number), 
                         color="grey")+
            geom_point(aes(col = M_register_date, size = 4, text = text)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(y = "Total Claim Number",
                 x = "Year & Month of Register Date")
        
        ggplotly(plot_yoy_by_claim_number, tooltip = "text") %>% 
            config(displaylogo = F) %>% 
            layout(showlegend = FALSE)      
        
    })
    
    output$plot12 <- renderPlotly({
        yoy_by_claim_amount <-  
            claim %>% 
            filter( register_date >= input$registerdate1[1], register_date <= input$registerdate1[2],
                    claim_type %in% c(input$claimtype1),
                    category %in% c(input$category1),
                    region %in% c(input$region1)) %>% 
            group_by(Y_register_date, M_register_date) %>% 
            summarise(total_claim_amount = sum(claim_amount)) %>% 
            ungroup() %>% 
            mutate(text = glue(
                "Register Date:  {M_register_date} - {Y_register_date}
          Total Claim Amount: {total_claim_amount}"
            ))
        
        plot_yoy_by_claim_amount <- ggplot(data = yoy_by_claim_amount, 
                                           aes(fill=M_register_date, 
                                               y=total_claim_amount, 
                                               x=interaction(Y_register_date, M_register_date))) +
            geom_segment(aes(x=interaction(Y_register_date, M_register_date), 
                             xend=interaction(Y_register_date, M_register_date), 
                             y=0, 
                             yend=total_claim_amount), 
                         color="grey")+
            geom_point(aes(col = M_register_date, size = 4, text = text)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
            labs(y = "Total Claim Amount",
                 x = "Year & Month of Register Date")
        
        ggplotly(plot_yoy_by_claim_amount, tooltip = "text") %>% 
            config(displaylogo = F) %>% 
            layout(showlegend = FALSE)
        
    })
    #---------------------------------------------------------------------------------------------------------------------
    
    #---- SLA Performance ------------------------------------------------------------------------------------------------
    
    observe({
        print(input$claimtype2)
        print(input$category2)
        print(input$region2)
    })
    
    output$claimnumber2 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype2),
                    category %in% c(input$category2),
                    region %in% c(input$region2))                  
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Total Case"
        )
    })
    output$claimamount2 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype2),
                    category %in% c(input$category2),
                    region %in% c(input$region2))                   
        
        valueBox(value = comma(sum(claim$claim_amount)),
                 subtitle = "in IDR")
    })
    output$claimmeetsla2 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype2),
                    category %in% c(input$category2),
                    region %in% c(input$region2),                    
                    status_sla_complete_doc_to_payment == "Meet SLA")
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Meet SLA"
        )
    })
    output$claimbelowsla2 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype2),
                    category %in% c(input$category2),
                    region %in% c(input$region2),                    
                    status_sla_complete_doc_to_payment == "Below Expectation")
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Below SLA"
        )
    })  
    
    output$plot21 <- renderPlotly({
        
        claim_sla_complete_doc_to_payment <- 
            claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype2),
                    category %in% c(input$category2),
                    region %in% c(input$region2))%>%
            filter(claim_status == "Paid") %>% 
            group_by(
                YYYY_mm_register_date, 
                status_sla_complete_doc_to_payment) %>% 
            summarise(
                count_status_sla_complete_doc_to_payment = 
                    sum(meet_sla_complete_doc_to_payment)+
                    sum(below_sla_complete_doc_to_payment)) %>%
            ungroup() %>% 
            mutate(count_status = ifelse(status_sla_complete_doc_to_payment == "Meet SLA",
                                         count_status_sla_complete_doc_to_payment,
                                         -1*count_status_sla_complete_doc_to_payment)) %>%   
            arrange(desc(YYYY_mm_register_date)) %>%
            mutate(text = glue(
                "SLA Status: {status_sla_complete_doc_to_payment}
        Register Date:  {YYYY_mm_register_date}
        Claim Number: {count_status_sla_complete_doc_to_payment}"
            ))
        
        breaks_values <- pretty(claim_sla_complete_doc_to_payment$count_status)
        
        plot_claim_sla_complete_doc_to_payment <- ggplot(data = claim_sla_complete_doc_to_payment, 
                                                         aes(x = YYYY_mm_register_date, 
                                                             y = count_status, 
                                                             fill = status_sla_complete_doc_to_payment))+
            geom_hline(yintercept = 0)+
            geom_bar(stat = "identity")+
            geom_point(aes(text = text)) +
            coord_flip()+
            scale_y_continuous(breaks = breaks_values,
                               labels = abs(breaks_values))+
            theme_minimal() +
            labs(y = "SLA Status",
                 x = "Year & Month of Register Date")
        
        ggplotly(plot_claim_sla_complete_doc_to_payment, tooltip = "text") %>% 
            config(displaylogo = F) %>% 
            layout(showlegend = F)  
    })
    #---------------------------------------------------------------------------------------------------------------------
    
    #---- What If Analysis -----------------------------------------------------------------------------------------------
    
    output$sla_x_value <- renderText({ paste("Using ", input$sla_x, "Days for SLA as Parameter") })
    
    observe({
        print(input$claimtype3)
        print(input$category3)
        print(input$region3)
    })
    
    output$claimnumber3 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate3[1], register_date <= input$registerdate3[2],
                    claim_type %in% c(input$claimtype3),
                    category %in% c(input$category3),
                    region %in% c(input$region3))
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Total Case"
        )
    })
    output$claimamount3 <- renderValueBox({
        claim <- claim %>% 
            filter( register_date >= input$registerdate3[1], register_date <= input$registerdate3[2],
                    claim_type %in% c(input$claimtype3),
                    category %in% c(input$category3),
                    region %in% c(input$region3))                    
        
        valueBox(value = comma(sum(claim$claim_amount)),
                 subtitle = "in IDR")
    })
    output$claimmeetsla3 <- renderValueBox({
        SLA_x <- input$sla_x #parameter input SLA ini silahkan diganti-ganti, jika ingin tahu bagaimana SLA performance jika SLA diturunkan
        
        claim_sla_x <- 
            claim %>% 
            filter( register_date >= input$registerdate3[1], register_date <= input$registerdate3[2],
                    claim_type %in% c(input$claimtype3),
                    category %in% c(input$category3),
                    region %in% c(input$region3)) %>%
            mutate( 
                status_sla_complete_doc_to_payment_new = 
                    case_when(aging_complete_doc_to_payment > SLA_x ~ "Below Expectation",
                              TRUE ~ "Meet SLA"))

                
        claim <- claim_sla_x %>% 
            filter(status_sla_complete_doc_to_payment_new == "Meet SLA")
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Meet SLA"
        )
    })
    output$claimbelowsla3 <- renderValueBox({
        SLA_x <- input$sla_x #parameter input SLA ini silahkan diganti-ganti, jika ingin tahu bagaimana SLA performance jika SLA diturunkan
        
        claim_sla_x <- 
            claim %>% 
            filter( register_date >= input$registerdate3[1], register_date <= input$registerdate3[2],
                    claim_type %in% c(input$claimtype3),
                    category %in% c(input$category3),
                    region %in% c(input$region3))%>%
            mutate( 
                status_sla_complete_doc_to_payment_new = 
                    case_when(aging_complete_doc_to_payment > SLA_x ~ "Below Expectation",
                              TRUE ~ "Meet SLA"))
        
        claim <- claim_sla_x %>% 
            filter(status_sla_complete_doc_to_payment_new == "Below Expectation")
        
        valueBox(value = comma(length(claim$id), accuracy = NULL),
                 subtitle = "Below SLA"
        )
    })   
    
    output$plot31 <- renderPlotly({
        #karena data di bawah Juli 2018 menggunakan SLA 14 hari maka data difilter hanya untuk yang menggunakan SLA 7 hari yaitu klaim dengan tanggal pengajuan di atas Juni 2018
        
        SLA_x <- input$sla_x #parameter input SLA ini silahkan diganti-ganti, jika ingin tahu bagaimana SLA performance jika SLA diturunkan
        
        claim_sla_x <- 
            claim %>% 
            filter( register_date >= input$registerdate2[1], register_date <= input$registerdate2[2],
                    claim_type %in% c(input$claimtype3),
                    category %in% c(input$category3),
                    region %in% c(input$region3)) %>% 
            filter(claim_status == "Paid") %>%
            mutate( 
                status_sla_complete_doc_to_payment_new = 
                    case_when(aging_complete_doc_to_payment > SLA_x ~ "Below Expectation",
                              TRUE ~ "Meet SLA")) %>% 
            select( 
                YYYY_mm_register_date,
                status_sla_complete_doc_to_payment_new)
        
        claim_sla_x_ready <- 
            claim_sla_x %>% 
            mutate(
                meet_sla_complete_doc_to_payment_new = 
                    case_when(status_sla_complete_doc_to_payment_new != 'Meet SLA' ~ 0, TRUE ~ 1),
                below_sla_complete_doc_to_payment_new = 
                    case_when(status_sla_complete_doc_to_payment_new != 'Below Expectation' ~ 0, TRUE ~ 1)
            )
        
        claim_sla_complete_doc_to_payment_x <- 
            claim_sla_x_ready %>%
            group_by(
                YYYY_mm_register_date, 
                status_sla_complete_doc_to_payment_new) %>% 
            summarise(
                count_status_sla_complete_doc_to_payment_new = 
                    sum(meet_sla_complete_doc_to_payment_new)+
                    sum(below_sla_complete_doc_to_payment_new)) %>% 
            ungroup() %>% 
            mutate(count_status = ifelse(status_sla_complete_doc_to_payment_new == "Meet SLA",
                                         count_status_sla_complete_doc_to_payment_new,
                                         -1*count_status_sla_complete_doc_to_payment_new)) %>%   
            arrange(desc(YYYY_mm_register_date)) %>%
            mutate(text = glue(
                "SLA Status: {status_sla_complete_doc_to_payment_new}
                Register Date: {YYYY_mm_register_date}
                Claim Number: {count_status_sla_complete_doc_to_payment_new}"
            ))  
        
        breaks_values <- pretty(claim_sla_complete_doc_to_payment_x$count_status)
        
        plot_claim_sla_complete_doc_to_payment_x <- ggplot(data = claim_sla_complete_doc_to_payment_x, 
                                                           aes(x = YYYY_mm_register_date, 
                                                               y = count_status, 
                                                               fill = status_sla_complete_doc_to_payment_new))+
            geom_hline(yintercept = 0)+
            geom_bar(stat = "identity")+
            geom_point(aes(text = text)) +
            coord_flip()+
            scale_y_continuous(breaks = breaks_values,
                               labels = abs(breaks_values))+
            theme_minimal() +
            labs(y = "SLA Status",
                 x = "Year & Month of Register Date")
        
        ggplotly(plot_claim_sla_complete_doc_to_payment_x, tooltip = "text") %>% 
            config(displaylogo = F) %>% 
            layout(showlegend = F)        
    })
    #---------------------------------------------------------------------------------------------------------------------
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
