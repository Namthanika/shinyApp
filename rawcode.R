# if you save this file in the same directory as "TotalEmissionsGermanyInKT.xlsx" and name it "app",
# RStudio will automatically recognize it as a shiny app!

# load packages, install them outside the app-script 
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# read data
emissions <- read_excel("TotalEmissionsGermanyInKT.xlsx")

# tidy data
emissions_restructed <- emissions %>% 
  tidyr::gather(key = "year", value = "emissions", 2:10)

# set order for pollutants-appearance
pollutantsorder <- c("TSP", "SO2", "PM 2.5", "PM 10", "NOx", "NMVOCs", "NH3", "CO")

# build user interface
ui <- shinyUI(
  # header panel
  fluidPage(
    fluidRow(column(width = 10, style = "font-size: 25pt; line-height: 40pt; width = 100", # dividing header panel in two columns
                    tags$strong("Germanys air pollutants emissions")), # tags are for html-functions
             column(width = 2,tags$head(tags$img(src='http://journocode.com/wordpress/wp-content/uploads/2016/01/small-logo.png', align = "left", width= "100")))
    ),
    # sidebar
    sidebarPanel(style = "background-color: #78d9c9;",
                 tags$style(type='text/css', 
                            ".selectize-input { font-size: 12pt; line-height: 13pt;} 
                            .selectize-dropdown { font-size: 12pt; line-height: 13pt; }"),
                 width = 3,
                 checkboxGroupInput("pollutants",label=HTML('<p style="color:white; font-size: 12pt"> Pollutants </p>'), choices=pollutantsorder, selected = pollutantsorder),
                 selectInput("year",label=HTML('<p style="color:white; font-size: 12pt"> Year </p>'), choices=unique(emissions_restructed$year), multiple = TRUE, selected = c(2005:2013)),
                 helpText(HTML('<p style="color:white; font-size: 9pt">choose air pollutants by clicking the check boxes,
                               exclude years with a click and the backspace key</p>'))
                 ),
    # main with tabs
    mainPanel(tabsetPanel(type = "tabs", 
                          tabPanel("Graphic", plotOutput("mplot"), style = "width:100%"), # plotOutput, htmlOutput etc. as defined later in the server part
                          tabPanel("Data", htmlOutput("text"), tableOutput("table"), style = "font-size:70%", htmlOutput("datasource")),
                          # to include the markdown we only need this short line of code
                          tabPanel("Code", includeMarkdown("emissions_app.Rmd"))
    )
    )
                 ))

# build server part
server <- shinyServer(function(input, output) {
  # reactive operations
  data <- reactive({
    
    validate( # error message, if no input is selected
      need(input$pollutants != "", "Please select at least one air polutant"),
      need(input$year != "", "Please select at least one year")
    )
    # filter data for plot dependent on reactive data input
    plotdata <- emissions_restructed  %>%
      as.data.frame() %>%
      filter(pollutants %in% input$pollutants & year %in% input$year)
    
    # "omptimize" range of y-axis scales dependent on reactive data input
    scalecalc <- plotdata %>%
      group_by(year) %>%
      summarize(value = sum(emissions))
    
    scalemax <- max(scalecalc$value)
    scalesteps <- round(scalemax/5, digits = -1)
    # make list of reactive results to use for the outputs
    list(plotdata = plotdata,
         scalemax = scalemax,
         scalesteps = scalesteps
    )
  })
# outputs, taking results of the reactive data()
  # build the plot
  output$mplot <- renderPlot({
    myplot <- ggplot(data = data()$plotdata, aes(as.factor(year), y = emissions, fill = factor(pollutants), order = pollutants)) + 
      geom_bar(stat = "identity") + 
      xlab("Year") +
      ylab("Emissions in kt") +
      theme_minimal()+
      ggtitle("Barchart of air pollutants emissions\n") +
      guides(fill=guide_legend(title="pollutants", reverse = T))+
      scale_y_continuous(breaks=seq(0,data()$scalemax, data()$scalesteps),
                         labels=abs(seq(0,data()$scalemax, data()$scalesteps))) +
      theme(plot.title=element_text(family="Arial", face="bold", size=18),
            axis.text.x = element_text(angle = 0, family="Arial", size=13), 
            axis.text.y = element_text(angle = 0, family="Arial", size=13),
            axis.title.x = element_text(size=14, face="bold", vjust = -1),
            axis.title.y = element_text(size=14, face="bold", vjust = 2)
      ) +
      scale_fill_manual(values = c("TSP" = "#ffc7e4", "SO2" = "#ffb155", "PM 2.5" = "#ff6f69", "PM 10" = "#b1e6e6", 
                                   "NOx" = "#77b1d5", "NMVOCs" = "#c0b7db", "NH3" = "#fcffaf", "CO" = "#78d9c9"), drop = F)
    
    print(myplot)
  })
  # reactive table
  output$table <- renderTable({
    emissions_restructed %>%
      filter(pollutants %in% input$pollutants & year %in% input$year) %>%
      spread(year, emissions)  
  })
  # html-output, locations of outputs are defined in the user interface part
  output$datasource <- renderUI({
    tags$div(
      tags$strong("Source:"), 
      tags$a("Umweltbundesamt", href="http://www.umweltbundesamt.de/daten/luftbelastung/luftschadstoff-emissionen-in-deutschland")
    )   
  })  
  output$text <- renderUI({
    tags$div(
      HTML('<p style="color:black; font-size: 9pt">This data on Germanys air pollutants emissions was downloaded from the german
           Federal Environment Agencys website. The table provides information on nitrogen oxides (NOx), ammonia (NH3), volatile 
           organic compounds without methane (NMVOC), sulfur dioxide (SO2) and dust - including the fine dust fractions PM 10 and PM 2.5 
           - and carbon monoxide (CO).</p>')
    )   
  })
})

# merge parts to shiny-app
shinyApp(ui = ui, server = server)