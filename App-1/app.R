library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(plotly)
library(ggthemes)

SNOTEL_List <- read.csv("SNOTELcsvModded.csv")
SNOTEL_List <- SNOTEL_List %>%
  separate(col = start, into = c("YearBegin", "MonthBegin"), sep = "-")# %>%
#arrange(YearBegin)

ui <- fluidPage(
  h3("Average Daily Air Temp, Precipitation, and Snow Water Equivalent, October 1, 2020 - September 30, 2022"),
  selectInput("state", "Select State", choices = SNOTEL_List$state),
  selectInput("site", "Select Site", NULL),
  actionButton("runButton", "Run"),
  textOutput("text_result"),
  plotlyOutput("result")
)

server <- function(input, output, session) {
  # Reactive values to store selected state and site
  selectedSite <- reactiveVal(NULL)
  selectedState <- reactiveVal(NULL)
  result_data <- reactiveVal(NULL)


  observe({
    # Update choices in the "site" dropdown based on the selected State
    selectedState(input$state)
    updateSelectInput(session, "site", choices = get_sites(input$state))

    # Observe selected site changes
    observe({
      selectedSite(input$site)
    })

  })


  get_sites <- function(state) {
    # Simulate fetching sites based on the selected state
    Sys.sleep(0.5)  # Simulate delay
    switch(state,
           "AK" = filter(SNOTEL_List, state == "AK")$SiteCode,
           "AZ" = filter(SNOTEL_List, state == "AZ")$SiteCode,
           "CA" = filter(SNOTEL_List, state == "CA")$SiteCode,
           "CO" = filter(SNOTEL_List, state == "CO")$SiteCode,
           "ID" = filter(SNOTEL_List, state == "ID")$SiteCode,
           "MT" = filter(SNOTEL_List, state == "MT")$SiteCode,
           "NM" = filter(SNOTEL_List, state == "NM")$SiteCode,
           "NV" = filter(SNOTEL_List, state == "NV")$SiteCode,
           "OR" = filter(SNOTEL_List, state == "OR")$SiteCode,
           "SD" = filter(SNOTEL_List, state == "SD")$SiteCode,
           "UT" = filter(SNOTEL_List, state == "UT")$SiteCode,
           "WA" = filter(SNOTEL_List, state == "WA")$SiteCode,
           "WY" = filter(SNOTEL_List, state == "WY")$SiteCode)
  }


  my_function <- function(site, state){

    raw_data <- httr::GET(url = paste0( "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/", site,":",state, ":SNTL%7Cid=%22%22%7Cname/2020-10-01,2022-09-30/WTEQ::value,PRCP::value,TAVG::value?fitToScreen=false"))

    unpacked_data <- httr::content(raw_data, as = "text", encoding = "UTF-8")

    csv_data <- read.csv2(text = unpacked_data, sep = ";", header = TRUE)



    cleaned_csv <- csv_data %>%
      slice(62:10000000) %>%
      mutate("Data" = X...................................................WARNING.............................................) %>%
      select(Data) %>%
      separate(col = Data, into = c("Date", "SWE", "PrecipitationIncrement", "AvgAirTemp"), sep = ",")

    if(nrow(cleaned_csv) > 0){
      cleaner_csv <- cleaned_csv %>%
        mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
        mutate(SWE = as.numeric(SWE)) %>%
        mutate(AvgAirTemp = as.numeric(AvgAirTemp)) %>%
        mutate(PrecipitationIncrement = as.numeric(PrecipitationIncrement)) %>%
        arrange(Date)

      cleaner_csv_long <- cleaner_csv %>%
        gather(variable, value, SWE, PrecipitationIncrement, AvgAirTemp) %>%
        mutate(variable = if_else(variable == "PrecipitationIncrement", "Precip", variable)) %>%
        group_by(variable) %>%
        filter(n() > 0) %>%
        ungroup()

      ggplotly(
        ggplot(data = cleaner_csv_long)+
          geom_point(mapping = aes(x = Date, y = value, color = variable))+
          geom_path(mapping = aes(x = Date, y = value, group = 1, color = variable)) +
          facet_wrap(~variable, scales = "free_y", ncol = 1)+
          labs(
            x = "Date",
            y = "Inches",
            title = paste0(site, ","," ",state))+
          theme(
            # angle axis labels
            axis.text.x = element_text(angle = -30)
          )
      )
    }
    else{

      print("No Data Available, Please Try Another Site")

    }
  }

  observeEvent(input$runButton, {
    result_plot <- my_function(selectedSite(), selectedState())
    result_data(result_plot)

    if (identical(result_plot, "No Data Available, Please Try Another Site")) {
      # Display the text if there's no data
      output$text_result <- renderText({
        result_plot
      })

      # Clear the plot output
      output$result <- renderPlotly(NULL)
    } else {
      # Display the plot if there's data
      output$result <- renderPlotly({
        result_plot
      })

      # Clear the text output
      output$text_result <- renderText(NULL)
    }
  })
}

shinyApp(ui, server)
