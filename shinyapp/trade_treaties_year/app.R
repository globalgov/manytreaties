library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidygraph)

# Prepare data
trade_mem <- manytreaties::memberships$HUGGO_MEM %>%
    dplyr::filter(Domain == "Trade") %>%
    dplyr::select(manyID, stateID, Title, StateSignature,
                  StateRatification, StateForce) %>%
    manydata::transmutate(StateBegin = dplyr::coalesce(messydates::year(StateSignature),
                                                       messydates::year(StateRatification),
                                                       messydates::year(StateForce))) %>%
    dplyr::arrange(StateBegin) %>%
    dplyr::filter(StateBegin != "NA") %>%
    dplyr::mutate(Type = "NA")
# Determine whether an agreement is bilateral or multilateral
i <- 0
manyID <- NA
for(i in 1:nrow(trade_mem)){
  manyID <- as.character(trade_mem[i, 1])
  if(sum(trade_mem$manyID == manyID) > 2) {
    trade_mem[i, 5] <- "Multilateral"
  }
  else {
    trade_mem[i, 5] <- "Bilateral"
  }
}


# Define dashboard interface
ui <- dashboardPage(
    dashboardHeader(title = "Trade treaties by year from 1948 to 2020", titleWidth = "450px"),
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            selectInput("country",
                        "Select country(s):",
                        choices = c("choose" = "", stringr::str_sort(trade_mem$stateID)),
                        selected = "choose",
                        multiple = T),
            selectInput("type",
                        "Select type:",
                        choices = c("choose" = "", "Bilateral", "Multilateral"),
                        selected = "choose",
                        multiple = T),
            menuItem(sliderInput("range", "Dates", value = c(1950, 1957), min = 1948, 
                                 max = 2020, width = 350, sep = "")),
            checkboxInput("treatylabel", "Display treaty node labels", TRUE),
            checkboxInput("countrylabel", "Display country node labels", TRUE)),
        wellPanel(
          style = " background: #222D32; border-color: #222D32; margin-left: 20px",
          textOutput("click_info"),
          tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
          tags$head(tags$style(".shiny-output-error:after{content: 'No treaties found. Please try again with different inputs.';
visibility: visible}"))
        )),
    dashboardBody(
        plotOutput("distPlot", height = "550px",
                   click = "plot_click")
    ))

# Connect the data with the interface
server <- function(input, output){
  # Save a data frame of agreement titles
    titles <- trade_mem %>%
      dplyr::distinct(manyID, .keep_all = TRUE) %>%
      dplyr::select(manyID, Title)
  # filteredData for each combination of inputs
  # Four possibilities for each combination of inputs due to possible
  # combinations of display labels checkboxes
  # Preserve trade_mem dataframe for later use with coordinates
    filteredData <- reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                          TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                         TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                         TRUE ~ "square"))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    filteredData2 <- reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                          TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                         TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                          TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    filteredData3 <- reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      if(input$treatylabel == FALSE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    filteredData4 <- reactive({
      if (input$treatylabel == TRUE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))   %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))   %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        trade_mem1 <- trade_mem %>%
          dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
          dplyr::filter(stateID %in% input$country) %>%
          dplyr::filter(Type %in% input$type) %>%
          manynet::as_tidygraph() %>%
          tidygraph::activate(nodes) %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))   %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    # Create a plot object, whose coordinates will serve as reference
    # for the click interactivity on the plot that will be rendered.
    # Using the original trade_mem data frame instead of the tidygraph object
    # created after filtering data allows to display titles of agreements even
    # if their labes are not rendered on the actual plot.
    coords1 <- reactive({
      ggdata1 <- trade_mem %>%
        dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata1 <- ggplot2::ggplot_build(ggdata1)$data[[1]]
    })
    coords2 <- reactive({
      ggdata2 <- trade_mem %>%
        dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
        dplyr::filter(stateID %in% input$country) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata2 <- ggplot2::ggplot_build(ggdata2)$data[[1]]
    })
    coords3 <- reactive({
      ggdata3 <- trade_mem %>%
        dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
        dplyr::filter(Type %in% input$type) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata3 <- ggplot2::ggplot_build(ggdata3)$data[[1]]
    })
    coords4 <- reactive({
      ggdata4 <- trade_mem %>%
        dplyr::filter(StateBegin >= input$range[1] & StateBegin <= input$range[2]) %>%
        dplyr::filter(stateID %in% input$country) %>%
        dplyr::filter(Type %in% input$type) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata4 <- ggplot2::ggplot_build(ggdata4)$data[[1]]
    })
    # Render plots according to combination of inputs
    output$distPlot <- renderPlot({
        if(is.null(input$country) & is.null(input$type)){
          manynet::autographr(filteredData(), node_color = "color", node_size = "size",
                              node_shape = "shape", labels = TRUE)
        }
        else if(!is.null(input$country) & !is.null(input$type)){
          manynet::autographr(filteredData4(), node_color = "color", node_size = "size",
                                node_shape = "shape", labels = TRUE)
          }
        else if(is.null(input$type)){
          manynet::autographr(filteredData2(), node_color = "color", node_size = "size",
                                node_shape = "shape", labels = TRUE)
        }
        else if(is.null(input$country)){
          manynet::autographr(filteredData3(), node_color = "color", node_size = "size",
                                node_shape = "shape", labels = TRUE)
        }
    })
    # Add interactivity to display the titles of agreements the user
    # clicks on, according to possible input combinations.
      output$click_info <- renderText({
        if(is.null(input$country) & is.null(input$type)){
        point <- nearPoints(coords1(), input$plot_click, 
                   addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
          if(title == "character(0)"){
            print("Please click on a node representing a treaty (red circle) to display its title.")
          }
          else if(!(title == "character(0)")){
            print(title)
          }
        }
        else if(is.null(input$type)){
          point <- nearPoints(coords2(), input$plot_click, 
                     addDist = TRUE)
          title <- as.character(titles[titles$manyID %in% point$label, 2])
          if(title == "character(0)"){
            print("Please click on a node representing a treaty (red circle) to display its title.")
          }
          else if(!(title == "character(0)")){
            print(title)
          }
        }
          
        else if(is.null(input$country)){
          point <- nearPoints(coords3(), input$plot_click, 
                              addDist = TRUE)
          title <- as.character(titles[titles$manyID %in% point$label, 2])
          if(title == "character(0)"){
            print("Please click on a node representing a treaty (red circle) to display its title.")
          }
          else if(!(title == "character(0)")){
            print(title)
          }
        }
        else if(!is.null(input$country) & !is.null(input$type)){
          point <- nearPoints(coords4(), input$plot_click, 
                              addDist = TRUE)
          title <- as.character(titles[titles$manyID %in% point$label, 2])
          if(title == "character(0)"){
            print("Please click on a node representing a treaty (red circle) to display its title.")
          }
          else if(!(title == "character(0)")){
            print(title)
          }
        }
    })
}

# I will add an interactive map here later, for reference see:
# https://www.r-bloggers.com/2020/04/hands-on-how-to-build-an-interactive-map-in-r-shiny-an-example-for-the-covid-19-dashboard/
# ui<- fluidPage(
#     #Assign Dasbhoard title 
#     titlePanel("Trade Agreements per Country"),
#     sliderInput(inputId = "date", "Date:", min = 1948, max = 2021, 
#                 value = 1989, width = "600px"),
#     # plot leaflet object (map) 
#     leafletOutput(outputId = "distPlot", width = "700px", 
#                   height = "300px"),
#     #End:  the First Block
#     #Start: the second Block
#     sidebarLayout(
#         #Sidebar Panel: the selected country, history and 
#         #whether to plot daily new confirmed cases.
#         sidebarPanel(
#             selectInput("selectedcountry", h4("Country"), choices 
#                         =list("China","US","United_Kingdom","Italy","France",
#                               "Germany", "Spain"), selected = "US"),
#             selectInput("selectedhistoricwindow", h4("History"), 
#                         choices = list("the past 10 days", "the past 20 
#       days"), selected = "the past 10 days"),
#             checkboxInput("dailynew", "Daily new infected", 
#                           value = TRUE),
#             width = 3  
#         ),
#         #Main Panel: plot the selected values
#         mainPanel (
#             plotOutput(outputId = "Plotcountry", width = "500px", 
#                        height = "300px")
#         )
#     ),
#     #End: the second Block 
# )
# server <- function(input, output){
#     #Assign output$distPlot with renderLeaflet object
#     output$distPlot <- renderLeaflet({
#         # row index of the selected date (from input$date)
#         rowindex = which(as.Date(as.character(daten$Date), 
#                                  "%d.%m.%Y") ==input$date)
#         # initialise the leaflet object
#         basemap= leaflet()  %>%
#             addProviderTiles(providers$Stamen.TonerLite,
#                              options = providerTileOptions(noWrap = TRUE)) 
#         # assign the chart colors for each country, where those 
#         # countries with more than 500,000 cases are marked 
#         # as red, otherwise black
#         chartcolors = rep("black",7)
#         stresscountries 
#         = which(as.numeric(daten[rowindex,c(2:8)])>50000)
#         chartcolors[stresscountries] 
#         = rep("red", length(stresscountries))
#         
#         # add chart for each country according to the number of 
#         # confirmed cases to selected date 
#         # and the above assigned colors
#         basemap %>%
#             addMinicharts(
#                 citydaten$long, citydaten$Lat,
#                 chartdata = as.numeric(daten[rowindex,c(2:8)]),
#                 showLabels = TRUE,
#                 fillColor = chartcolors,
#                 labelMinSize = 5,
#                 width = 45,
#                 transitionTime = 1
#             ) 
#     })
#     #Assign output$Plotcountry with renderPlot object
#     output$Plotcountry <- renderPlot({
#         #the selected country 
#         chosencountry = input$selectedcountry
#         #assign actual date
#         today = as.Date("2020/04/02")
#         #size of the selected historic window
#         chosenwindow = input$selectedhistoricwindow
#         if (chosenwindow == "the past 10 days")
#         {pastdays = 10}
#         if (chosenwindow  == "the past 20 days")
#         {pastdays = 20}
#         #assign the dates of the selected historic window
#         startday = today-pastdays-1
#         daten$Date=as.Date(as.character(daten$Date),"%d.%m.%Y")
#         selecteddata 
#         = daten[(daten$Date>startday)&(daten$Date<(today+1)), 
#                 c("Date",chosencountry)]
#         #assign the upperbound of the y-aches (maximum+100)
#         upperboundylim = max(selecteddata[,2])+100
#         #the case if the daily new confirmed cases are also
#         #plotted
#         if (input$dailynew == TRUE){
#             plot(selecteddata$Date, selecteddata[,2], type = "b", 
#                  col = "blue", xlab = "Date", 
#                  ylab = "number of infected people", lwd = 3, 
#                  ylim = c(0, upperboundylim))
#             par(new = TRUE)
#             plot(selecteddata$Date, c(0, diff(selecteddata[,2])), 
#                  type = "b", col = "red", xlab = "", ylab = 
#                      "", lwd = 3,ylim = c(0,upperboundylim))
#             #add legend
#             legend(selecteddata$Date[1], upperboundylim*0.95, 
#                    legend=c("Daily new", "Total number"), 
#                    col=c("red", "blue"), lty = c(1,1), cex=1)
#         }
#         #the case if the daily new confirmed cases are 
#         #not plotted
#         if (input$dailynew == FALSE){
#             plot(selecteddata$Date, selecteddata[,2], type = "b", 
#                  col = "blue", xlab = "Date", 
#                  ylab = "number of infected people", lwd = 3,
#                  ylim = c(0, upperboundylim))
#             par(new = TRUE)
#             #add legend
#             legend(selecteddata$Date[1], upperboundylim*0.95, 
#                    legend=c("Total number"), col=c("blue"), 
#                    lty = c(1), cex=1)
#         }
#     })
# }

# Run the application
shinyApp(ui = ui, server = server)
