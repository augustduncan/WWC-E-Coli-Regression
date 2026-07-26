# so im gonna have to make this a shiny app. 
# i dont think i have any other options at this point, so lets see how it goes!

library(bslib)
library(crosstalk)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)
library(shiny)
library(stringr)
library(tidyr)


source("myapp/functions.R")


# data set up -- maybe this should go in my other file ... ill do that later. 
# data from predicting.Rmd
mt_data_wide<-read.csv("myapp/dashboard_data.csv", check.names = FALSE)

mt_data_wide<-mt_data_wide %>% mutate(ID = paste0("x", cur_group_id()), .by = Site) %>% relocate(ID, .after=Site)
mt_data_wide$Site<-as.factor(mt_data_wide$Site)

mt_data_wide <- mt_data_wide %>% mutate(across(6:last_col(), ~ round(., 2)))

#we also need a long data set for ggplot: 
mt_data_long<-mt_data_wide %>% pivot_longer(cols=6:last_col(), names_to="Date", values_to="Ecoli") %>%
  mutate(Date = as.Date(Date))

# very roundabout way to get column names since they will change everyday -- 
# these column names are for the textbox mainly  
today_col_wide <- last(colnames(mt_data_wide))

mt_data_wide <- add_groups(mt_data_wide, today_col_wide)
today_group_col_wide <- last(colnames(mt_data_wide))

mt_data_long <- add_groups(mt_data_long, "Ecoli")
mt_data_long$Ecoli.GROUP <- as.factor(mt_data_long$Ecoli.GROUP)

# UI setup
ui <- page_sidebar(
  title = "Warren Wilson EColi Regression",
  theme = bs_theme(bg = "#FDFDFD", fg = "#101712", primary = "#98b39f"),
  
  sidebar = sidebar(
    width = 300,
    uiOutput("site_buttons"),
    actionButton("clear_selection", "Clear Selection", class = "btn-sm btn-outline-secondary mt-2"),
    hr(),
    p("Primary recreation is defined as E. Coli levels less than 126 MPN/100mL, meaning it is recommended safe to swim and be submerged in the water."),
    p("Secondary recreation is defined as E. Coli levels between 126 and 886 MPN/100mL, where the water is not recommended for swimming but generally safe for boating or paddling, with a lower potential for ingestion of the water."),
    p("Above 886 MPN/100mL is classified as unsafe, and it is not recommended to be in the water at this time.")),
  
  layout_columns(
    col_widths = c(6, 6),
    
    card(card_header("Site Map"), leafletOutput("mymap", height = "650px")),
    
    layout_columns( col_widths = 12,
      card(card_header(uiOutput("site_header")), 
           div(style = "white-space: pre-wrap; padding: 10px;", textOutput("site_summary"))),
      card(card_header("Time Series"),
        plotlyOutput("timeseries", height = "400px")))
  )
)

# server setup

server <- function(input, output, session) {
# everything goes in here. the plots, maps, summary, and my reactive variable functions
  
  # reactive variables and filtered data and such
  selected_site <- reactiveVal(NULL)
  marker_just_clicked <- reactiveVal(FALSE)
  
  observeEvent(input$mymap_marker_click, {
    req(input$mymap_marker_click$id)
    marker_just_clicked(TRUE)
    selected_site(input$mymap_marker_click$id)
  })
  
  observeEvent(input$mymap_click, {
    if (marker_just_clicked()) {
      marker_just_clicked(FALSE)
      return()
    }
    selected_site(NULL)
  })
  
  observe({
    sites <- mt_data_wide %>% select(ID) %>% distinct() %>% pull(ID)
    
    for (site_id in sites) {
      local({id <- site_id
        btn_id <- paste0("btn_", id)
        observeEvent(input[[btn_id]], {selected_site(id)})
      })
    }
  })
  
  filtered_long <- reactive({
    site_id <- selected_site()
    if (is.null(site_id)) {
      return(mt_data_long) 
    } else {
      return(mt_data_long %>% filter(ID == site_id))
    }
  })
  
  filtered_wide <- reactive({
    site_id <- selected_site()
    if (is.null(site_id)) {
      return(mt_data_wide)
    } else {
      return(mt_data_wide %>% filter(ID == site_id))
    }
  })
  
  output$mymap <- renderLeaflet({
    pal <- colorFactor(palette = c("primary" = "limegreen", "secondary" = "#f1c40f", "unsafe" = "orangered"), 
                       domain = mt_data_wide[[today_group_col_wide]])
    
    leaflet(mt_data_wide) %>% 
      addTiles() %>%
      addProviderTiles(providers$Stadia.OSMBright) %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        label = ~Site,
        layerId = ~ID,
        fillOpacity = 1, 
        color = ~pal(mt_data_wide[[today_group_col_wide]]))
  })
  
  output$site_buttons <- renderUI({
    sites <- mt_data_wide %>% select(ID, Site) %>% distinct()
    
    button_list <- lapply(seq_len(nrow(sites)), function(i) {
      site_id <- sites$ID[i]
      site_name <- sites$Site[i]
      
      actionButton(
        inputId = paste0("btn_", site_id), 
        label = site_name,
        style = "color: #fff; background-color: #98b39f; border-color: #00000000; font-size: 16px; outline: none;
        border-radius: 5px")
    })
    
    div(style = "display: flex; flex-wrap: wrap; gap: 10px;", button_list)
  })
  
  output$timeseries <- renderPlotly({
    df = filtered_long() %>% na.omit()
    mycolors = c("primary" = "limegreen", "secondary" = "#f1c40f", "unsafe" = "orangered")
    marker_colors <- unname(mycolors[as.character(df$Ecoli.GROUP)])
    
    plot_ly(data = df) %>% 
      add_lines(data = df %>% filter(Variable == "E.Coli.SUM"), 
                x = ~Date, y = ~Ecoli, name = "Sampled E Coli",
                line = list(color = "darkgreen")) %>%
      add_lines(data = df %>% filter(Variable == "Predict"), 
                x = ~Date, y = ~Ecoli, name = "Predicted E Coli",
                line = list(color = "orangered")) %>%
      add_markers(data = df, x = ~Date, y = ~Ecoli, marker = list(size = 8, color = marker_colors),
                  text = ~paste(Variable), hoverinfo = "text+x+y", legendgroup = ~Ecoli.GROUP, 
                  inherit = FALSE, name = "Safety Category") %>%
      layout(title = "Time Series",
             xaxis = list(
               title = "Date",
               range = c("2026-05-01", "2026-08-01")),
             yaxis = list(title = "E. Coli", range = c(0, 5000)),
             showlegend = TRUE)
  })
  
  output$site_header <- renderUI({
    predict_row <- filtered_wide() %>% filter(Variable == "Predict")
    
    site_name <- predict_row$Site
    today_group <- predict_row[[today_group_col_wide]][1]
    
    colors <- c("primary" = "#2ecc71", "secondary" = "#f1c40f", "unsafe" = "#e74c3c")
    
    bg_color <- colors[today_group]
    
    if (length(bg_color) == 0){
      bg_color <- "#00000000"
    }
    
    if (nrow(predict_row) > 2) {
      return("Please click on a site to view more information!")
    }
    
    return(
      div(style = paste0("background-color: ", bg_color, "; color: white; padding: 10px;"), site_name))
  })
  
  output$site_summary <- renderText({
    predict_row <- filtered_wide() %>% filter(Variable == "Predict")
    actual_row <- filtered_wide() %>% filter(Variable == "E.Coli.SUM") %>% select(-last_col()) 
    # last column being groups of the predicted values, which we do not need for the actual row
    
    if (nrow(predict_row) > 2) {
      return("These predictions are made through logarithmic linear regression models. Each site was tested with an accuracy result of between 70-85%. Swim at your own risk!")
    }
    
    site_name <- predict_row$Site
    today_prediction <- predict_row[[today_col_wide]]
    today_group <- predict_row[[today_group_col_wide]]
    last_real_value <- actual_row %>% select(where(~ !any(is.na(.)))) %>% select(last_col())
    last_real_date <- names(last_real_value)
    
    if(is.na(today_prediction)) {
      return(paste0("Unfortunary, we do not have a prediction for today :( ", "\n", "The last recorded value was ",
                    last_real_value, " CFU/100mL, recorded on ", last_real_date, "."))
    }
    
    paste0("Predicted E. coli: ", today_prediction, " MPN/100mL", "\n",
           "Today's safety category is ", today_group, ".", "\n", "Last recorded value: ",
           last_real_value, " CFU/100mL, recorded on ", last_real_date, ".")
  })

}


shinyApp(ui = ui, server = server)



