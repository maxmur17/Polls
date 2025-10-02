# Data load

data_path <- "D:\\Work\\Projects\\Fed Election Polls Shiny\\canada_polls_data\\data"

# List of variable names
vars <- c("dat_343", "results_chart", "seats_proj", 
          "dat_343_prov_agg", "prov_results", "dat_343_prov")

# Loop over variable names, read CSV, and assign to same variable name
for (v in vars) {
  assign(v, read.csv(file.path(data_path, paste0(v, ".csv"))))
}

dat_343 <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/dat_343.csv")
dat_343_prov <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/dat_343_prov.csv")
dat_343_prov_agg <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/dat_343_prov_agg.csv")
results_chart <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/results_chart.csv")
seats_proj <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/seats_proj.csv")
prov_results <- readr::read_csv("https://raw.githubusercontent.com/maxmur17/Polls/main/data/prov_results.csv")


# App


ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "superhero"),
  "Canadian Elections Polls",
  navbarMenu("Election",
             tabPanel("Federal",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region", "Region",
                                      choices = dat_343_long$region),
                          pickerInput("party", "Party",
                                      choices = levels(dat_343_long$party),
                                      multiple = T,
                                      options = list(`actions-box`= T)),
                          pickerInput("polls", "Polls",
                                      choices = levels(dat_343_long$firm),
                                      options = list(`actions-box`=T),
                                      multiple = T),
                          
                          checkboxInput("lines", "Election Date", FALSE),
                          strong("App Overview"),
                          br(),
                          p("In the election dropdown menu, you can choose between federal and provincial elections.
                            In the federal and provincial predictions dropdowns, you can choose between popular vote predictions and seat projections"
                          ),
                          br(),
                          p("Page Overview"),
                          br(),
                          p("The purpose of this page is to visualize the polling data for the upcoming Canadian Federal Election.
                            Party input allows to select a federal political party of interest. Polls input allows to select a polling company of interest.
                            Region allows to select a specific region of the polling data.
                            Election date tickbox will display election date on the graph when ticked"),
                          br(),
                          p("To use the app follow the instructions on the right. When all the inputs are selected, this should produce a graph and a table with results.
                            "),
                          br(),
                          p("Please note that selecting only certain polling companies may produce too few datapoints to see the full graph, in that case select additional polling firms.")
                          
                          
                          
                        ),
                        mainPanel(
                          plotlyOutput("plot", width = 1000, height = 500),
                          br(), br(),
                          DT::dataTableOutput(outputId = "pollstable")
                        )
                      )),
             tabPanel("Provincial",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region_prov", "Region",
                                      choices = dat_343_prov$region),
                          pickerInput("party_prov", "Party",
                                      choices = levels(dat_343_prov$party),
                                      options = list(`actions-box`=T),
                                      multiple = T),
                          pickerInput("polls_prov", "Polls",
                                      choices = levels(dat_343_prov$firm),
                                      options = list(`actions-box`=T),
                                      multiple = T),
                          
                          checkboxInput("lines_prov", "Election Date", FALSE),
                          strong("Page Overview"),
                          br(),
                          p("The purpose of this page is to visualize the polling data for the general elections in Canadian Provinces.
                            Party input allows to select a provincial political party of interest. Polls input allows to select a polling company of interest.
                            Region allows to select a specific province.
                            Election date tickbox will display election date on the graph when ticked."),
                          br(),
                          p("To use the app follow the instructions on the right. When all the inputs are selected, this should produce a graph and a table with results."),
                          br(),
                          p("Please note that parties in provincial elections participate in elections only in there province, so they will only appear on the graph of their corresponding province.
                          Thus, selecting 'all' in the party input is highly recommended at first. Also note that the list of polling companies includes all pollsters but some of them conduct regional polling only; thus, 
                            selecting 'all' in the polls input is highly recommended at first.")
                        ),
                        mainPanel(
                          plotlyOutput("plot_prov", width = 1000, height = 500),
                          br(), br(),
                          DT::dataTableOutput(outputId = "pollstable_prov")
                        )
                      ))
             
  ), 
  navbarMenu("Federal Predictions",
             tabPanel("Popular Vote",
                      sidebarLayout(
                        sidebarPanel(
                          strong("Page Overview"),
                          br(),
                          p("This pages shows popular vote predictions based on the aggregation of weighted polls. 
                          The weighting of the polls is based on the date of the poll and its sample size. The closer a poll is to the election date, the more weight is assigned to it.
                          ")
                          
                        ),
                        mainPanel(
                          plotOutput("pop_vote", width = 1000, height = 500)
                        )
                      )),
             tabPanel("Seat Projection",
                      sidebarLayout(
                        sidebarPanel(
                          strong("Page Overview"),
                          br(),
                          p("The seat projection prediction uses a proportional swing method with regional adjustments and ceilings for each party.")
                          
                        ),
                        mainPanel(
                          plotOutput("seat_proj", width = 1000, height = 500)
                        )
                      ))),
  navbarMenu("Provincial Predictions",
             tabPanel("Popular Vote",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region_prov2", "Province",
                                      choices = dat_343_prov$region),
                          strong("Page Overview"),
                          br(),
                          p("This pages shows popular vote predictions based on the aggregation of weighted polls. 
                          The weighting of the polls is based on the date of the poll and its sample size. The closer a poll is to the election date, the more weight is assigned to it.
                          ")
                        ),
                        mainPanel(
                          plotOutput("pop_vote_prov", width = 1000, height = 500)
                        )
                      )
                      
                      
             ),
             tabPanel("Seat Projection",
                      sidebarLayout(
                        sidebarPanel(selectInput("region_prov3", "Province",
                                                 choices = winners2$region),
                                     strong("Page Overview"),
                                     br(),
                                     p("The seat projection prediction uses a proportional swing method with regional adjustments and ceilings for each party.")
                        ),
                        mainPanel(
                          plotOutput("seat_proj_prov", width = 1000, height = 500)
                        )
                      )))
  
)


server <- function(input, output, session) {
  
  polls_selected <- reactive({
    req(input$polls)
    dat_343 %>% select(names(dat_343))
    filter(dat_343, firm %in% input$polls & region %in% input$region)
  })
  
  
  results_chart_react <- reactive({
    results_chart
  })
  
  seats_proj_react <- reactive({
    seats_proj
  })  
  
  pop_vote_prov_react <- reactive({
    req(input$region_prov2)
    dat_343_prov_agg %>% filter(region %in% input$region_prov2)
  })
  
  seats_proj_react_prov <- reactive({
    req(input$region_prov3)
    winners2 %>% filter(region %in% input$region_prov3)
  })
  
  output$plot <- renderPlotly({
    ggplotly(tooltip = "text",{
      dat_343_react <- subset(dat_343_long, firm %in% input$polls &
                                party %in% input$party & region %in% input$region)
      validate(
        need(input$polls != "", "Please select a polling firm"),
        need(input$party != "", "Please select a party"),
        need(input$region != "", "Please select a region")
        
      )
      
      p <-  ggplot(dat_343_react, aes(x=date,y=pop_sup, colour=party, label = firm)) +
        geom_point(alpha=0.5,aes(text=paste0('Date of the Poll: ', date,
                                             '<br>Popular Support:', pop_sup,
                                             '<br>Political Party:', party,
                                             '<br>Polling Firm:', firm))) +
        stat_smooth(span = alpha, se=F) +
        scale_colour_manual(values=colors_pal_app_fed) +
        labs(x="Date of the poll", y="% Popular Support", colour="Party") +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "77 days", date_minor_breaks = "11 days") +
        scale_y_continuous(lim=c(0,56), expand = c(0,0)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90))
      if(input$lines) {
        p + geom_vline(xintercept = as.numeric(as.Date("2025-04-28")), linetype="dashed", color = "grey") +
          annotate(geom = "text", y =53, x=as.Date("2025-04-28"), label = "Election Date", size = 3)
      } else {
        p
      }
      
      
      
    })
    
  })
  
  output$pollstable <- DT::renderDataTable({
    DT::datatable(data = polls_selected(),
                  options = list(pageLength = 10),
                  rownames = F)
    
  })
  dat_343_prov_react <- reactive({
    dat_343_prov %>% 
      filter(party %in% input$party_prov & firm %in% input$polls_prov & region %in% input$region_prov)
    
  })
  
  observeEvent(input$region_prov, {
    req(input$region_prov)
    
    filtered_data <- dat_343_prov %>% filter(region == input$region_prov)
    
    updatePickerInput(session, "party_prov",
                      choices = sort(unique(filtered_data$party)),
                      selected = unique(filtered_data$party))
    
    updatePickerInput(session, "polls_prov",
                      choices = sort(unique(filtered_data$firm)),
                      selected = unique(filtered_data$firm))
  })
  
  selected_position <- reactive(line_positions[[input$region_prov]])
  
  prov_wide <- reactive({
    dat_343_prov_react() %>%
      pivot_wider(names_from = party, values_from = pop_sup)
    
  })
  
  
  
  output$plot_prov <- renderPlotly({
    
    validate(
      need(input$polls_prov != "", "Please select a polling firm"),
      need(input$party_prov != "", "Please select a party"),
      need(input$region_prov != "", "Please select a region")
      
    )
    
    
    
    ggplotly(tooltip = "text",{
      
      g <- ggplot(dat_343_prov_react(), aes(x=date, y=pop_sup, colour = party, label = firm)) +
        geom_point(alpha = 0.5, aes(text = paste0('Date of the Poll: ', date,
                                                  '<br>Popular Support:', pop_sup,
                                                  '<br>Political Party:', party,
                                                  '<br>Polling Firm:', firm))) +
        stat_smooth(span = alpha, se = F) +
        scale_colour_manual(values = colors_pal_app) +
        labs(x="Date of the poll", y="% Popular Support", colour="Party") +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "77 days", date_minor_breaks = "11 days", limits = range) +
        scale_y_continuous(lim=c(0,56), expand = c(0,0)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90)) 
      if(input$lines_prov) {
        g +   geom_vline(xintercept = as.numeric(selected_position()), linetype="dashed", color = "grey") +
          annotate(geom = "text", y =53, x=selected_position(), label = "Election Date", size = 3)
      } else {
        g
      }
      
    })
    
    
    
  })
  
  output$pollstable_prov <- DT::renderDataTable({
    DT::datatable(data = prov_wide(),
                  options = list(pageLength = 10),
                  rownames = F)
    
  })
  
  
  output$pop_vote <- renderPlot(
    ggplot(data = results_chart_react(), aes(x = party_names, y = party_avgs, fill = party_names, label = round(party_avgs, 1))) +
      geom_col() +
      geom_text(size = 5, position = position_stack(vjust = 0.5)) +  
      coord_flip() +
      scale_fill_manual(values=c("blue", "red", "orange", "turquoise4", "green3", "blueviolet")) + 
      theme_classic() +
      theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
  )
  output$seat_proj <- renderPlot(
    ggplot(data = seats_proj_react(), aes(x = party, y = seats, fill = party, label = seats)) +
      geom_col() +
      geom_text(size = 5, position = position_stack(vjust = 0.5)) + 
      coord_flip() +
      scale_fill_manual(values=c("blue", "red", "orange", "turquoise4", "green3", "blueviolet")) + 
      theme_classic() +
      theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
  )
  
  output$pop_vote_prov <- renderPlot({
    validate(
      need(input$region_prov2 != "", "Please select a province")
    )
    ggplot(data = pop_vote_prov_react(), aes(x = party, y = poll_agg, fill = party, label = round(poll_agg, 1))) +
      geom_col() +
      geom_text(size = 5, position = position_stack(vjust = 0.5)) +
      coord_flip() +
      scale_fill_manual(values=colors_pal_app) + 
      theme_classic() +
      theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
  })
  
  output$seat_proj_prov <- renderPlot({
    validate(
      need(input$region_prov3 != "",  "Please select a province")
    )
    ggplot(data = seats_proj_react_prov(), aes(x = party, y = n, fill = party, label = n)) +
      geom_col() +
      geom_text(size = 5, position = position_stack(vjust = 0.5)) + 
      coord_flip() +
      scale_fill_manual(values=colors_pal_app) + 
      theme_classic() +
      theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)