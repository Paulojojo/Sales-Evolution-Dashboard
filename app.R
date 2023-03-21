library(dplyr) # to manipulate data
library(ggplot2) # to create charts
library(lubridate) # to manipulate dates
library(zoo) # for the average rolling
library(shiny) # for the app
library(shinydashboard) # for the tabs and panels creations 
library(shinythemes) # to use the library of themes

# Data set--------------------

# We will try to emulate a real world sales data format, in a wide format
# Each row represents one sale, and each row has 3 variables: date, revenue & cost

set.seed(123)
date_range <- seq(as.Date("2021-01-01"), as.Date("2023-03-31"), by = "day")
n_sales <- round(rnorm(length(date_range), mean = 6, sd = 2))
df_1 <- rep(date_range, n_sales)
revenue <- rnorm(length(df_1), mean = 5000, sd = 980)
cost <- rnorm(length(df_1), mean = 3500, sd = 700)
df_2 <- data.frame(date = df_1, revenue = revenue, cost = cost)


# UI -----------------

# In our dashboard we will only use one filter: dates
# However, we will have three ways to explore the data: by month, by quarter and YoY
# For that reason, we will create three tabs for this dashboard

ui <- fluidPage(
  titlePanel((strong("Sales KPI Evolution"))),
  theme = shinytheme("cosmo"), # the shiny theme we will work with
  sidebarLayout(
    sidebarPanel(
      h4("Select date range"),
      dateRangeInput("date_range", label = NULL, 
                     start = "2022-10-01", end = max(df_2$date),
                     min = min(df_2$date), max = max(df_2$date),
                     weekstart = 1)),
    # I'm selecting a specific date to start, but the filter will all data frame dates availables
    mainPanel(
      tabsetPanel(
        tabPanel(p(strong("Monthly Sales Tab")),
                 h2("Sales Evolution"),
                 plotOutput("sales_plot"),
                 h2("Revenue Evolution $"),
                 plotOutput("revenue_plot"),
                 h2("Margin Evolution $"),
                 plotOutput("margin_plot")),
        tabPanel(p(strong("Quarterly Sales Tab")),
                 h2("Sales Quarterly Evolution"),
                 plotOutput("sales_plot_quarter"),
                 h2("Revenue Quarterly Evolution $"),
                 plotOutput("revenue_plot_quarter"),
                 h2("Margin Quarterly Evolution $"),
                 plotOutput("margin_plot_quarter")),
        tabPanel(p(strong("YoY Sales Tab")),
                 h2("Sales YoY Evolution"),
                 plotOutput("sales_plot_year"),
                 h2("Revenue YoY Evolution $"),
                 plotOutput("revenue_plot_year"),
                 h2("Margin YoY Evolution $"),
                 plotOutput("margin_plot_year"))
      )
    )
  ))

# Server-------------------------------

# In order to use the date, we will create a reactive data frame based on the selected date range

server <- function(input, output) {
  
  #### Sales Data to Manipulate ####
  database <- reactive({
    df_2 %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  sales_data <- reactive({
    database() %>% 
      mutate(id = row(.)) %>%
      group_by(date = floor_date(date, unit = "month")) %>% 
      summarise(sales = n_distinct(id),
                sales_revenue = sum(revenue),
                sales_cost = sum(cost),
                margin = sales_revenue - sales_cost)
  })
  # sales data is group by months. We create the id columns in order to sum the amount of sales
  
  
  #### 12 months average sales data ####
  
  
  # We create the 12 rolling average for three major variables: sales, revenue and margin
  #Notice we are doing a double filter, the first one is ti get data from th 12 month previous months
  #The second filter helps us select only the data we will be using for visualization
  sales_avg <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      mutate(id = row(.)) %>%
      group_by(date = floor_date(date, unit = "month")) %>% 
      summarise(sales = n_distinct(id)) %>%
      mutate(prev_sales_avg = rollapplyr(sales, 12, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_sales_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_sales_avg)
  })
  
  revenue_avg <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      group_by(date = floor_date(date, unit = "month")) %>% 
      summarise(revenue = sum(revenue)) %>%
      mutate(prev_revenue_avg = rollapplyr(revenue, 12, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_revenue_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_revenue_avg)
  })
  
  margin_avg <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      mutate(margin = revenue - cost) %>%
      group_by(date = floor_date(date, unit = "month")) %>% 
      summarise(margin = sum(margin)) %>%
      mutate(prev_margin_avg = rollapplyr(margin, 12, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_margin_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_margin_avg)
  })
  
  #### Quarter Sales data ####
  
  # We replicate the same exercise for the quarterly sales, revenue and margin
  sales_data_quarter <- reactive({
    database() %>% 
      mutate(id = row(.)) %>%
      group_by(date = floor_date(date, unit = "quarter")) %>% 
      summarise(sales = n_distinct(id),
                sales_revenue = sum(revenue),
                sales_cost = sum(cost),
                margin = sales_revenue - sales_cost) %>%
      mutate(date_quarter = paste(year(date),"-", quarter(date)))
  })
  
  
  sales_avg_quarter <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      mutate(id = row(.)) %>%
      group_by(date = floor_date(date, unit = "quarter")) %>% 
      summarise(sales = n_distinct(id)) %>%
      mutate(prev_sales_avg = rollapplyr(sales, 4, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_sales_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_sales_avg)
  })
  
  revenue_avg_quarter <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      group_by(date = floor_date(date, unit = "quarter")) %>% 
      summarise(revenue = sum(revenue)) %>%
      mutate(prev_revenue_avg = rollapplyr(revenue, 4, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_revenue_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_revenue_avg)
  })
  
  margin_avg_quarter <- reactive({
    df_2 %>% 
      filter(date <= input$date_range[2])  %>%
      mutate(margin = revenue - cost) %>%
      group_by(date = floor_date(date, unit = "quarter")) %>% 
      summarise(margin = sum(margin)) %>%
      mutate(prev_margin_avg = rollapplyr(margin, 4, mean, partial = TRUE, fill = NA)) %>% 
      filter(!is.na(prev_margin_avg),
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(date, prev_margin_avg)
  })
  
  #### YoY Sales data ####
  
  # We replicate the same exercise for the quarterly sales, revenue and margin,
  # except that we won't be using a rolling average here
  sales_data_year <- reactive({
    database() %>% 
      mutate(id = row(.)) %>%
      group_by(date = floor_date(date, unit = "year")) %>% 
      summarise(sales = n_distinct(id),
                sales_revenue = sum(revenue),
                sales_cost = sum(cost),
                margin = sales_revenue - sales_cost)
  })
  
  
  #### Monthly Tab-----
  
  ## Sales Unit Plot
  output$sales_plot <- renderPlot({
    ggplot(sales_data(), aes(x = date, y = sales)) +
      geom_line(aes(y = sales, color = "Sales")) +
      labs(x = NULL, y = "Units", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data()$date), unit = "month"),
                                ceiling_date(max(sales_data()$date), unit = "month"),
                                by = "1 month")) +
      geom_line(data = sales_avg(), aes(x = date, y = prev_sales_avg, color = "12 months rolling")) +
      scale_color_manual(values = c("Sales" = "blue", "12 months rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  ## Revenue plot
  output$revenue_plot <- renderPlot({
    ggplot(sales_data(), aes(x = date, y = sales_revenue)) +
      geom_line(aes(y = sales_revenue, color = "Revenue")) +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data()$date), unit = "month"),
                                ceiling_date(max(sales_data()$date), unit = "month"),
                                by = "1 month")) +
      geom_line(data = revenue_avg(), aes(x = date, y = prev_revenue_avg, color = "12 months rolling")) +
      scale_color_manual(values = c("Revenue" = "green", "12 months rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  ## Margin plot
  output$margin_plot <- renderPlot({
    ggplot(sales_data(), aes(x = date, y = margin)) +
      geom_line(aes(y = margin, color = "Margin")) +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data()$date), unit = "month"),
                                ceiling_date(max(sales_data()$date), unit = "month"),
                                by = "1 month")) +
      geom_line(data = margin_avg(),
                aes(x = date, y = prev_margin_avg, color = "12 Months Rolling")) +
      scale_color_manual(values = c("Margin" = "purple", "12 Months Rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  ## Quarterly Tab-----
  
  
  ## Sales Plot
  output$sales_plot_quarter <- renderPlot({
    ggplot(sales_data_quarter(), aes(x = date, y = sales)) +
      geom_line(aes(y = sales, color = "Sales")) +
      labs(x = NULL, y = "Units", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data_quarter()$date), unit = "quarter"),
                                ceiling_date(max(sales_data_quarter()$date), unit = "quarter"),
                                by = "3 months")) +
      geom_line(data = sales_avg_quarter(), aes(x = date, y = prev_sales_avg, color = "Quarter rolling")) +
      scale_color_manual(values = c("Sales" = "blue", "Quarter rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  ## Revenue plot
  output$revenue_plot_quarter <- renderPlot({
    ggplot(sales_data_quarter(), aes(x = date, y = sales_revenue)) +
      geom_line(aes(y = sales_revenue, color = "Revenue")) +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data_quarter()$date), unit = "quarter"),
                                ceiling_date(max(sales_data_quarter()$date), unit = "quarter"),
                                by = "3 months")) +
      geom_line(data = revenue_avg_quarter(), aes(x = date, y = prev_revenue_avg, color = "Quarter rolling")) +
      scale_color_manual(values = c("Revenue" = "green", "Quarter rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  ## Margin plot
  output$margin_plot_quarter <- renderPlot({
    ggplot(sales_data_quarter(), aes(x = date, y = margin)) +
      geom_line(aes(y = margin, color = "Margin")) +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y-%m",
                   breaks = seq(floor_date(min(sales_data_quarter()$date), unit = "quarter"),
                                ceiling_date(max(sales_data_quarter()$date), unit = "quarter"),
                                by = "3 months")) +
      geom_line(data = margin_avg_quarter(),
                aes(x = date, y = prev_margin_avg, color = "Quarter Rolling")) +
      scale_color_manual(values = c("Margin" = "purple", "Quarter Rolling" = "orange")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            panel.background = element_blank())
  })
  
  
  #### YoY Tab-----
  
  # For year on year we will use geom_bar chart
  # Because we already summarise the sale data we will use stat = "identity"
  # Now, the accumulated revenue and margin are really big numbers
  # We will be using the function formatC to make then easy to read
  
  ## Sales Plot
  output$sales_plot_year <- renderPlot({
    ggplot(sales_data_year(), aes(x = date, y = sales)) +
      geom_bar(stat = "identity",  fill = "darkgreen") +
      labs(x = NULL, y = "Units", color = NULL) +
      scale_x_date(date_labels = "%Y",
                   breaks = seq(floor_date(min(sales_data_year()$date), unit = "year"),
                                ceiling_date(max(sales_data_year()$date), unit = "year"),
                                by = "12 months")) +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(size = 14),
            legend.position = "none") +
      geom_text(label = sales_data_year()$sales, vjust = 1.5, colour = "white",
                size = 7)
  })
  
  ## Revenue plot
  output$revenue_plot_year <- renderPlot({
    ggplot(sales_data_year(), aes(x = date, y = sales_revenue)) +
      geom_bar(stat = "identity",  fill = "darkblue") +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y",
                   breaks = seq(floor_date(min(sales_data_year()$date), unit = "year"),
                                ceiling_date(max(sales_data_year()$date), unit = "year"),
                                by = "12 months"))+
      theme(panel.background = element_blank(),
            axis.text.x = element_text(size = 14),
            legend.position = "none") +
      geom_text(label = formatC(sales_data_year()$sales_revenue,
                                format = "f",
                                big.mark = ",", digits = 0),
                vjust = 1.5, colour = "white",
                size = 7)
  })
  
  ## Margin plot
  output$margin_plot_year <- renderPlot({
    ggplot(sales_data_year(), aes(x = date, y = margin)) +
      geom_bar(stat = "identity",  fill = "darkturquoise") +
      labs(x = NULL, y = "$ USD", color = NULL) +
      scale_x_date(date_labels = "%Y",
                   breaks = seq(floor_date(min(sales_data_year()$date), unit = "year"),
                                ceiling_date(max(sales_data_year()$date), unit = "year"),
                                by = "12 months"))+
      theme(panel.background = element_blank(),
            axis.text.x = element_text(size = 14),
            legend.position = "none") +
      geom_text(label = formatC(sales_data_year()$margin, format = "f",
                                big.mark = ",", digits = 0),
                vjust = 1.5, colour = "black",
                size = 7)
  })
}

# Run the app--------------

shinyApp(ui = ui, server = server)
