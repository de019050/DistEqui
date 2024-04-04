library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(overlapping)
library(moments)

# Sample datasets
set1 <- runif(1000)
set2 <- rnorm(1000, mean = 2)
REF <-rnorm(1e5)

#Function to plot Overlap
final.plot <- function(x, OV = NULL ) {
  
  #has.ggplot2 <- requireNamespace("ggplot2")
  
  #  if (has.ggplot2) {
  #if (!isNamespaceLoaded("ggplot2")) attachNamespace("ggplot2")
  AREA <- NULL
  
  for (i1 in 1:(length(x) - 1)) {
    for (i2 in (i1 + 1):(length(x))) {
      A <- data.frame(x = x[[i1]],
                      group = names(x)[i1],
                      k = paste(names(x)[i1],
                                names(x)[i2], sep = "-", collapse = ""))
      B <- data.frame(x = x[[i2]],
                      group = names(x)[i2],
                      k = paste(names(x)[i1],
                                names(x)[i2], sep = "-", collapse = ""))
      AREA <- rbind(AREA, rbind(A, B))
    }
  }
  
  if (!is.null(OV)){
    for (j in 1:length(levels(AREA$k))) {
      levels(AREA$k)[j] <- paste(levels(AREA$k)[j], " (ov. perc. ",
                                 round(OV[grep(levels(AREA$k)[j],
                                               names(OV), fixed = TRUE)]*100), ")", sep = "")
    }
  }
  ggplot(AREA, aes(x = x)) +
    facet_wrap(~k) +
    geom_density(aes(fill = group), alpha = .35) +
    theme_minimal()+
    xlab("") + theme(legend.title = element_blank())
  #  } #else {
  #    warning("package ggplot2 is missing.")
  #  }
}


# Function to calculate skewness and kurtosis
calc_moments <- function(data) {
  moments_data <- data.frame(
    Value = c(
      mean(data),
      min(data),
      max(data),
      median(data),
      sd(data),
      moments::skewness(data),
      moments::kurtosis(data)
    )
  )
  return(moments_data)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Distribution Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Selection", tabName = "data"),
      menuItem("Distribution Moments", tabName = "moments")
    )
  ),
  dashboardBody(
    tabItems(
      # Data Selection Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Select Datasets",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("dataset1", "Select Dataset 1", c("Set 1" = "set1", "Set 2" = "set2"), selected = "set1"),
            selectInput("dataset2", "Select Dataset 2", c("Set 1" = "set1", "Set 2" = "set2"), selected = "set2")
          ),
          #
          box(
            title = "Histogram and Density Plot",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("histogram_density_plot")
          )
        ),
        box(
          title = "Data",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          DTOutput("data_table")
        ),
      ),
      # Distribution Moments Tab
      tabItem(
        tabName = "moments",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Distribution Moments",
              status = "info",
              solidHeader = TRUE,
              DTOutput("moments_table")
            ),
            box(
              title = "Analytic Comparision",
              status = "info",
              solidHeader = TRUE,
              DTOutput("comp_table")
            )
          ),
          column(
            width = 6,
            box(
              title = "Histogram with Max",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("histogram_max_plot")
            ),
            box(
              title = "Comparison of Moments",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("moments_plot")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Moments Summary",
              status = "info",
              solidHeader = TRUE,
              DTOutput("moments_summary_table")
            )
          ),
          column(
            width = 6,
            box(
              title = "Histogram with Median",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("histogram_median_plot")
            ),
            box(
              title = "Comparison of Densities",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("overlap_plot")
            )
          )
          
        )
      )
    )
  )
)



# Server
server <- function(input, output) {
  # Selected data based on input
  selected_data1 <- reactive({
    switch(input$dataset1,
           "set1" = set1,
           "set2" = set2)
  })
  
  selected_data2 <- reactive({
    switch(input$dataset2,
           "set1" = set1,
           "set2" = set2)
  })
  
  
  # Histogram and Density Plot
  output$histogram_density_plot <- renderPlotly({
    data1 <- selected_data1()
    data2 <- selected_data2()
    
    p <- ggplot() +
      geom_histogram(data = data.frame(x = data1), aes(x, y = ..density.., fill = "Dataset 1"), alpha = 0.5, bins = 30) +
      geom_histogram(data = data.frame(x = data2), aes(x, y = ..density.., fill = "Dataset 2"), alpha = 0.5, bins = 30) +
      geom_density(data = data.frame(x = data1), aes(x, color = "Dataset 1")) +
      geom_density(data = data.frame(x = data2), aes(x, color = "Dataset 2")) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Data table
  output$data_table <- renderDT({
    data_combined <- data.frame(Data1 = selected_data1(), Data2 = selected_data2())
    datatable(data_combined, options = list(scrollY = TRUE))
  })
  
  # Calculate skewness and kurtosis for the selected data
  moments_data <- reactive({
    data1 <- selected_data1()
    data2 <- selected_data2()
    
    moments_data <- data.frame(
      Moments = c("Mean", "Min", "Max", "Median", "Standard Deviation", "Skewness", "Kurtosis"),
      `Data Set 1` = c(
        round(mean(data1,na.rm=T),digits = 3),
        round(min(data1,na.rm=T),digits = 3),
        round(max(data1,na.rm=T),digits = 3),
        round(median(data1,na.rm=T),digits = 3),
        round(sd(data1,na.rm=T),digits = 3),
        round(moments::skewness(data1,na.rm=T),digits = 3),  # Use moments package for skewness
        round(moments::kurtosis(data1,na.rm=T),digits = 3)  # Use moments package for kurtosis
      ),
      `Data Set 2` = c(
        round(mean(data2,na.rm=T),digits = 3),
        round(min(data2,na.rm=T),digits = 3),
        round(max(data2,na.rm=T),digits = 3),
        round(median(data2,na.rm=T),digits = 3),
        round(sd(data2,na.rm=T),digits = 3),
        round(moments::skewness(data2,na.rm=T),digits = 3),  # Use moments package for skewness
        round(moments::kurtosis(data2,na.rm=T),digits = 3)  # Use moments package for kurtosis
      )
    )
    
    return(moments_data)
  })
  
  # Distribution Moments Table
  output$moments_table <- renderDT({
    datatable(moments_data())
  })
  
  # Create a data frame with mean and standard deviation values
  moments_summary <- reactive({
    moments_data_df <- moments_data()
    
    if (!is.null(moments_data_df)) {
      mean1 <-1
      mean2 <-2
      sd1 <- 3
      sd2 <- 4
      
      data_frame <- data.frame(
        Statistic = c("Mean1", "Mean2", "SD1", "SD2"),
        Value = c(mean1, mean2, sd1, sd2)
      )
      
      return(data_frame)
    } else {
      return(data.frame())  # Return an empty data frame
    }
  })
  
  
  # Create a data frame with analytical comparisions
  output$comp_table <- renderDT({
    
    data1 <- selected_data1()
    data2 <- selected_data2()
    meandiff <-round(mean(data1,na.rm=t)-mean(data2,na.rm=t),digits=3)
    x <- list(REF=scale(REF), Set1=scale(data1), Set2=scale(data2))
    
    
    comp1 <- data.frame(
      Statistic = c("T-test", "Olap R-D1 ","Olap R-D2 ","Olap D1-D2 ", "Meandiff", "relMeandiff"),
      Value = c(round(as.numeric(t.test(data1,data2)["p.value"]),digits=3), overlap(x,pairsOverlap = TRUE)$OVPairs[[1]], overlap(x,pairsOverlap = TRUE)$OVPairs[[2]],overlap(x,pairsOverlap = TRUE)$OVPairs[[3]],abs(meandiff) ,'6'))
    
    return(comp1)
    
  })
  
  
  
  
  # Moments Summary Table
  output$moments_summary_table <- renderDT({
    datatable(moments_summary(), options = list(scrollY = TRUE))
  })
  
  # Histogram with Max Line
  output$histogram_max_plot <- renderPlotly({
    data1 <- selected_data1()
    data2 <- selected_data2()
    
    max_val1 <- as.numeric(mean(data1))
    max_val2 <- as.numeric(mean(data2))
    #    max_val1 <- as.numeric(density(data1)$x[which.max(density(data1)$y)])
    #    max_val2 <- as.numeric(density(data2)$x[which.max(density(data2)$y)])
    
    p <- ggplot() +
      geom_histogram(data = data.frame(x = data1), aes(x, y = ..density.., fill = "Dataset 1"), alpha = 0.5, bins = 30) +
      geom_histogram(data = data.frame(x = data2), aes(x, y = ..density.., fill = "Dataset 2"), alpha = 0.5, bins = 30) +
      geom_density(data = data.frame(x = data1), aes(x, color = "Dataset 1")) +
      geom_density(data = data.frame(x = data2), aes(x, color = "Dataset 2")) +
      geom_vline(xintercept = max_val1, linetype = "dashed", color = "red") +
      geom_vline(xintercept = max_val2, linetype = "dashed", color = "red") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot based on the moments with 2 distributions using moments summary table
  output$moments_plot <- renderPlotly({
    moments_summary_df <- moments_summary()
    
    if (!is.null(moments_summary_df)) {
      mean1 <- moments_summary_df[moments_summary_df$Statistic == "Mean1", "Value"]
      mean2 <- moments_summary_df[moments_summary_df$Statistic == "Mean2", "Value"]
      sd1 <- moments_summary_df[moments_summary_df$Statistic == "SD1", "Value"]
      sd2 <- moments_summary_df[moments_summary_df$Statistic == "SD2", "Value"]
      
      # Generate data for two distributions with 1000 points each
      data_dist1 <- rnorm(1000, mean = mean1, sd = sd1)
      data_dist2 <- rnorm(1000, mean = mean2, sd = sd2)
      
      # Create a density plot for each distribution
      p <- ggplot() +
        geom_density(data = data.frame(x = data_dist1), aes(x, color = "Dataset 1")) +
        geom_density(data = data.frame(x = data_dist2), aes(x, color = "Dataset 2")) +
        labs(title = "Comparison of Moments (Mean and SD)", y = "Density") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    } else {
      # Return an empty ggplotly plot if there's no data
      ggplotly(ggplot() + theme_void())
    }
  })
  
  # Histogram with median Line
  output$histogram_median_plot <- renderPlotly({
    data1 <- selected_data1()
    data2 <- selected_data2()
    
    max_val1 <- as.numeric(median(data1))
    max_val2 <- as.numeric(median(data2))
    
    p <- ggplot() +
      geom_histogram(data = data.frame(x = data1), aes(x, y = after_stat(density), fill = "Dataset 1"), alpha = 0.5, bins = 30) +
      geom_histogram(data = data.frame(x = data2), aes(x, y =  after_stat(density), fill = "Dataset 2"), alpha = 0.5, bins = 30) +
      geom_density(data = data.frame(x = data1), aes(x, color = "Dataset 1")) +
      geom_density(data = data.frame(x = data2), aes(x, color = "Dataset 2")) +
      geom_vline(xintercept = max_val1, linetype = "dashed", color = "red") +
      geom_vline(xintercept = max_val2, linetype = "dashed", color = "red") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot based on the moments with 2 distributions using moments summary table
  output$overlap_plot <- renderPlotly({
    
    
    Set1 <- selected_data1()
    Set2 <- selected_data2()
    
    x <- list(REF=scale(REF), Set1=scale(Set1), Set2=scale(Set2))
    if (!is.null(x)) {
      
      ggplotly(final.plot(x))
      
    } else {
      # Return an empty ggplotly plot if there's no data
      ggplotly(ggplot() + theme_void())
    }
  })
  
  
}

# Run the app
shinyApp(ui, server)
