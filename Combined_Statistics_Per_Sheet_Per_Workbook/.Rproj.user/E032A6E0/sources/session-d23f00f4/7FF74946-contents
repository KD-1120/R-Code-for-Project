# Load Libraries
library(shiny)
library(tidyverse)
library(readxl)
library(ggtext)
library(grid)
library(purrr)
library(gridExtra)
library(grDevices)
library(svglite)
library(extrafont)
library(stringr)


# Load plot choices for biochemistry and hematology
plotChoices_biochemistry <- c(
  "Glucose",
  "Albumin",
  "AST",
  "Total Protein",
  "GGT",
  "Alkaline Phosphatase",
  "LDH",
  "HB-A1C",
  "Total Bilirubin",
  "Direct Bilirubin",
  "Indirect Bilirubin",
  "Creatinine",
  "Urea",
  "HDL-C",
  "Total Cholesterol",
  "LDL-C",
  "Triglycerides",
  "VLDL-C",
  "Troponin",
  "Uric Acid",
  "Globulin",
  "Sodium",
  "Potassium",
  "Phosphorus",
  "Calcium",
  "Magnesium",
  "Iron", "Chlorine", "BUN", "Creatinine Kinase", "Amylase", "ALT"
)


plotChoices_hematology <- c(
  "WBC", "RBC", "HGB", "HCT (%)", "MCV", "MCH", "MCHC", 
  "MPV", "RDW %", "RDW Count", "Platelets %", "Platelets Count", 
  "Neutrophils %", "Neutrophils Count", "Lymphocytes %", 
  "Lymphocytes Count", "Monocytes %", "Monocytes Count", 
  "Basophils %", "Basophils Count", "Eosinophils %", 
  "Eosinophils Count", "PDW", "PCT", "RCDW %", 
  "RCDW Count", "CD4+ Count", "CD4+ %", "CD8+ Count", "CD8+ %"
)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Reference Interval Graph Plotter"),
  helpText("This Shiny app takes in Excel data in the specified format and returns a downloadable plot in .svg format."),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("plotData", label = "Choose Data File", multiple = FALSE,
                accept = c(".csv", ".xls", ".xlsx", "text/csv", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      radioButtons("category", label = "Select Data Category", choices = c("Biochemistry", "Hematology"), selected = "Biochemistry"),
      uiOutput("selectAnalyte"),
      radioButtons("age", label = "Is this data Adult (>18) or Pediatrics (<18)", choices = c("<", ">")),
      textInput("units", label = "Type your units here", value = "mmol/L"),
      radioButtons("plotType", label = "Select the plot to display", choices = c("Male", "Female")),
      sliderInput("height", "height", min = 400, max = 1400, value = 700),
      sliderInput("width", "width", min = 400, max = 1400, value = 900)
    ),
    mainPanel(
      downloadButton("downloadPlot", "Download Plot as SVG"),
      plotOutput("plot"),
      
      #tableOutput("value2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$selectAnalyte <- renderUI({
    if (input$category == "Biochemistry") {
      selectInput("analyte", label = "Select the Analyte data for the plot", choices = plotChoices_biochemistry)
    } else if (input$category == "Hematology") {
      selectInput("analyte", label = "Select the Analyte data for the plot", choices = plotChoices_hematology)
    }
  })
  
  plottingData <- reactive({
    req(input$plotData)
    readxl::read_excel(input$plotData$datapath, sheet = input$analyte, range = cell_rows(1:100)) %>%
      drop_na() %>%
      mutate(
        AuthorList = str_split(Author, " et al\\.") %>% map_chr(~ paste(.x, collapse = "")),
        Number = row_number(),
        Author = paste0(AuthorList, "-", Number)
      )
  })
  
  
  sort_data <- function(data, author, ll, ul, gender) {
    data <- data %>%
      select(Author = {{author}},
             L.L = {{ll}},
             U.L = {{ul}},
             gender = {{gender}},
             AuthorList = AuthorList) %>%
      mutate(
        gender = if_else({{ gender }} == 1, "Male", "Female")
      )
    
    return(data)
  }
  
  
  
  
  
  # Combine sorted data
  combined_data <- reactive({
    bind_rows(male_data <- sort_data(plottingData(), Author, L_L, U_L, gender) %>%
                filter(gender == "Male"),
              female_data <- sort_data(plottingData(), Author, L_L, U_L, gender) %>%
                filter(gender == "Female")) %>%
      drop_na()
  })
  
  # Process combined data
  all_data <- reactive({ 
    data <- combined_data() %>%
      mutate(diff = round(U.L - L.L, 2)) %>%
      filter(diff > 0) %>%
      mutate(x_pos = L.L + (diff / 2))
    
    # Create a new variable to store the order for reordering the plots
    data <- data %>%
      mutate(ord = nrow(data):1)
    
    data
  })
  
  
  #Stats calculation
  stats <- reactive({ all_data() %>%
      pivot_longer(cols = c(L.L, U.L), names_to = "hb", values_to = "value") %>%
      group_by(gender,hb) %>%
      summarise(mean = mean(value), SD = sd(value)) %>%
      mutate(meanpos = mean + 1 * SD, meanneg = mean - 1 * SD)
  })
  
  #Filters and stores mean and standard deviation for each group in a specific variable.
  stats_male <- reactive({
    stats() %>%
      filter(gender == "Male")
  })
  
  stats_female <- reactive({
    stats() %>%
      filter(gender == "Female")
  })
  
  
  # Find mean and standard deviation for annotation
  find_mean_sd <- function(gender) {
    L_L_data <- gender %>%
      filter(hb == "L.L")
    U_L_data <- gender %>%
      filter(hb == "U.L")
    
    # Extract mean and sd for L.L
    mean_L.L <- round(L_L_data$mean, 2)
    sd_L.L <- round(L_L_data$SD, 2)
    
    # Extract mean and sd for U.L
    mean_U.L <- round(U_L_data$mean, 2)
    sd_U.L <- round(U_L_data$SD, 2)
    
    # Create a data frame to store the results
    results <- data.frame(
      A = c("LRL", "URL"),
      Mean = c(mean_L.L, mean_U.L),
      SD = c(sd_L.L, sd_U.L)
    )
    colnames(results)<- c(" ", "Mean", "SD")
    return(results)
  }
  
  male_mean_sd <- reactive({find_mean_sd(stats_male())})
  female_mean_sd <- reactive({find_mean_sd(stats_female())})
  
  
  # Separate data by gender and age_group for plotting
  Male <- reactive({all_data() |> filter(gender == "Male")})
  Female <- reactive({all_data() |> filter(gender == "Female")})
  
  
  #Generate plot
  generate_plot <- function(data, gender_stats, first_analyzer, last_analyzer, annotation,title, gender, sign, unit) {
    data$Author <- as.factor(data$Author)
    
    ggplot(data) +
      geom_rect(aes(xmin = gender_stats %>%
                      filter(hb == "U.L") %>%
                      pull(meanneg),
                    xmax = gender_stats %>%
                      filter(hb == "U.L") %>%
                      pull(meanpos),
                    ymin = -Inf, ymax = Inf,
                    group = interaction(gender)),
                fill = "#00BFC4", alpha = 0.05) +
      geom_vline(data = gender_stats %>% filter(hb == "U.L"),
                 aes(xintercept = mean),
                 color = "#762a83", linetype = "dashed", size = 0.5, alpha = 0.5) +
      geom_rect(aes(xmin = gender_stats %>%
                      filter(hb == "L.L") %>%
                      pull(meanneg),
                    xmax = gender_stats %>%
                      filter(hb == "L.L") %>%
                      pull(meanpos),
                    ymin = -Inf, ymax = Inf,
                    group = interaction(gender)),
                fill = "#F8766D", alpha = 0.05) +
      geom_vline(data = gender_stats %>% filter(hb == "L.L"),
                 aes(xintercept = mean),
                 color = "#762a83", linetype = "dashed", size = 0.5, alpha = 0.5) +
      geom_segment(aes(y = reorder(Author, ord),
                       x = L.L,
                       yend = reorder(Author, ord),
                       xend = U.L,
                       group = Author),
                   color = "#000000", size = 4.5, alpha = 0.8) +
      geom_point(aes(x = L.L, y = reorder(Author, ord), color = "#F8766D"), size = 4.5, show.legend = FALSE) +
      geom_point(aes(x = U.L, y = reorder(Author, ord), color = "#00BFC4"), size = 4.5, show.legend = FALSE) +
      scale_color_manual(values = c("#00BFC4", "#F8766D")) +
      annotation_custom(
        grob = tableGrob(annotation, rows = NULL),
        xmin = max(data$U.L) * 1.1,
        xmax = max(data$U.L) * 1.2,
        ymin = data %>%
          slice_max(order_by = Author, n = 5)%>%  # Take the first (i.e., last) value
          pull(Author) %>%
          tail(n = 1),
        ymax = data %>%
          slice_max(order_by = Author, n = 6) %>%  # Take the first (i.e., last) value
          pull(Author) %>%
          tail(n = 1)
      ) +
      geom_text(data = data,
                aes(label = paste("D: ", diff), x = x_pos, y = Author),
                color = "white", size = 2.5, family = "Segoe UI Semibold") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
        strip.text.y.left = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 12, hjust = 0.3),
        plot.title = element_markdown(size = 16, hjust = 0.3)
      ) +
      labs(
        title = paste0("<span style = 'color: #F8766D;'>**Variations in Reference Intervals**</span> <br/> ",title," (", gender, " ", sign, " 18)"),
        x = paste("Reference Intervals from Different Publications (",unit,")"),
        y = "Author",
        subtitle = paste(
          data %>% pull(L.L) %>% min(),
          " ≤ LRL ≤ ",
          data %>% pull(L.L) %>% max(),
          ",",
          data %>% pull(U.L) %>% min(),
          " ≤ URL ≤ ",
          data %>% pull(U.L) %>% max(),
          ",",
          data %>% pull(diff) %>% min(),
          " ≤ δ ≤ ",
          data %>% pull(diff) %>% max()
        )
      ) +
      scale_y_discrete(labels = rev(as.factor(data$AuthorList))) +
      coord_cartesian(xlim = c(min(data$L.L), max(data$U.L) * 1.3))-> graph_styled
    
    return(graph_styled)
  }
  
  plot_Male <- reactive({
    generate_plot(Male(), stats_male(),"AA", "AI", male_mean_sd(),input$analyte, "Male", input$age, input$units)
  })
  plot_Female <- reactive({
    generate_plot(Female(), stats_female(),"AA", "AI", female_mean_sd(),input$analyte, "Female", input$age, input$units)
  })
  
  
  # Render the selected plot based on radio button input
  output$plot <- renderPlot({
    plot_type <- input$plotType
    
    if (plot_type == "Male") {
      plot_Male()
    } else if (plot_type == "Female") {
      plot_Female()
    } 
  }, 
  width = function() input$width,
  height = function() input$height,
  res = 96)
  
  output$value2 <- renderTable({
    Female()
  })
  
  # save plot
  
  # save plot
  generate_and_save_plot <- function(plot_type, analyte, width, height, file_path) {
    plot_data <- switch(plot_type,
                        "Male" = plot_Male(),
                        "Female" = plot_Female()
    )
    
    ggsave(
      file = file_path,
      plot = plot_data,
      width = width / 96, # Convert pixels to inches
      height = height / 96, # Convert pixels to inches
      device = "svg"
    )
  }
  
  #handle download
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$plotType, "_", input$analyte, ".svg")
    },
    content = function(file) {
      generate_and_save_plot(input$plotType, input$analyte, input$width, input$height, file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


