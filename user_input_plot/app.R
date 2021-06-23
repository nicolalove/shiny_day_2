
library(shiny)
library(tidyverse)
library(emojifont)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(paste0("Volcanoes", emojifont::emoji("volcano"))),
            plotOutput("count_plot", click = "plot_click"),
            tableOutput("clicked_data"),
            plotOutput("one_plot")
    
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    # here makes paths correct very easily 
    volcano <- readRDS(here::here("data", "volcanoes.rds"))
    
    volcano_counts <- volcano %>% 
        group_by(volcano_type_consolidated) %>%
        tally() %>%
        mutate(volcano_type_consolidated = 
                forcats::fct_reorder(volcano_type_consolidated, desc(n)))
    output$count_plot <- renderPlot(ggplot(volcano_counts, 
                        aes(x = volcano_type_consolidated, y = n)) +
                        geom_point(size = 4))
    
    observeEvent(input$plot_click, {cat("you clicked!")})
    output$clicked_data <- renderTable({
        req(input$plot_click) # require that user clicked the plot to proceed
        #browser()
        nearPoints(volcano_counts, coordinfo = input$plot_click) 
        # give me datapoint closest to where I clicked. 
    }) 
    plot_one_volcano_type <- function(volcano_type_df){
        chosen_type <- volcano_type_df$volcano_type_consolidated
        volcano %>% 
            filter(volcano_type_consolidated == chosen_type) %>% 
            drop_na(last_eruption_year) %>% 
            group_by(volcano_type_consolidated) %>% 
            arrange(last_eruption_year) %>% 
            mutate(n_erupt = cumsum(!is.na(last_eruption_year))) %>% 
            ggplot(aes(x = last_eruption_year, y = n_erupt)) + geom_step() + 
            labs(title = paste0("Cumulative explosions of ", chosen_type, " Volcanoes"),
                 x = "Years since eruption", y = "Eruptions") + 
            theme_bw()
    }
    output$one_plot <- renderPlot({
        req(input$plot_click)
        plot_one_volcano_type(nearPoints(volcano_counts, coordinfo = input$plot_click))
    })
    
    
    
    
    plot_one_volcano_type(volcano_type_df = data.frame(volcano_type_consolidated = "Cone"))
}

# Run the application 
shinyApp(ui = ui, server = server)
