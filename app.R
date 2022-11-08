library(shiny)
library(shinydashboard)
library(ggplot2)
library(GGally)

db_header <- dashboardHeader(
    title = "EDA"
)

db_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Old Faithful Geyser Data", tabName = "tab_faithful", icon = icon("dashboard")),
        menuItem("Edgar Anderson's Iris Data", tabName = "tab_iris", icon = icon("dashboard"))
    )
)

tab_iris <- tabItem(
    tabName = "tab_iris",
    fluidRow(
        h2("Correlations between Continuous Variables"),
        box(plotOutput("numeric_plots_iris"))
    )
)

tab_faithful <- tabItem(
    tabName = "tab_faithful",
    fluidRow(
        h2("Distribution of Waiting Times between Eruptions"),
        box(plotOutput("hist_faithful"), width = 200),
        box(
            sliderInput(
                "n",
                "Number of bins",
                10,
                100,
                value = 30,
                step = 5
                )
        )
        )
    )

db_body <- dashboardBody(
    tabItems(
        tab_iris,
        tab_faithful
    )
)

# Define UI 
ui <- dashboardPage(
    db_header, db_sidebar, db_body
)

# Define server logic 
server <- function(input, output) {

    output$numeric_plots_iris <- renderPlot({
        iris |> dplyr::select_if(is.numeric) |>
            GGally::ggpairs()
    })
    
    output$hist_faithful <- renderCachedPlot({
        Sys.sleep(2)
        faithful |> ggplot(aes(x=waiting)) + 
            geom_histogram(bins = input$n) + 
            ggtitle(paste0(
                "The Old Faithful has been faithfully erupting every ", 
                round(mean(faithful$waiting), 2), 
                " minutes on average."
                )
                ) + 
            geom_vline(xintercept = mean(faithful$waiting), color = 'red')
    },
    cacheKeyExpr = {
        list(input$n)
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
