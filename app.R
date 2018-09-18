library(shiny)
library(shinydashboard)
library(tidyverse)
library(digest)
library(shinyjs)
library(DT)
library(glue)

source("auth_module.R")

user_base <- tibble::tibble(
    user = c("admin", "user"),
    password = c("admin_pass", "user_pass"), 
    permissions = c("admin", "standard")
)

ui <- dashboardPage(
    dashboardHeader(title = "Shiny Auth", uiOutput("logout_button")),
    
    dashboardSidebar(collapsed = TRUE, sidebarMenuOutput("sidebar")),
    
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(tags$style(".table{margin: 0 auto;}")),
        loginUI("login"),
        tabItems(
            tabItem("admin_view", uiOutput("admin")),
            tabItem("standard_view", uiOutput("standard"))
        )
    )
)

server <- function(input, output, session) {
    
    rv <- reactiveValues(logout_init = 0)
    
    user_info <- callModule(login, "login", 
                            user_base = user_base, 
                            log_out = reactive(rv$logout_init))
    
    output$logout_button <- renderUI({
        if(user_info()$user_auth) {
            div(style = "padding: 8px;",
                tags$li(actionButton("logout", "Log out", class = "btn-danger", style = "color: white;"), class = "dropdown")
            )
        } else {
            return(NULL)
        }
    })
    
    observeEvent(input$logout, {
        rv$logout_init <- rv$logout_init + 1
    })
    
    observe({
        if(user_info()$user_auth) {
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        } else if (!isTruthy(user_info()$user_auth)) {
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        }
    })
    
    output$sidebar <- renderMenu({
        req(user_info()$user_auth)
        
        if(user_info()$permission == "admin") {
            sidebarMenu(id = "tabs",
                        menuItem("Admin View", tabName = "admin_view", icon = icon("unlock")),
                        menuItem("User View", icon = icon("users"), tabName = "standard_view")
            )
            
        } else {
            sidebarMenu(id = "tabs",
                        menuItem("Standard View", icon = icon("users"), tabName = "standard_view")
            )
        }
        
    })
    
    output$standard <- renderUI({
        req(user_info()$user_auth)
        
        tagList(
            h2("Standard Data: iris"),
            DT::dataTableOutput("standard_table")
        )
    })
    
    output$admin <- renderUI({
        req(user_info()$permission == "admin")
        
        tagList(
            h2("Admin Data: mtcars"),
            DT::dataTableOutput("admin_table")
        )
    })
    
    admin_data <- reactive({
        req(user_info()$permission == "admin")
        
        mtcars
    })
    
    output$admin_table <- DT::renderDataTable({
        req(admin_data())
        
        admin_data()
    })
    
    standard_data <- reactive({
        req(user_info()$user_auth)
        
        iris
    })
    
    output$standard_table <- DT::renderDataTable({
        req(standard_data())
        
        standard_data()
    })
    
}

shiny::shinyApp(ui, server)
