loginUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("login_ui"))
  
}

login <- function(input, output, session, user_base, log_out) {
  
  credentials <- reactiveValues(user_auth = FALSE,
                                permission = NULL)
  
  observe({
    if(log_out() > 0) {
      credentials$user_auth <- FALSE
      credentials$permission <- NULL
    }
  })
  
  output$login_ui <- renderUI({
    
    if(credentials$user_auth == TRUE) return(NULL)
    
    fluidPage(
      div(style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
          wellPanel(
            h2("Please log in", class = "text-center", style = "padding-top: 0;"),
            
            tags$div(class = "text-center",
                     tags$p("test the different outputs from the sample logins below")
            ), 
            
            textInput(session$ns("user_name"), "User Name:"),
            
            passwordInput(session$ns("password"), "Password:"),
            
            div(
              style="text-align: center;",
              actionButton(session$ns("login_button"), "Log in", class = "btn-primary", style = "color: white;")
            ),
            
            uiOutput(session$ns("login_error"))
          ),
          
          renderTable({ user_base })
      )
    )
    
  })
  
  observeEvent(input$login_button, {
    
    user_base <- mutate(user_base,  password = map_chr(password, digest))
    
    row_username <- which(user_base$user == input$user_name)
    row_password <- which(user_base$password == digest(input$password)) # digest() makes md5 hash of password
    
    # if user name row and password name row are same, credentials are valid
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      credentials$user_auth <- TRUE
      credentials$permission <- user_base$permissions[row_username]
    } else {
      output$login_error <- renderUI({
        p("Invalid username or password!", style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")
      })
    }
    
  })
  
  return(reactive(reactiveValuesToList(credentials)))
  
}