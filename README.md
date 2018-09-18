# shiny_auth

This draws on code from [shiny_password](https://github.com/treysp/shiny_password) with adaptations to work within the [shinydashboard](https://github.com/rstudio/shinydashboard) framework.

The app allows you to pass a `user_base` dataframe with `user` `password` `permissions` fields to an authentication module. If login criteria is met, the module will pass a `user_info` list back to the main app from which you can dynamically load and serve different data and UI components based on the assigned permissions of the logged in user.