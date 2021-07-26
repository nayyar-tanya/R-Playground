if(!require(shiny))           install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyjs))         install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(odbc))            install.packages("odbc", repos = "http://cran.us.r-project.org")
if(!require(DBI))             install.packages("DBI", repos = "http://cran.us.r-project.org")
if(!require(plotly))          install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))         install.packages("ggplot2",version = "3.2.1", repos = "http://cran.us.r-project.org")
if(!require(sodium))          install.packages("sodium", repos = "http://cran.us.r-project.org")
if(!require(janitor))         install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(tidyverse))       install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(DT))              install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(dplyr))           install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tibble))          install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(shinythemes))     install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets))    install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(writexl))         install.packages("writexl", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard))  install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(plyr))            install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(sendmailR))            install.packages("sendmailR", repos = "http://cran.us.r-project.org")
if(!require(pool))            install.packages("pool", repos = "http://cran.us.r-project.org")
if(!require(uuid))            install.packages("uuid", repos = "http://cran.us.r-project.org")
if(!require(reshape2))            install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(scales))            install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(reactable))            install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(ggQC))            install.packages("ggQC", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))            install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate))            install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(shiny)
library(shinydashboard)
library(DT)
library(DBI)
library(shinyjs)
library(sodium)
library(shinyWidgets)
library(odbc)
library(shinythemes)
library(dplyr)
library(tibble)
library(writexl)
library(plyr)
library(sendmailR)
library(pool)
library(uuid)
library(reshape2)
library(scales)
library(pool)
library(uuid)
library(reactable)
library(ggQC)
library(ggplot2)
library(lubridate)

##connect to database
if(Sys.info()['sysname'] == 'Windows'){
    
    
    con <- dbConnect(odbc(),
                     driver = '{driver}',
                     server = '{server-name}',
                     database = '{db_name}',
                     trusted_connection = TRUE
    )
    
    
}else if(Sys.info()['sysname'] == '{other}'){
    con <- dbConnect(odbc(),
                     driver = '{driver}',
                     dsn = '{dsn-server-name}',
                     uid = Sys.getenv(x = '{uid}'),
                     pwd = Sys.getenv(x = '{u_password}')
    )
}

##getting data from various tables
## format example
sample_data<- dbGetQuery(con,
                       "
SELECT * FROM [db].[schema].[tablename]")
#table 1 to save form data
table <- "table 1"
#table 2 to save ui data
secondtable <- "table 2"

org_levels<- dbGetQuery(con,
                      "
SELECT * FROM table")

##the above example can be used as a reference to get data from various
##tables by writing respective dbGetQuery statements

# Define the fields we want to save from the form
field <- c('User_Name','Options_Type','Log_Date','Level_One','Level_Two','Level_Three','Num_Units','Num_People')
sfields <- c('field1','field2','field3','field4','field5','field6')

##write functions for saving, loading, deleting, and updating the 
##data sample example is as follows

saveData <- function(data) {
    tryCatch(
        expr = {
            # Construct the insert query by attaching the data fields info
            query <- sprintf(
                "INSERT INTO %s (%s) VALUES ('%s')",
                table,
                paste(names(data), collapse = ", "),
                paste(data, collapse = "', '")
            )
            # Submit the query and disconnect
            dbGetQuery(con, query)
            dbDisconnect(con)
        },
        error = function(e){
            message('Caught an error!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp server'
                    
                )
                
            )
            
            stop()
        },
        warning = function(w){
            message('Caught a warning!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp serverr'
                    
                )
                
            )
            stop()
        }
    )
    
}




savecalData <- function(data) {
    tryCatch(
        expr = {

        # Construct the insert query by looping over the data fields
        for(i in 1:nrow(data))
        {   
            query <- sprintf(
                "INSERT INTO %s (%s) VALUES ('%s')",
                secondtable,
                paste(names(data), collapse = ", "),
                paste(data[i,], collapse = "', '")
            )
            print(query) 
            dbGetQuery(con, query)
        }
        dbDisconnect(con)
        },
    error = function(e){
        message('Caught an error!')
        sendmail(
            
            from='from email'
            
            , to = 'to email'
            
            , subject = 'Error from Log Tool'
            
            , msg = w
            
            , control = list(
                
                smtpServer='smtp server'
                
            )
            
        )
        
        stop()
    },
    warning = function(w){
        message('Caught a warning!')
        sendmail(
            
            from='from email'
            
            , to = 'to email'
            
            , subject = 'Error from Log Tool'
            
            , msg = w
            
            , control = list(
                
                smtpServer='smtp serverr'
                
            )
            
        )
    stop()
    }
    )
    
}




loadData <- function(loguser,s_date) {
    tryCatch(
        expr = {
            # Construct the fetching query
            query <- sprintf("SELECT * FROM %s WHERE <<<<<<<conditions>>>>>>>>>",
                             secondtable,
                             loguser,
                             s_date)
            print(query)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(con, query)
            dbDisconnect(con)
            data
        },
        error = function(e){
            message('Caught an error!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp server'
                    
                )
                
            )
            
            stop()
        },
        warning = function(w){
            message('Caught a warning!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp serverr'
                    
                )
                
            )
            stop()
        }
    )
    
}

updateData <- function(datavalue1,colname,datavalue2, datavalue3) {
    #try catch block
    tryCatch(
        expr = {
            # Construct the insert query by looping over the data fields
            query2 <- sprintf(
                "UPDATE %s SET %s = '%s' WHERE <<<<<condition>>>>>>> ",
                secondtable, 
                colname,
                datavalue1,
                datavalue2,
                datavalue3
            )
            # Submit the insert query and disconnect
            dbGetQuery(con, query2)
            dbDisconnect(con)
        },
        error = function(e){
            message('Caught an error!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp server'
                    
                )
                
            )
            
            stop()
        },
        warning = function(w){
            message('Caught a warning!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp serverr'
                    
                )
                
            )
            stop()
        }
    )
}

deleteselecteddata <- function(datavalue1,datavalue2){
    #try catch block
    tryCatch(
        expr = {
            # Construct the delete query
            query <- sprintf(
                "DELETE FROM %s WHERE <<<<<Condition1>>>>> = '%s' AND <<<<<Condition2>>>>> ='%s'",
                secondtable, 
                datavalue1,
                datavalue2
            )
            # Submit the delete query and disconnect
            dbGetQuery(con, query)
            dbDisconnect(con)
        },
        error = function(e){
            message('Caught an error!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp server'
                    
                )
                
            )
            
            stop()
        },
        warning = function(w){
            message('Caught a warning!')
            sendmail(
                
                from='from email'
                
                , to = 'to email'
                
                , subject = 'Error from Log Tool'
                
                , msg = w
                
                , control = list(
                    
                    smtpServer='smtp serverr'
                    
                )
                
            )
            stop()
        }
    )
    
    
}

entry_form <- function(button_id){
    
    showModal(
        modalDialog(
            div(id=("entry_form"),
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                pickerInput(
                    inputId = 'iid',
                    label = 'iilabel',
                    choices = c('c1','c2'),
                    selected = list('c1'),
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE)),
                actionButton(button_id, "Submit"),
                easyClose = TRUE
                
            )
        ),session$ns
    )
}

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("Log In", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         br()
                     ))
)

credentials = data.frame(
    username_id = c("User1","User2","User3","User4"),
    passod   = sapply(c("Password1","Password2","Password3","Password4"),password_store),
    stringsAsFactors = F
)

##### Module
myeditFunction <- function(input, output, session, data,submit,loguser,sdrange,editbutton,datadelete, emp_name) {
    
    ns <- session$ns
    r_values <- reactiveValues(data = data)
    
    proxy = dataTableProxy("proxy_table")
    
    observeEvent(input$proxy_table_cell_edit, {
        #try catch block
        tryCatch(
            expr = {
                print(names(r_values$data))
                info = input$proxy_table_cell_edit
                str(info)
                i = info$row
                j = info$col
                k = info$value
                str(info)
                isolate(
                    if (j %in% match(c("field1","field3","field6"), names(r_values$data))) 
                    {
                        r_values$data[i, j] <<- DT::coerceValue(k, r_values$data[i, j])
                        print(i)
                        print(j)
                        print(r_values$data[i, j])
                        if (j %in% match("field1", names(r_values$data))) {
                            ##field 7 is the id field (P.K.) in the db table hence using that in combination 
                            ##with field 1 to update the data as needed
                            updateData(r_values$data[i, j], "field1",r_values$data[i,7],r_values$data[i,1])
                        }
                        if (j %in% match("field3", names(r_values$data))) {
                            updateData(r_values$data[i, j], "field3",r_values$data[i,7],r_values$data[i,1])
                        }
                        if (j %in% match("field6", names(r_values$data))) {
                            r_values$data[i, j] <- gsub("\'","\"", r_values$data[i, j])
                            updateData(r_values$data[i, j], "field6",r_values$data[i,7],r_values$data[i,1])
                        }
                        
                        print(r_values$data)
                        
                    } else {
                        tags$code("You are not supposed to change this column.")
                    }
                )
                replaceData(proxy, r_values$data, resetPaging = FALSE)  # replaces data displayed by the updated table
            },
            error = function(e){
                message('Caught an error!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp server'
                        
                    )
                    
                )
                
                stop()
            },
            warning = function(w){
                message('Caught a warning!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp serverr'
                        
                    )
                    
                )
                stop()
            }
        )
    })
    
    rowsel = reactive(input$proxy_table_rows_selected)
    
    observeEvent(editbutton(), priority = 20,{
        if(is.null(rowsel()))
        {
            showModal(
                modalDialog(
                    title = "Warning",
                    paste("Please select a row." ),easyClose = TRUE)
            ) 
            
        }
        
        
        else{
            
            showModal(settngsModal(session$ns))
            
        }
        
    })
    
    ##filter employee records
    observeEvent(emp_name(), {
        r_values$data <- loadData(emp_name(), sdrange())
    })
    
    observeEvent(sdrange(),{
        r_values$data <- loadData(emp_name(), sdrange())
        
    })
    
    last_row = reactive(input$proxy_table_row_last_clicked)
    
    observeEvent(input$modalApply,{
        if(!(input$downtimenone==""))
        {
            updateData(input$downtimenone, "field4",r_values$data[last_row(),7:7],r_values$data[last_row(),1:1])
        }
        if(!(input$downreasonone==""))
        {
            updateData(input$downreasonone, "field5",r_values$data[last_row(), 7:7],r_values$data[last_row(),1:1])
        }
        
        r_values$data <- loadData(emp_name(),sdrange())
        
    })
    
    delrowsel = reactive(input$proxy_table_rows_selected)
    #####Functionality added for deleting a particular row
    observeEvent(datadelete(), {
        #try catch block
        tryCatch(
            expr = {
                if(is.null(delrowsel()))
                {
                    shiny::showNotification("Please select a row to delete", type = "warning", duration = NULL);
                }
                else
                {
                    for(val in delrowsel())
                    {
                        deleteselecteddata(r_values$data[val,7:7], r_values$data[val,1:1])
                    }
                    
                    r_values$data <- loadData(emp_name(),sdrange())
                }
                
            },
            error = function(e){
                message('Caught an error!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp server'
                        
                    )
                    
                )
                
                stop()
            },
            warning = function(w){
                message('Caught a warning!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp serverr'
                        
                    )
                    
                )
                stop()
            }
        )
    })
    
    observeEvent(submit(), {
        #try catch block
        tryCatch(
            expr = {
                
                r_values$data <- loadData(emp_name(), format(as.Date(Sys.Date()),"%Y-%m-%d"))
                
            },
            error = function(e){
                message('Caught an error!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp server'
                        
                    )
                    
                )
                
                stop()
            },
            warning = function(w){
                message('Caught a warning!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp serverr'
                        
                    )
                    
                )
                stop()
            }
        )
    })
    
    
    print(isolate(colnames(r_values$data)))
    output$proxy_table <- DT::renderDataTable({
        
        DT::datatable(r_values$data,editable = list(target = 'cell', disable = list(columns = c(0,4,5))), selection = "multiple"
                      ,options=list(columnDefs = list(list(visible=FALSE, targets=c(7),width = '200px', targets = "_all"))
                                    ,language = list(emptyTable = 'No Data for selected range')
                                    , lengthMenu = c(15,25,50,100),pageLength = 50,ordering=F
                      ))
        
    })
    
}
myeditFunctionUI <- function(id) {
    ns <- NS(id)
    DT::dataTableOutput(ns("proxy_table"))
    
}
settngsModal <- function(ns) {
    modalDialog(
        h5(p(strong("Please enter time in minutes. For example: Please enter 15 minutes as 15, 30 minutes as 30, and 1 hour as 60"))),
        textInput(ns("downtimenone"), "Downtime One", ""),
        pickerInput(
            ns("downreasonone"),
            label = 'Downtime One Reason',
            choices = c(''
                       , 'Reason - 1'
                       , 'Reason - 2'
                       , 'Reason - 3'
                       , 'Reason - 4'
                       , 'Reason - 5'
            ),
            selected = c(''),
            multiple = FALSE,
            options = list(`actions-box` = TRUE)),
        modalButton("Cancel", icon("remove")),
        actionButton(ns("modalApply"), "Apply", icon = icon("check")),
        
        #  )
        #}),
        title = "Enter time and Reason",
        size = "l",
        easyClose = TRUE,
        fade = TRUE)
}

# Define UI for application
ui <- fluidPage(
    br(), br(),
    img(src="<<<<source address for image logo>>>>", align = "left",height='200px',width='300px'),br(),
    h1(strong("              My Tool"),style = "font-size:50px;"),
    tags$head(tags$style(HTML(".rightAlign{float:right;}")),
              fluidRow(uiOutput("logoutbtn"))),
    shinyjs::useShinyjs(), uiOutput("body"),
    tags$head(
        tags$style(
            HTML(".shiny-notification {
         position:fixed;
         top: calc(50%);;
         left: calc(50%);;
         width: 425px ;;
         font-size: 24px ;;
         }
         "
            )
        )
    )
)

# Define server logic required
server <- function(input, output) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;list-style-type: none;",align = "right")
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            if(input$userName %in% c("User1","User2","User3","User4"))
            {
                tabsetPanel(type = "pills",
                            tabPanel("Tracking", fluid = TRUE,
                                     br(),
                                     br(),
                                     sidebarPanel(
                                         br(),
                                         hr(style = "border-color: #606060;"),
                                         div(style="display:inline-block",downloadButton("Export", "Export data as .xlsx",class = "butt1"), style="float:center"),
                                         # style font family as well in addition to background and font color
                                         tags$head(tags$style(".butt1{background-color:black;} .butt1{color: white;} .butt1{font-family:italic}")),
                                         hr(style = "border-color: #606060;"),
                                         textInput("User_Name", "User Name",input$userName)%>% disabled(),
                                         chooseoptions <-  pickerInput(
                                             inputId = 'Options_Type',
                                             label = 'Options',
                                             choices = c('Option 1','Option 2','Option 3','Option 4'),
                                             selected = list('Option 1'),
                                             multiple = FALSE,
                                             options = list(`actions-box` = TRUE)),
                                         dateInput("Log_Date", "Log Date",
                                                   value = Sys.Date(),
                                                   min = Sys.Date()-56, max = Sys.Date()+56, format = "yyyy-mm-dd", language="en"),
                                         htmlOutput("levelone_type_selector"),#add selectinput boxs
                                         htmlOutput("leveltwo_type_selector"),# from objects created in server
                                         htmlOutput("levelthree_type_selector"),#from objects created in server
                                         numericInput("Num_Units", "Number of Units",0),
                                         numericInput("Num_People", "Number of People",0),
                                         actionButton("submit", "Submit"),width=3),
                                     fluidRow(box(width = 8,
                                                  fixedRow(column(12,
                                                                  fixedRow(
                                                                      column(3,chooseusers <- pickerInput(
                                                                          inputId = 'usr_name',
                                                                          label = 'Filter User Data',
                                                                          choices = c('User 1',
                                                                                      'User 2',
                                                                                      'User 3',
                                                                                      'User 4'),
                                                                          selected = input$userName,
                                                                          multiple = FALSE,
                                                                          options = list(`actions-box` = TRUE))),
                                                                      column(3, dateInput("datesel", "Live Date",
                                                                                          value = Sys.Date(),
                                                                                          min = Sys.Date()-360, max = Sys.Date()+300, format = "yyyy-mm-dd", language="en")),
                                                                      column(6,  
                                                                             br(),
                                                                             actionButton("edit_button", "Add downtime data", icon("edit")),
                                                                             actionButton("datadelete", "Delete Row")
                                                                      )
                                                                  ))),
                                                  
                                                  myeditFunctionUI("editable")
                                     )
                                     )
                            )
                )
            }
        }
            else {
                loginpage
            }
            
        })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        firstdata <- sapply(fields[1:2],function(x) input[[x]])
        seconddata <- sapply(fields[3], function(x) format(as.Date(input[[x]]),"%Y-%m-%d"))
        thirddata <- sapply(fields[4:8], function(x) input[[x]])
        data <- c(firstdata,seconddata,thirddata)
        print(data)
        data
    })
    
    # # # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
                saveData(formData())
                planned_data <- formData()
                req_units <- (as.numeric(getElement(planned_data, "Num_Units")))
                
                if(getElement(planned_data, "Options_Type") %in% c("Option 1" ))
                {
                    df = data.frame(field1 = c(getElement(planned_data, "User_Name")),
                                    field2 = c(getElement(planned_data, "Log_Date")),
                                    field3 = c(getElement(planned_data, "Num_Units")),
                                    field4 = c(''),
                                    field5 = c(''),
                                    field6 = c(getElement(planned_data, "Options_Type")),
                                    stringsAsFactors = FALSE
                    )
                    savecalData(df)
                    
                } else if(getElement(planned_data, "Options_Type") %in% c("Option 2" )){
                    df = data.frame(field1 = c(getElement(planned_data, "User_Name")),
                                    field2 = c(getElement(planned_data, "Log_Date")),
                                    field3 = c(getElement(planned_data, "Num_Units")),
                                    field4 = c(''),
                                    field5 = c(''),
                                    field6 = c(getElement(planned_data, "Options_Type")),
                                    stringsAsFactors = FALSE
                    )
                    savecalData(df)
                    
                }else if(getElement(planned_data, "Options_Type") %in% c("Option 3" )){
                    df = data.frame(field1 = c(getElement(planned_data, "User_Name")),
                                    field2 = c(getElement(planned_data, "Log_Date")),
                                    field3 = c(getElement(planned_data, "Num_Units")),
                                    field4 = c(''),
                                    field5 = c(''),
                                    field6 = c(getElement(planned_data, "Options_Type")),
                                    stringsAsFactors = FALSE
                    )
                    savecalData(df)
                    
                }else{
                    df = data.frame(field1 = c(getElement(planned_data, "User_Name")),
                                    field2 = c(getElement(planned_data, "Log_Date")),
                                    field3 = c(getElement(planned_data, "Num_Units")),
                                    field4 = c(''),
                                    field5 = c(''),
                                    field6 = c(getElement(planned_data, "Options_Type")),
                                    stringsAsFactors = FALSE
                    )
                    
                    savecalData(df)
                }
    })
    
    start_range <- reactive({
        format(as.Date(input$datesel),"%Y-%m-%d")
    })
    
    observe({
        tryCatch(
            expr = {
                if (USER$login == TRUE)
                {
                    if(input$userName %in% c("User1","User2","User3","User4"))
                    {
                        demodata<-loadData("User1','User2','User3','User4"
                                                   ,format(as.Date(Sys.Date()),"%Y-%m-%d"))
                        
                        callModule(myeditFunction,"editable", demodata,
                                   submit = reactive(input$submit)
                                   ,loguser = reactive(input$userName)
                                   ,sdrange = reactive(format(as.Date(input$datesel),"%Y-%m-%d"))
                                   ,editbutton = reactive(input$edit_button)
                                   ,emp_name = reactive(input$usr_name)
                                   ,datadelete = reactive(input$datadelete)
                        )
                        
                        output$Export <- downloadHandler(
                            filename = function() {
                                paste("MyData", gsub(":","-", Sys.time()), ".xlsx", sep = "")},
                            content = function(con){
                                write_xlsx(list(Summary = loadScheduleData("User1','User2','User3','User4"
                                                                           ,start_range())),
                                           path = con)}  # this will download all sheets
                        )
                        
                    }
                }
            },
            error = function(e){
                message('Caught an error!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp server'
                        
                    )
                    
                )
                
                stop()
            },
            warning = function(w){
                message('Caught a warning!')
                sendmail(
                    
                    from='from email'
                    
                    , to = 'to email'
                    
                    , subject = 'Error from Log Tool'
                    
                    , msg = w
                    
                    , control = list(
                        
                        smtpServer='smtp serverr'
                        
                    )
                    
                )
                stop()
            }
        )
    })
    
    output$levelone_type_selector = renderUI({ #creates level one select box object called in ui
        selectInput(inputId = "Level_1", #name of input
                    label = "Level-1:", #label displayed in ui
                    choices = c(unique(org_levels$Level_One)),
                    selected = "LevelOneOption") #default choice (not required)
    })
    output$leveltwo_type_selector = renderUI({#creates level two select box object called in ui
        
        data_available = org_levels[org_levels$Level_One == input$Level_1, "Level_Two"]
        #creates a reactive list of available level 2's based on previous selection made
        
        selectInput(inputId = "Level_2", #name of input
                    label = "Level-2:", #label displayed in ui
                    choices = unique(data_available), #calls list of available levels
                    selected = unique(data_available)[1])
    })
    output$levelthree_type_selector = renderUI({#creates level three select box object called in ui
        
        data__available = org_levels[org_levels$Level_Two == input$Level_2, "Level_Three"]
        #creates a reactive list of available level 3's based on previous selection made
        
        selectInput(inputId = "Level_3", #name of input
                    label = "Level-3:", #label displayed in ui
                    choices = unique(data__available), #calls list of available levels
                    selected = unique(data__available)[1])
    })
                                             
    
    }
    # Run the application 
    shinyApp(ui = ui, server = server)
    





