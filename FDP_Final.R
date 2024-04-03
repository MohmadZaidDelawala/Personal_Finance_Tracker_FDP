# Required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(RMySQL)
library(dplyr)
library(openxlsx)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Personal Finance Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Set Salary and Savings Goal", tabName = "salary"),
      menuItem("Add Transaction", tabName = "add_transaction"),
      menuItem("Calculate Total Expenses", tabName = "total_expenses"),
      menuItem("Visualize Spending", tabName = "visualize_spending"),
      menuItem("Set Bill Reminder", tabName = "set_bill_reminder"),
      menuItem("Display Bill Reminders", tabName = "display_bill_reminders"),
      menuItem("Expense Report", tabName = "expense_report"),
      menuItem("Erase Data", tabName = "erase_data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "salary",
              numericInput("salary_input", "Enter your monthly salary:", value = 0, min = 0),
              numericInput("goal_input", "Enter your savings goal:", value = 0, min = 0),
              actionButton("set_goal_button", "Set Goal"),
              textOutput("goal_status")
      ),
      tabItem(tabName = "add_transaction",
              dateInput("date_input", "Transaction Date:", value = Sys.Date()),
              selectInput("category_input", "Select transaction category:", 
                          choices = c("Food", "Travel", "Shopping", "Bill Payment", "Rent", "Other")),
              conditionalPanel(
                condition = "input.category_input == 'Other'",
                textInput("custom_category_input", "Enter custom category:", "")
              ),
              numericInput("amount_input", "Enter transaction amount:", value = 0, min = 0),
              selectInput("payment_mode_input", "Select Payment Mode:",
                          choices = c("Cash", "UPI", "Credit/Debit Card", "Cheque", "Other")),
              actionButton("add_transaction_button", "Add Transaction"),
              textOutput("transaction_status")
      ),
      tabItem(tabName = "total_expenses",
              fluidRow(
                box(
                  title = "Total Expenses",
                  textOutput("total_expenses_output")
                ),
                box(
                  title = "Remaining Amount",
                  textOutput("remaining_amount_output")
                )
              )
      ),
      tabItem(tabName = "visualize_spending",
              radioButtons("spending_type", "Select Spending Type:",
                           choices = c("Category-wise", "Date-wise", "Payment Mode-wise"),
                           selected = "Category-wise"),
              plotlyOutput("spending_plot")
      ),
      tabItem(tabName = "set_bill_reminder",
              dateInput("bill_date_input", "Enter the date of the bill reminder (YYYY-MM-DD):"),
              selectInput("bill_name_input", "Enter the name of the bill reminder:", 
                          choices = c("Mobile Bill", "Electricity Bill", "Credit Card", "Gas Cylinder", "Television", "Subscriptions", "Other")),
              numericInput("bill_amount_input", "Enter the amount for the bill reminder:", value = 0, min = 0),
              actionButton("set_reminder_button", "Set Bill Reminder"),
              textOutput("bill_reminder_status")
      ),
      tabItem(tabName = "display_bill_reminders",
              tableOutput("BillRemindersTable") 
      ),
      tabItem(tabName = "erase_data",
              checkboxGroupInput("erase_options", "Select Options to Erase:",
                                 choices = c("Erase Transaction", "Erase Saving Goal", "Erase Bill Reminder")),
              actionButton("erase_transactions_button", "Erase Data"),
              textOutput("erase_transactions_status")
      ),
      tabItem(tabName = "expense_report",
              fluidRow(
                box(
                  title = "Expense Report",
                  tableOutput("expense_report_table")
                ),
                box(
                  title = "",
                  downloadButton("download_expense_report", "Download Expense Report (Excel)", 
                                 class = "btn-success")
                )
              )
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  con <- dbConnect(MySQL(), user = "root",
                   password = "M.zaid_01",
                   dbname = "personal_finance_tracker",
                   host = "localhost")
  
  # Function to set a savings goal
  set_savings_goal <- function(salary, goal) {
    if (goal >= 0) {
      query <- sprintf("INSERT INTO savings_goal (salary, goal) VALUES (%f, %f)", salary, goal)
      dbSendQuery(con, query)
      output$goal_status <- renderText({
        paste("Savings goal set successfully.")
      })
      # Calculate usable amount and insert into usable_amount table
      usable_amt <- salary - goal
      remaining_amt <- usable_amt - get_total_expenses()
      insert_usable_amount(usable_amt, remaining_amt)
    } else {
      output$goal_status <- renderText({
        paste("Savings goal must be a non-negative amount.")
      })
    }
  }
  
  
  # Function to add a transaction
  add_transaction <- function() {
    amount_input <- as.numeric(input$amount_input)
    remaining_amt <- get_usable_amount() - get_total_expenses()
    
    if (!is.na(amount_input) && amount_input > 0) {
      category <- input$category_input
      if(category == "Other") {
        category <- input$custom_category_input
      }
      date <- input$date_input
      payment_mode <- input$payment_mode_input
      
      if (amount_input <= remaining_amt) {
        # Construct and execute the query to insert transaction details into the database
        query <- sprintf("INSERT INTO transactions (date, category, amount, payment_mode) VALUES ('%s', '%s', %.2f, '%s')", date, category, amount_input, payment_mode)
        dbSendQuery(con, query)
        
        output$transaction_status <- renderText({
          paste("Transaction added successfully.")
        })
        
        update_remaining_amount(get_usable_amount() - get_total_expenses())
      } else {
        output$transaction_status <- renderText({
          paste("Error: Transaction amount exceeds remaining amount.")
        })
      }
    } else {
      output$transaction_status <- renderText({
        paste("Alert: Invalid transaction amount.")
      })
    }
  }
  
  
  # Function to erase data
  erase_transactions <- eventReactive(input$erase_transactions_button, {
    selected_options <- input$erase_options
    
    # Define default queries
    query_transactions <- ""
    query_savings_goal <- ""
    query_bill_reminders <- ""
    query_usable_amount <- "DELETE FROM usable_amount"
    
    # Check which checkboxes are selected
    if ("Erase All" %in% selected_options) {
      query_transactions <- "DELETE FROM transactions"
      query_savings_goal <- "DELETE FROM savings_goal"
      query_bill_reminders <- "DELETE FROM bill_reminders"
    } else {
      if ("Erase Transaction" %in% selected_options) {
        query_transactions <- "DELETE FROM transactions"
        # Reset auto-increment counter for transactions table
        query_reset_auto_increment <- "ALTER TABLE transactions AUTO_INCREMENT = 1"
      }
      if ("Erase Saving Goal" %in% selected_options) {
        query_savings_goal <- "DELETE FROM savings_goal"
      }
      if ("Erase Bill Reminder" %in% selected_options) {
        query_bill_reminders <- "DELETE FROM bill_reminders"
      }
    }
    
    # Execute the queries
    tryCatch({
      if (query_transactions != "") {
        dbSendQuery(con, query_transactions)
        if (exists("query_reset_auto_increment")) {
          dbSendQuery(con, query_reset_auto_increment)
        }
      }
      if (query_savings_goal != "") dbSendQuery(con, query_savings_goal)
      if (query_bill_reminders != "") dbSendQuery(con, query_bill_reminders)
      dbSendQuery(con, query_usable_amount)  # Delete data from usable_amount table
      
      return("Selected data has been erased successfully.")
    }, error = function(e) {
      cat("Error erasing data:", e$message, "\n")
      return("Failed to erase selected data.")
    })
  })
  
  # Function to get total expenses
  get_total_expenses <- function() {
    query <- "SELECT IFNULL(SUM(amount), 0) FROM transactions"
    result <- dbGetQuery(con, query)
    return(result[[1]])
  }
  
  # Function to insert usable amount and remaining amount into usable_amount table
  insert_usable_amount <- function(usable_amt, remaining_amt) {
    query <- sprintf("INSERT INTO usable_amount (usable_amount, remaining_amount) VALUES (%.2f, %.2f)", usable_amt, remaining_amt)
    dbSendQuery(con, query)
  }
  
  # Function to get usable amount from usable_amount table
  get_usable_amount <- function() {
    query <- "SELECT usable_amount FROM usable_amount ORDER BY id DESC LIMIT 1"
    result <- dbGetQuery(con, query)
    return(result[[1]])
  }
  
  # Function to update remaining amount in usable_amount table
  update_remaining_amount <- function(remaining_amt) {
    query <- sprintf("UPDATE usable_amount SET remaining_amount = %.2f ORDER BY id DESC LIMIT 1", remaining_amt)
    dbSendQuery(con, query)
  }
  
  # Function to visualize spending categories
  visualize_spending_category <- function() {
    query <- "SELECT category, SUM(amount) AS total_amount FROM transactions GROUP BY category"
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      # Create a pie chart with labels showing both category names and amounts
      plot_ly(result, labels = ~category, values = ~total_amount, type = 'pie') %>%
        layout(title = "Spending by Category")
    } else {
      cat("No transactions found.\n")
    }
  }
  
  # Function to visualize spending by date
  visualize_spending_date <- function() {
    query <- "SELECT date, SUM(amount) AS total_amount FROM transactions GROUP BY date"
    result <- dbGetQuery(con, query)
    
    return(result)
  }
  
  # Function to visualize spending by payment mode
  visualize_spending_payment_mode <- function() {
    query <- "SELECT payment_mode, SUM(amount) AS total_amount FROM transactions GROUP BY payment_mode"
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      # Create a bar chart with labels showing both payment modes and amounts
      plot_ly(result, x = ~payment_mode, y = ~total_amount, type = 'bar') %>%
        layout(title = "Spending by Payment Mode", xaxis = list(title = "Payment Mode"), yaxis = list(title = "Total Amount"))
    } else {
      cat("No transactions found.\n")
    }
  }
  
  # Function to set a bill reminder
  set_bill_reminder <- function() {
    query <- sprintf("INSERT INTO bill_reminders (date, name, amount) VALUES ('%s', '%s', %.2f)", input$bill_date_input, input$bill_name_input, input$bill_amount_input)
    dbSendQuery(con, query)
    output$bill_reminder_status <- renderText({
      paste("Bill reminder set successfully.")
    })
  }
  
  # Function to display bill reminders
  display_bill_reminders <- function() {
    query <- "SELECT date, name, amount FROM bill_reminders"
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      # Display bill reminders in a table
      return(result)
    } else {
      return("No bill reminders have been set.")
    }
  }
  
  # Event handler for setting savings goal
  observeEvent(input$set_goal_button, {
    set_savings_goal(input$salary_input, input$goal_input)
  })
  
  # Event handler for adding a transaction
  observeEvent(input$add_transaction_button, {
    add_transaction()
  })
  
  # Render total expenses
  output$total_expenses_output <- renderText({
    total_expenses <- get_total_expenses()
    paste("Total Expenses: $", format(total_expenses, big.mark = ",", scientific = FALSE))
  })
  
  # Render remaining amount
  output$remaining_amount_output <- renderText({
    remaining_amt <- get_usable_amount() - get_total_expenses()
    paste("Remaining Amount: $", format(remaining_amt, big.mark = ",", scientific = FALSE))
  })
  
  # Render spending plot based on selection
  output$spending_plot <- renderPlotly({
    if (input$spending_type == "Category-wise") {
      visualize_spending_category()
    } else if (input$spending_type == "Date-wise") {
      result <- visualize_spending_date()
      plot_ly(result, x = ~date, y = ~total_amount, type = 'bar', marker = list(color = 'rgba(50, 171, 96, 0.6)')) %>%
        layout(title = "Transaction by Date", xaxis = list(title = "Date"), yaxis = list(title = "Total Amount"))
    } else if (input$spending_type == "Payment Mode-wise") {
      visualize_spending_payment_mode()
    }
  })
  
  # Render bill reminders table
  output$BillRemindersTable <- renderTable({
    display_bill_reminders()
  })
  
  # Event handler for setting a bill reminder
  observeEvent(input$set_reminder_button, {
    set_bill_reminder()
  })
  
  # Render the status message after erasing data
  output$erase_transactions_status <- renderText({
    erase_transactions()
  })
  
  # Function to display expense report
  output$expense_report_table <- renderTable({
    query <- "SELECT * FROM transactions"
    result <- dbGetQuery(con, query)
    return(result)
  })
  
  # Function to generate and download expense report as Excel
  output$download_expense_report <- downloadHandler(
    filename = function() {
      paste("expense_report", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      query <- "SELECT * FROM transactions"
      result <- dbGetQuery(con, query)
      write.xlsx(result, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
