library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(nloptr)
library(zoo)
library(gridExtra)
library(highcharter)
library(dplyr)
library(billboarder)
library(formattable)

not_sel <- "Not Selected"

optimizer_page_mmm <- tabPanel(
  title = "DFT MMM Optimizer",
  titlePanel("Budget optimization by MMM"),
  br(),
  sidebarLayout(
    sidebarPanel(width = 3,
                 title = "Drv budget optimizer",
                 selectInput("objective", "Objective", c("max_revenue", "max_profit")),
                 textInput("total", "Total maximum spend", 167499),
                 textInput("Facebook", "Facebook current spend", 82800),
                 sliderInput("facebook_min",
                             "Facebook lower boundary %:",
                             min = 0,
                             max = 100,
                             value = 80),
                 sliderInput("facebook_max",
                             "Facebook upper boundary %:",
                             min = 0,
                             max = 500,
                             value = 120),
                 textInput("google_sem", "Google SEM current spend" , 30097),
                 sliderInput("google_sem_min",
                             "Google SEM lower boundary %:",
                             min = 0,
                             max = 100,
                             value = 80),
                 sliderInput("google_sem_max",
                             "Google upper boundary %:",
                             min = 0,
                             max = 500,
                             value = 120),     
                 textInput("Google_search", "Google UAC search current spend", 32234),
                 sliderInput("google_search_min",
                             "Google UAC search lower boundary %:",
                             min = 0,
                             max = 100,
                             value = 80),
                 sliderInput("google_search_max",
                             "Google UAC search upper boundary %:",
                             min = 0,
                             max = 500,
                             value = 120), 
                 textInput("Google_display", "Google UAC display current spend", 17185),
                 sliderInput("google_display_min",
                             "Google UAC display lower boundary %:",
                             min = 0,
                             max = 100,
                             value = 80),
                 sliderInput("google_display_max",
                             "Google UAC display upper boundary %:",
                             min = 0,
                             max = 500,
                             value = 120), 
                 textInput("Google_youtube", "Google UAC youtube current spend", 5183),
                 sliderInput("google_youtube_min",
                             "Google UAC youtube lower boundary %:",
                             min = 0,
                             max = 100,
                             value = 80),
                 sliderInput("google_youtube_max",
                             "Google UAC youtube upper boundary %:",
                             min = 0,
                             max = 500,
                             value = 120), 
                 br(),
                 actionButton("run_optimization", "Run Optimizer", icon = icon("play"))),
    mainPanel(
      tabsetPanel(
        fluidRow(
          column(width = 12, formattableOutput("budget_new")),
          plotOutput("budget_plot")
        ))))
)

optimizer_page_attri <- tabPanel(
  title = "DFT Marketing Category Optimizer",
  titlePanel("Budget optimization by marketing category"),
  br(),
  sidebarLayout(
    sidebarPanel(
      title = "Drv budget optimizer 2",
      selectInput("objective_2", "Objective", c("max_revenue", "max_profit")),
      textInput("total_2", "Total maximum spend", 527862),
      textInput("brand", "Brand current spend", 12512),
      sliderInput("brand_min",
                  "Brand lower boundary %:",
                  min = 0,
                  max = 100,
                  value = 80),
      sliderInput("brand_max",
                  "Brand upper boundary %:",
                  min = 0,
                  max = 500,
                  value = 120),
      textInput("referral", "Referral current spend" , 347850),
      sliderInput("referral_min",
                  "Referral lower boundary %:",
                  min = 0,
                  max = 100,
                  value = 80),
      sliderInput("referral_max",
                  "Referral upper boundary %:",
                  min = 0,
                  max = 500,
                  value = 120),     
      textInput("performance", "Performance current spend", 167500),
      sliderInput("performance_min",
                  "Performance lower boundary %:",
                  min = 0,
                  max = 100,
                  value = 80),
      sliderInput("performance_max",
                  "Performance upper boundary %:",
                  min = 0,
                  max = 500,
                  value = 120), 
      br(),
      actionButton("run_optimization_2", "Run Optimizer", icon = icon("play"))),
    mainPanel(
      tabsetPanel(
        fluidRow(
          column(width = 12, formattableOutput("budget_new_2")),
          plotOutput("budget_plot_2")
        ))))
)


mmm_page <- tabPanel(
  title = "MMM results",
  titlePanel("MMM decomposition"),
  sidebarLayout(
    sidebarPanel(
      title = "options",
      selectInput(
        "output_type", "",
        c("Coefficient", "Model decomposition", "Raw data")),
      br()),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Decomposition",
          fluidRow(
            column(width = 8, highchartOutput("area", width = 1200, height = 700)),
          )
        ),
        tabPanel(
          title = "Contribution",
          # fluidRow(
          #   column(width = 8, highchartOutput("area", width = 1200, height = 700)),
          #   )
        )
      )
    )
  ))



main_page <- tabPanel(
  title = "MMM data analysis",
  titlePanel("MMM MX Driver"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title"))),
            column(width = 4, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table")),
            column(width = 4, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 12, strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 12, tableOutput("combined_summary_table"))
          )
          
        )
      )
    )
  )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}

create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value)
    data.table(statistic, value)
  }
}

create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var != not_sel){
    if(num_var_1 != not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
    }
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)])))
  }
  return(res_tbl)
}

ui <- navbarPage(
  title = "MMM MX Driver v1.0",
  theme = shinytheme('united'),
  #main_page,
  mmm_page,
  optimizer_page_mmm,
  optimizer_page_attri
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  # plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  # 1-d summary tables
  
  output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(data_input(), fact_var())
  })
  
  output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
  
  # multi-d summary table
  
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
  
  
  
  new_budget <- reactiveValues(df = data.frame(Channels=c('Facebook', 
                                                          "Google SEM", 
                                                          'Goolge UAC display',
                                                          'Google UAC search',
                                                          'Google UAC youtube'), 
                                               recommended_spend=c(0, 0, 0, 0, 0)))
  optimized_val <- observeEvent(input$run_optimization, {
    channelConstrLow <- c(as.numeric(input$facebook_min), 
                          as.numeric(input$google_sem_min),
                          as.numeric(input$google_search_min),
                          as.numeric(input$google_display_min),
                          as.numeric(input$google_youtube_min))
    channelConstrUp <- c(as.numeric(input$facebook_max), 
                         as.numeric(input$google_sem_max),
                         as.numeric(input$google_search_max),
                         as.numeric(input$google_display_max),
                         as.numeric(input$google_youtube_max))
    histSpendUnit <- c(as.numeric(input$Facebook),
                       as.numeric(input$google_sem),
                       as.numeric(input$Google_search),
                       as.numeric(input$Google_display),
                       as.numeric(input$Google_youtube))
    expSpendUnitTotal <- as.numeric(input$total)
    
    hill_slope <- c(1.5655063084633656, 2.9811245580618904, 
                    3.2973626316638884, 0.9976730076396302,
                    2.7475943200274706)
    hill_ec <- c(0.9631160398067928,0.9634122889542315, 0.6547526797112453, 
                 0.804891091430947, 0.9157418527928798)
    hill_x_mean <- c(30946.59947930558, 30024.82280242298, 
                     16831.924873742566, 25711.826205114157,
                     14534.11587196591)
    hill_y_mean <- c(158.565556071875, 890.4239896113924,  
                     104.6256958768116,  180.4624142710145,
                     80.08055491470589)
    objective = input$objective
    if (objective ==  "max_revenue") {
      
      eval_f <- function(X) {
        return(
          list(
            "objective" =
              -round(sum(
                mapply(function(x, slope, ec, x_mu, y_mu) {
                  x <- x/x_mu
                  xOut <- y_mu*sum(1 / (1 + (x/ ec)**(-slope)))
                  
                  return(xOut)
                }, x=X, 
                slope = hill_slope, 
                ec = hill_ec, 
                x_mu = hill_x_mean, 
                y_mu = hill_y_mean,
                SIMPLIFY = T))),
            
            "gradient" = c(
              mapply(function(x, slope, ec, x_mu, y_mu) {
                x <- x/x_mu
                xOut <- -y_mu * sum(slope*(x/ec)**slope/(x*((x/ec)**slope +1)**2))
                
                return(xOut)
              }, x=X,
              slope = hill_slope,
              ec = hill_ec,
              x_mu = hill_x_mean,
              y_mu = hill_y_mean,
              SIMPLIFY = T)
            ), # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
            
            "objective.channel" =
              mapply(function(x, slope, ec, x_mu, y_mu) {
                
                x <- x/x_mu
                
                # hill transformation
                #xOut <- coeff * sum( (1 + gammaTran**alpha / (x/costMultiplier*adstockMultiplier) **alpha)**-1)
                xOut <- -y_mu*(1 / (1 + (x / ec)**(-slope)))
                
                return(xOut)
              }, x=X,
              slope = hill_slope,
              ec = hill_ec,
              x_mu = hill_x_mean,
              y_mu = hill_y_mean,
              SIMPLIFY = T
              )))}
      
    } else if (objective == 'max_profit') {
      eval_f <- function(X) {
        return(
          list(
            "objective" =
              -sum(
                mapply(function(x, slope, ec, x_mu, y_mu) {
                  x <- x/x_mu
                  xOut <- y_mu*sum(1 / (1 + (x/ ec)**(-slope)))
                  
                  return(xOut)
                }, x=X, 
                slope = hill_slope, 
                ec = hill_ec, 
                x_mu = hill_x_mean, 
                y_mu = hill_y_mean,
                SIMPLIFY = T))/sum(X),
            
            "gradient" = c(
              mapply(function(x, slope, ec, x_mu, y_mu) {
                x <- x/x_mu
                xOut <- -y_mu * sum(slope*(x/ec)**slope/(x*((x/ec)**slope +1)**2))
                
                return(xOut)
              }, x=X,
              slope = hill_slope,
              ec = hill_ec,
              x_mu = hill_x_mean,
              y_mu = hill_y_mean,
              SIMPLIFY = T)/X
            ), # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
            
            "objective.channel" =
              mapply(function(x, slope, ec, x_mu, y_mu) {
                
                x <- x/x_mu
                
                # hill transformation
                #xOut <- coeff * sum( (1 + gammaTran**alpha / (x/costMultiplier*adstockMultiplier) **alpha)**-1)
                xOut <- -y_mu*(1 / (1 + (x / ec)**(-slope)))
                
                return(xOut)
              }, x=X,
              slope = hill_slope,
              ec = hill_ec,
              x_mu = hill_x_mean,
              y_mu = hill_y_mean,
              SIMPLIFY = T
              )))}
    }
    
    ## set initial values and bounds
    lb <- histSpendUnit * channelConstrLow/100
    ub <- histSpendUnit * channelConstrUp/100
    x0 <- lb
    
    # set optim options
    
    # local_opts <- list(
    #   "algorithm" = "NLOPT_LD_SLSQP",
    #   "xtol_rel" = 0)
    # 
    # opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
    #               "xtol_rel" = 0,
    #               "maxeval" = 100000,
    #               "local_opts" = local_opts,
    #               "print_level" = 3)
    
    eval_g_eq <- function(X){
      return (round(sum(X)) - expSpendUnitTotal)
    }
    
    eval_g_ineq <- function(X) {
      return (round(sum(X)) - expSpendUnitTotal)
    }
    #Set optimization options.
    local_opts <- list( "algorithm" = "NLOPT_LN_AUGLAG_EQ", "xtol_rel" = 1.0e-5 )
    opts <- list( "algorithm"= "NLOPT_LN_AUGLAG_EQ",
                  "xtol_rel"= 1.0e-5,
                  "maxeval"= 1000000,
                  "local_opts" = local_opts )
    if (objective == 'max_revenue') {
      constr_mode = "ineq"} 
    else if (objective == 'max_profit'){
      constr_mode = "ineq"
    }
    
    ## run optim
    if (constr_mode  == "eq") {
      nlsMod <- nloptr( x0=x0,
                        eval_f=eval_f,
                        lb=lb,
                        ub=ub,
                        #eval_g_ineq=eval_g_ineq,
                        eval_g_eq=eval_g_eq,
                        opts=opts)
    } else if (constr_mode  == "ineq") {
      nlsMod <- nloptr( x0=x0,
                        eval_f=eval_f,
                        lb=lb,
                        ub=ub,
                        eval_g_ineq=eval_g_ineq,
                        #eval_g_eq=eval_g_eq,
                        opts=opts)
    }
    
    
    optimal_spend = c(nlsMod$solution[1], 
                      nlsMod$solution[2], 
                      nlsMod$solution[3],
                      nlsMod$solution[4],
                      nlsMod$solution[5])
    pred_dft =c(-eval_f(nlsMod$solution)[["objective.channel"]][1],
                -eval_f(nlsMod$solution)[["objective.channel"]][2],
                -eval_f(nlsMod$solution)[["objective.channel"]][3],
                -eval_f(nlsMod$solution)[["objective.channel"]][4],
                -eval_f(nlsMod$solution)[["objective.channel"]][5])
    current_dft = c(-eval_f(histSpendUnit)[["objective.channel"]][1],
                    -eval_f(histSpendUnit)[["objective.channel"]][2],
                    -eval_f(histSpendUnit)[["objective.channel"]][3],
                    -eval_f(histSpendUnit)[["objective.channel"]][4],
                    -eval_f(histSpendUnit)[["objective.channel"]][5])
    new_budget$df <- data.frame(
      Channels = c("Facebook", "Google SEM", "Google UAC search", 
                   "Google UAC display", "Google UAC youtube", "Total Paid"),
      Recommended_spend = append(optimal_spend, round(sum(optimal_spend))),
      Optimized_dft = append(pred_dft, sum(pred_dft)),
      Optimized_dac = append(optimal_spend, sum(optimal_spend))/append(pred_dft, sum(pred_dft)),
      Current_dft = append(current_dft,sum(current_dft)),
      Current_spend = append(histSpendUnit, sum(histSpendUnit)),
      Current_dac = append(histSpendUnit, sum(histSpendUnit))/append(current_dft,sum(current_dft)),
      Lower_boundary = append(lb, sum(lb)),
      Upper_bounday = append(ub, sum(ub)))
    
    new_budget$df <- new_budget$df %>% mutate(
      Budget_change = formattable::percent((Recommended_spend-Current_spend)/Current_spend),
      DFT_change = formattable::percent((Optimized_dft - Current_dft)/Current_dft))
  })
  
  new_budget_2 <- reactiveValues(df = data.frame(Channels=c('Brand', "Referral", 'Performance'), 
                                                 recommended_spend=c(0, 0, 0)))
  optimized_val_2 <- observeEvent(input$run_optimization_2, {
    
    channelConstrLow_2 <- c(as.numeric(input$brand_min), 
                            as.numeric(input$referral_min),
                            as.numeric(input$performance_min))
    
    channelConstrUp_2 <- c(as.numeric(input$brand_max), 
                           as.numeric(input$referral_max),
                           as.numeric(input$performance_max))
    
    histSpendUnit_2 <- c(as.numeric(input$brand),
                         as.numeric(input$referral),
                         as.numeric(input$performance))
    
    expSpendUnitTotal_2 <- as.numeric(input$total_2)
    
    hill_slope_2 <- c(1.7064256996062432, 1.7, 3.9292660873490286)
    hill_ec_2 <- c(0.8721761346440664, 0.8, 0.9858674430851717)
    hill_x_mean_2 <- c(100621.34149875806, 324298.79008264455, 93113.6494412291)
    hill_y_mean_2 <- c(434.02583218125, 1675.1789426652895, 911.29446330625)
    
    objective_2 = input$objective_2
    
    if (objective_2 ==  "max_revenue") {
      
      eval_f_2 <- function(X) {
        return(
          list(
            "objective" =
              -sum(
                mapply(function(x, slope, ec, x_mu, y_mu) {
                  x <- x/x_mu
                  xOut <- y_mu*sum(1 / (1 + (x/ ec)**(-slope)))
                  
                  return(xOut)
                }, x=X, 
                slope = hill_slope_2, 
                ec = hill_ec_2, 
                x_mu = hill_x_mean_2, 
                y_mu = hill_y_mean_2,
                SIMPLIFY = T)),
            
            "gradient" = c(
              mapply(function(x, slope, ec, x_mu, y_mu) {
                x <- x/x_mu
                xOut <- -y_mu * sum(slope*(x/ec)**slope/(x*((x/ec)**slope +1)**2))
                
                return(xOut)
              }, x=X,
              slope = hill_slope_2,
              ec = hill_ec_2,
              x_mu = hill_x_mean_2,
              y_mu = hill_y_mean_2,
              SIMPLIFY = T)
            ), # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
            
            "objective.channel" =
              mapply(function(x, slope, ec, x_mu, y_mu) {
                
                x <- x/x_mu
                
                # hill transformation
                #xOut <- coeff * sum( (1 + gammaTran**alpha / (x/costMultiplier*adstockMultiplier) **alpha)**-1)
                xOut <- -y_mu*(1 / (1 + (x / ec)**(-slope)))
                
                return(xOut)
              }, x=X,
              slope = hill_slope_2,
              ec = hill_ec_2,
              x_mu = hill_x_mean_2,
              y_mu = hill_y_mean_2,
              SIMPLIFY = T
              )))}
    } else if (objective == 'max_profit') {
      eval_f <- function(X) {
        return(
          list(
            "objective" =
              -sum(
                mapply(function(x, slope, ec, x_mu, y_mu) {
                  x <- x/x_mu
                  xOut <- y_mu*sum(1 / (1 + (x/ ec)**(-slope)))
                  
                  return(xOut)
                }, x=X, 
                slope = hill_slope_2, 
                ec = hill_ec_2, 
                x_mu = hill_x_mean_2, 
                y_mu = hill_y_mean_2,
                SIMPLIFY = T))/sum(X),
            
            "gradient" = c(
              mapply(function(x, slope, ec, x_mu, y_mu) {
                x <- x/x_mu
                xOut <- -y_mu * sum(slope*(x/ec)**slope/(x*((x/ec)**slope +1)**2))
                
                return(xOut)
              }, x=X,
              slope = hill_slope_2,
              ec = hill_ec_2,
              x_mu = hill_x_mean_2,
              y_mu = hill_y_mean_2,
              SIMPLIFY = T)/X
            ), # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
            
            "objective.channel" =
              mapply(function(x, slope, ec, x_mu, y_mu) {
                
                x <- x/x_mu
                
                # hill transformation
                #xOut <- coeff * sum( (1 + gammaTran**alpha / (x/costMultiplier*adstockMultiplier) **alpha)**-1)
                xOut <- -y_mu*(1 / (1 + (x / ec)**(-slope)))
                
                return(xOut)
              }, x=X,
              slope = hill_slope_2,
              ec = hill_ec_2,
              x_mu = hill_x_mean_2,
              y_mu = hill_y_mean_2,
              SIMPLIFY = T
              )))}
    }
    
    ## set initial values and bounds
    lb_2 <- histSpendUnit_2 * channelConstrLow_2/100
    ub_2 <- histSpendUnit_2 * channelConstrUp_2/100
    x0_2 <- lb_2
    
    # set optim options
    
    # local_opts <- list(
    #   "algorithm" = "NLOPT_LD_SLSQP",
    #   "xtol_rel" = 0)
    # 
    # opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
    #               "xtol_rel" = 0,
    #               "maxeval" = 100000,
    #               "local_opts" = local_opts,
    #               "print_level" = 3)
    
    eval_g_eq_2 <- function(X){
      return (round(sum(X)) - expSpendUnitTotal_2)
    }
    
    eval_g_ineq_2 <- function(X) {
      return (round(sum(X)) - expSpendUnitTotal_2)
    }
    #Set optimization options.
    local_opts_2 <- list( "algorithm" = "NLOPT_LN_AUGLAG_EQ", "xtol_rel" = 1.0e-5 )
    opts_2 <- list( "algorithm"= "NLOPT_LN_AUGLAG_EQ",
                    "xtol_rel"= 1.0e-5,
                    "maxeval"= 1000000,
                    "local_opts" = local_opts_2 )
    if (objective_2 == 'max_revenue') {
      constr_mode_2 = "ineq"} 
    else if (objective_2 == 'max_profit'){
      constr_mode_2 = "ineq"
    }
    
    ## run optim
    if (constr_mode_2  == "eq") {
      nlsMod_2 <- nloptr(x0=x0_2,
                         eval_f=eval_f_2,
                         lb=lb_2,
                         ub=ub_2,
                         #eval_g_ineq=eval_g_ineq,
                         eval_g_eq=eval_g_eq_2,
                         opts=opts_2)
    } else if (constr_mode_2  == "ineq") {
      nlsMod_2 <- nloptr( x0=x0_2,
                          eval_f=eval_f_2,
                          lb=lb_2,
                          ub=ub_2,
                          eval_g_ineq=eval_g_ineq_2,
                          #eval_g_eq=eval_g_eq,
                          opts=opts_2)
    }
    
    optimal_spend_2 = c(nlsMod_2$solution[1], 
                        nlsMod_2$solution[2], 
                        nlsMod_2$solution[3])
    pred_dft_2 =c(-eval_f_2(nlsMod_2$solution)[["objective.channel"]][1],
                  -eval_f_2(nlsMod_2$solution)[["objective.channel"]][2],
                  -eval_f_2(nlsMod_2$solution)[["objective.channel"]][3])
    current_dft_2 = c(-eval_f_2(histSpendUnit_2)[["objective.channel"]][1],
                      -eval_f_2(histSpendUnit_2)[["objective.channel"]][2],
                      -eval_f_2(histSpendUnit_2)[["objective.channel"]][3])
    new_budget_2$df <- data.frame(
      Channels = c("Brand", "Referral", "Performance", "Total"),
      Recommended_spend = append(optimal_spend_2, round(sum(optimal_spend_2))),
      Optimized_dft = append(pred_dft_2, sum(pred_dft_2)),
      Optimized_dac = append(optimal_spend_2, sum(optimal_spend_2))/append(pred_dft_2, sum(pred_dft_2)),
      Current_dft = append(current_dft_2,sum(current_dft_2)),
      Current_spend = append(histSpendUnit_2, sum(histSpendUnit_2)),
      Current_dac = append(histSpendUnit_2, sum(histSpendUnit_2))/append(current_dft_2,sum(current_dft_2)),
      lower_boundary = append(lb_2, sum(lb_2)),
      upper_bounday = append(ub_2, sum(ub_2))) 
    
    new_budget_2$df <- new_budget_2$df %>% mutate(
      Budget_change = formattable::percent((Recommended_spend-Current_spend)/Current_spend),
      DFT_change = formattable::percent((Optimized_dft - Current_dft)/Current_dft))
  })
  
  draw_pie <- function(df, colName, title){
    
    df <- head(df,5)
    df <- df %>% mutate(Channels_new = ifelse(grepl("UAC", Channels), "Google UAC", Channels))
    df <- df %>% group_by(Channels_new) %>% summarise(!!colName := sum(!!as.name(colName)))
    df <- df %>%
      mutate(share = !!as.name(colName)/sum(!!as.name(colName))*100)
    df$Channels_new <- factor(df$Channels_new, levels = rev(as.character(df$Channels_new)))
    ggplot(df, aes("", share, fill = Channels_new)) +
      geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
      coord_polar("y") +
      geom_text(aes(label = paste0(round(share), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL, title = title) + 
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual(values = c("#ffd700", "#C4A484", "#ffa500", "#254290")) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#666666"))}
  
  budget_plot <- eventReactive(input$run_optimization, {draw_pie(new_budget$df, "Recommended_spend", "Recommended budget share")})
  pft_plot <- eventReactive(input$run_optimization, {draw_pie(new_budget$df, "Optimized_dft", "Projected dft share")})
  
  
  output$budget_new <- renderFormattable({
    formattable(new_budget$df, list(
      Budget_change = formatter(
        "span",
        style = x ~ style(color=ifelse(x<0, "red", "green")),
        x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      DFT_change = formatter(
        "span",
        style = x ~ style(color=ifelse(x<0, "red", "green")),
        x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
      
    )
    )
    
  })
  
  #output$budget_new <- renderTable(new_budget$df)
  output$budget_plot <- renderPlot(grid.arrange(budget_plot(), pft_plot(), ncol=2, widths = c(2,2)))
  
  
  budget_plot_2 <- eventReactive(input$run_optimization_2, {draw_pie(head(new_budget_2$df, 3), "Recommended_spend", "Recommended budget share")})
  pft_plot_2 <- eventReactive(input$run_optimization_2, {draw_pie(head(new_budget_2$df, 3), "Optimized_dft", "Projected dft share")})
  
  output$budget_plot_2 <- renderPlot(grid.arrange(budget_plot_2(), pft_plot_2(), ncol=2, widths = c(2,2)))
  output$budget_new_2 <- renderFormattable({
    formattable(new_budget_2$df, list(
      Budget_change = formatter(
        "span",
        style = x ~ style(color=ifelse(x<0, "red", "green")),
        x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      DFT_change = formatter(
        "span",
        style = x ~ style(color=ifelse(x<0, "red", "green")),
        x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
      
    )
    )
    
  })
  
  # model contribution
  
  decomp <- read.csv("mmm_decomp_area_30_31.csv")
  cols <- names(decomp)[2:16]
  decomp[cols] <- lapply(format(decomp[cols], digits=0), as.numeric)
  
  decomp$Date <- as.Date.character(decomp$Date)
  
  
  
  contri_area <- highchart(type = "stock") %>%
    hc_yAxis(title = list(text = "Drv SUs"))%>%
    hc_add_series(decomp, "area", hcaes(Date, Drv.SUs), name = "Drv SUs" )%>%
    hc_add_series(decomp, "area", hcaes(Date, Baseline), name = "Baseline") %>%
    hc_add_series(decomp, "area", hcaes(Date, Facebook.branding), name = "Facebook branding") %>%
    hc_add_series(decomp, "area", hcaes(Date, Facebook.performance), name = "Facebook Performance") %>%
    hc_add_series(decomp, "area", hcaes(Date, DGD), name = "Referral DGD") %>%
    hc_add_series(decomp, "area", hcaes(Date, Pay.TV), name = "Pay TV") %>%
    hc_add_series(decomp, "area", hcaes(Date, Local.TV), name = "Local TV") %>%
    hc_add_series(decomp, "area", hcaes(Date, Google.branding), name = "Google branding") %>%
    hc_add_series(decomp, "area", hcaes(Date, Google.uac.display.network), name = "Google UAC display") %>%
    hc_add_series(decomp, "area", hcaes(Date, Google.uac.search), name = "Google UAC search") %>%
    hc_add_series(decomp, "area", hcaes(Date, Google.uac.youtube.videos), name = "Google UAC youtube") %>%
    hc_add_series(decomp, "area", hcaes(Date, Google.SEM), name = "Google SEM") %>%
    hc_add_series(decomp, "area", hcaes(Date, OOH), name = "OOH") %>%
    #hc_xAxis(categories = as.list(decomp$Date), title = list(text = "<b>Date</b>"), type = "datetime")%>%\
    #hc_tooltip(crosshairs=TRUE, pointFormat = '{point.y:.0f}') %>%
    hc_title(text = "Drivers contribution",
             margin = 20, align = "center",
             style = list(color = "#000000", useHTML = TRUE))
  
  output$area <- renderHighchart({
    contri_area
    
    
    # output$donut <- renderBillboarder({
    #     billboarder() %>% bb_donutchart(data = decomp, label = list(threshold = 0.10))
    #   })
  })
  
  output$mmm_result <-renderTable(decomp)
}

shinyApp(ui = ui, server = server)
