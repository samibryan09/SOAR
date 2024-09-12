# installing the needed packages
library(shiny)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(shinyscreenshot)
library(ggiraph)

##################################################################
################## Server ########################################

# Define server
server <- function(input, output, session) {
  shiny::tags$head(shiny::tags$style(HTML(css)))
  
  
  
  ##################################################################
  ################## Reactive buttons server #######################
  ##################################################################
  showSurvey <- reactiveVal(FALSE)
  
  observeEvent(input$start_survey_btn, {
    # Toggle the visibility of the survey
    showSurvey(!showSurvey())
  })
  output$showSurvey <- reactive({
    showSurvey()
  })
  outputOptions(output, "showSurvey", suspendWhenHidden = FALSE)
  
  
  ## showing the directions 
  showDirection <- reactiveVal(TRUE)
  
  observeEvent(input$direction_btn, {
    showDirection(!showDirection())
  })
  output$showDirection <- reactive({
    showDirection()
  })
  outputOptions(output, "showDirection", suspendWhenHidden = FALSE)
  
  ## showing the tool
  observeEvent(input$start_tool_btn, {
    updateTabsetPanel(session, "tabset", selected = "Questionnaire")
  })
  
  ## agrivoltaics
  observeEvent(input$acres_peach, {
    if(is.na(input$acres_peach) || input$acres_peach == "") {
      updateNumericInput(session, "acres_peach", value = 0)
    }
  })
  
  
  ##### Analysis Tab  buttons ##### 
  #### Descriptor buttons ####
  # demand checkboxes
  showDslider <- reactiveVal(FALSE)
  
  observeEvent(input$show_dslider_btn, {
    showDslider(!showDslider())
  })
  output$showDslider <- reactive({
    showDslider()
  })
  outputOptions(output, "showDslider", suspendWhenHidden = FALSE)
  
  # Supply check boxes
  showSslider <- reactiveVal(FALSE)
  observeEvent(input$show_sslider_btn, {
    showSslider(!showSslider())
  })
  output$showSslider <- reactive({
    showSslider()
  })
  outputOptions(output, "showSslider", suspendWhenHidden = FALSE)
  
  # dropdown menu button
  showdropdown <- reactiveVal(FALSE)
  observeEvent(input$show_dropdown_btn, {
    showdropdown(!showdropdown())
  })
  output$showdropdown <- reactive({
    showdropdown()
  })
  outputOptions(output, "showdropdown", suspendWhenHidden = FALSE)
  
  # solar advantage button 
  showSAbutton <- reactiveVal(FALSE)
  observeEvent(input$show_more_btn, {
    showSAbutton(!showSAbutton())
  })
  output$showSAbutton <- reactive({
    showSAbutton()
  })
  outputOptions(output, "showSAbutton", suspendWhenHidden = FALSE)
  
  # Electric advanatge button
  showEAbutton <- reactiveVal(FALSE)
  observeEvent(input$show_electric_btn, {
    showEAbutton(!showEAbutton())
  })
  output$showEAbutton <- reactive({
    showEAbutton()
  })
  outputOptions(output, "showEAbutton", suspendWhenHidden = FALSE)
  
  # electrical disadvantage
  showEDbutton <- reactiveVal(FALSE)
  observeEvent(input$ed_button, {
    showEDbutton(!showEDbutton())
  })
  output$showEDbutton <- reactive({
    showEDbutton()
  }) 
  outputOptions(output, "showEDbutton", suspendWhenHidden = FALSE)
  
  # 120% of demand 
  moredemand_btn <- reactiveVal(FALSE)
  observeEvent(input$show_120demand_btn, {
    moredemand_btn(!moredemand_btn())
  })
  output$moredemand_btn <- reactive({
    moredemand_btn()
  })
  outputOptions(output, "moredemand_btn", suspendWhenHidden = FALSE)
  
  
  # Reactive values to track visibility 
  solarVisible <- reactiveVal(FALSE)
  electricAdvVisible <- reactiveVal(FALSE)
  electricDisadvVisible <- reactiveVal(FALSE)
  demandVisible <- reactiveVal(FALSE)
  
  #### graph buttons ####
  observeEvent(input$toggle_button, {
    demandVisible(!demandVisible())
    
    if (input$toggle_button %% 2 == 1) {
      removeClass(selector = "#toggle_button", class = "initial-button")
      addClass(selector = "#toggle_button", class = "demand-button")
    } else {
      removeClass(selector = "#toggle_button", class = "demand-button")
      addClass(selector = "#toggle_button", class = "initial-button")
    }
  })
  
  output$graphic_demand <- renderUI({
    if (demandVisible()) {
      p("120% Demand Line: The 120% demand line represents the maximum grid input capacity. 
        Solar installations can not be installed exceeding this line without being taxed as an energy producers,
        requiring additional regulatory considerations.")
    }
  })
  
  # Observe button clicks to toggle visibility and change button style
  observeEvent(input$solar_advantage_button, {
    solarVisible(!solarVisible())
    
    if (input$solar_advantage_button %% 2 == 1) {
      removeClass(selector = "#solar_advantage_button", class = "initial-button")
      addClass(selector = "#solar_advantage_button", class = "solar-button")
    } else {
      removeClass(selector = "#solar_advantage_button", class = "solar-button")
      addClass(selector = "#solar_advantage_button", class = "initial-button")
    }
  })
  
  #solar advnatage text 
  output$solar_text <- renderUI({
    if (solarVisible()) {
      p("Solar Advantage: This highlighted section indicates where there $/kWh of installing solar is cheaper than buying electricity from the grid.")
    }
  })
  
  ### electric advantage button ###
  observeEvent(input$electric_advantage_button, {
    electricAdvVisible(!electricAdvVisible())
    
    if (input$electric_advantage_button %% 2 == 1) {
      removeClass(selector = "#electric_advantage_button", class = "initial-button")
      addClass(selector = "#electric_advantage_button", class = "electric-button")
    } else {
      removeClass(selector = "#electric_advantage_button", class = "electric-button")
      addClass(selector = "#electric_advantage_button", class = "initial-button")
    }
  })
  
  output$electric_adv_text <- renderUI({
    if (electricAdvVisible()) {
      p("Electric Advantage: The operational breakeven cost per kWh for these tools and vehicles is higher than the current grid price. 
        This means that the grid price would need to rise to meet the breakeven point before the electric versions become 
        less advantageous compared to their gas counterparts.")
    }
  })
  
  
  ### Electric Disadvantage ### 
  observeEvent(input$electric_disadvantage_button, {
    electricDisadvVisible(!electricDisadvVisible())
    
    if (input$electric_disadvantage_button %% 2 == 1) {
      removeClass(selector = "#electric_disadvantage_button", class = "initial-button")
      addClass(selector = "#electric_disadvantage_button", class = "disadv-button")
    } else {
      removeClass(selector = "#electric_disadvantage_button", class = "disadv-button")
      addClass(selector = "#electric_disadvantage_button", class = "initial-button")
    }
  })
  
  
  output$electric_disadv_text <- renderUI({
    if (electricDisadvVisible()) {
      p("Electric Disadvantage: The operational breakeven in $/kWh for these electric tools or vehicles
        are less than the grid price and therefore there is no economic advantage to electrifying these tools.")
    }
  })
  
  ## tab 3 buttons ##
  #Questionnaire results button
  showq_result_bttn  <- reactiveVal(FALSE)
  observeEvent(input$q_result_bttn, {
    showq_result_bttn(!showq_result_bttn())
  })
  output$showq_result_bttn <- reactive({
    showq_result_bttn()
  })
  outputOptions(output, "showq_result_bttn", suspendWhenHidden = FALSE)
  
  
  # supply and demand summary table button
  showsumtables_bttn  <- reactiveVal(FALSE)
  observeEvent(input$sumtables_bttn, {
    showsumtables_bttn(!showsumtables_bttn())
  })
  output$showsumtables_bttn <- reactive({
    showsumtables_bttn()
  })
  outputOptions(output, "showsumtables_bttn", suspendWhenHidden = FALSE)
  
  # Graphic summary button
  showgraph_q1_bttn <- reactiveVal(FALSE)
  observeEvent(input$graph_q1_bttn, {
    showgraph_q1_bttn(!showgraph_q1_bttn())
  })
  output$showgraph_q1_bttn <- reactive({
    showgraph_q1_bttn()
  })
  outputOptions(output, "showgraph_q1_bttn", suspendWhenHidden = FALSE)
  
  
  
  # show additional information button
  showadd_info_bttn <- reactiveVal(FALSE)
  observeEvent(input$add_info_bttn, {
    showadd_info_bttn(!showadd_info_bttn())
  })
  output$showadd_info_bttn <- reactive({
    showadd_info_bttn()
  })
  outputOptions(output, "showadd_info_bttn", suspendWhenHidden = FALSE)
  
  
  ##################################################################
  ################## Analysis Tab server ##################################
  ##################################################################
  
  # Create reactive expressions for each tool
  chainsaw_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tool_info(
      tool_name = "Chainsaw",
      electric_cost =  electric_cost_value,
      gas_cost = input$gas_cost,
      hours_used = input$tool_1,
      gas_kwh = 4.75,
      gas_use = 0.1323,
      lifespan = 10 # years
    )
  })
  
  
  forklift_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tool_info(
      tool_name = "Forklift",
      electric_cost =  electric_cost_value,
      gas_cost = input$diesel_cost,
      hours_used = input$tool_3,
      gas_kwh = 61.05,
      gas_use = 1.5,
      lifespan = 10
    )
  })
  
  windmachine_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tool_info(
      tool_name = "Windmachine",
      electric_cost =  electric_cost_value,
      gas_cost = input$gas_cost,
      hours_used = input$tool_4,
      gas_kwh = 407,
      gas_use = 10,
      lifespan = 10
    )
  })
  
  atv_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tool_info(
      tool_name = "ATV",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_5,
      gas_kwh = 14.91,
      gas_use = 0.9 , # 27.8 mpg at 25 mph, 
      lifespan = 10
    )
  })
  
  
  
  # Define a function to calculate tool information
  calculate_tool_info <- function(tool_name, gas_cost, electric_cost, hours_used, gas_kwh, gas_use, lifespan) {
    # Cost per hour for gas tool
    cost_hour_gas <- gas_use * gas_cost # gal/hour * $/gal = $/hour
    
    # Electric kWh, adjusting for efficiency
    electric_kwh <- gas_kwh * (0.6) # electric tools are 40% more efficient than gas
    
    # Cost per hour for electric tool
    cost_hour_electric <- electric_kwh * electric_cost
    
    electric_costs <- numeric(10)
    # Operational cost for electric tool
    op_cost_electric <- cost_hour_electric * hours_used # annual
    # Calculate costs for each year
    for (year in 1:10) {
      # Increase electricity cost by 2% each year
      adjusted_electric_cost <- electric_cost * (1.02 ^ (year - 1))
      
      # Calculate cost per hour for this year
      cost_hour_electric <- electric_kwh * adjusted_electric_cost
      
      # Calculate total cost for this year
      electric_cost_this_year <- cost_hour_electric * hours_used
      
      # Store the cost for this year
      electric_costs[year] <- electric_cost_this_year
    }
    
    
    op_cost_electric_total <- sum(electric_costs)
    
    # Operational cost for gas tool
    op_cost_gas <- cost_hour_gas * hours_used # annual
    op_cost_gas_total <- cost_hour_gas * hours_used * lifespan # total for 10 years
    
    # Annual kWh used per year
    annual_kwh_used <- hours_used * electric_kwh
    
    # Total cost across the lifespan
    electric_savings <- op_cost_gas_total - op_cost_electric_total
    
    op_be_gas <- cost_hour_gas/ electric_kwh
    
    list(
      tool = tool_name,
      op_be_gas = op_be_gas, 
      annual_kwh_used = annual_kwh_used,
      op_cost_gas = op_cost_gas,
      op_cost_electric = op_cost_electric,
      op_cost_gas_total = op_cost_gas_total,
      op_cost_electric_total = op_cost_electric_total,
      electric_savings = electric_savings 
    )
  }
  
  
  ################### vehicle df######### grid_increase_factor <- 1.02
  calculate_big_info <- function(gal_per_mile, electric_kwh_mile, mileage_input, gas_cost, electric_cost) {
    # Cost per mile for gas vehicle
    cost_per_mile_gas <- gal_per_mile * gas_cost
    
    # Operational budget efficiency for gas
    op_be_gas <- cost_per_mile_gas / electric_kwh_mile
    
    # Annual kWh used for electric vehicle
    annual_kwh_used <- mileage_input * electric_kwh_mile
    
    # Operational cost per year
    op_cost_gas <- mileage_input * cost_per_mile_gas
    op_cost_electric <- mileage_input * electric_kwh_mile * electric_cost
    
    # Total operational cost over 10 years
    
    vehicle_electric_costs <- numeric(10)
    
    # Calculate costs for each year
    for (year in 1:10) {
      # Increase electricity cost by 2% each year
      adjusted_electric_cost <- electric_cost * (1.02 ^ (year - 1))
      
      # Calculate operational cost for this year
      electric_cost_this_year <- annual_kwh_used * adjusted_electric_cost
      
      # Store the cost for this year
      vehicle_electric_costs[year] <- electric_cost_this_year
    }
    
    
    op_cost_gas_total <-  op_cost_gas * 10
    op_cost_electric_total <- sum(vehicle_electric_costs)
    electric_savings <- op_cost_gas_total - op_cost_electric_total
    
    
    list(
      op_be_gas = op_be_gas, 
      annual_kwh_used = annual_kwh_used,
      op_cost_gas = op_cost_gas,
      op_cost_electric = op_cost_electric,
      op_cost_gas_total = op_cost_gas_total,
      op_cost_electric_total = op_cost_electric_total,
      electric_savings = electric_savings
    )
  }
  
  # Define reactives
  pickup_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_big_info(
      gal_per_mile = 0.05,
      electric_kwh_mile = 0.4, 
      mileage_input = input$vehicle_1, 
      gas_cost = input$gas_cost,
      electric_cost = electric_cost_value
    )
  })
  
  worktruck_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_big_info(
      gal_per_mile = 0.125,
      electric_kwh_mile = 2.035, 
      mileage_input = input$vehicle_2, 
      gas_cost = input$gas_cost,
      electric_cost = electric_cost_value
    )
  })
  
  ### tractor 
  ### tractor implements ###
  tpruner_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tractor_info(
      tool_name = "Tractor Pruner",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_6,
      gas_kwh = 13.42,
      gas_use = 0.3, 
      lifespan = 10
    )
  })
  
  tmower_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tractor_info(
      tool_name = "Tractor mower",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_7,
      gas_kwh = 22.37,
      gas_use = 0.5, 
      lifespan = 10
    )
  })
  
  tsprayer_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tractor_info(
      tool_name = "Tractor sprayer",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_8,
      gas_kwh = 814, 
      gas_use = 20, 
      lifespan = 10
    )
  })
  
  tflail_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tractor_info(
      tool_name = "Tractor flail",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_9,
      gas_kwh = 1221, 
      gas_use = 30, 
      lifespan = 10
    )
  })
  
  tnoimplement_info <- reactive({
    electric_cost_value <- grid_charge()
    calculate_tractor_info(
      tool_name = "Tractor alone",
      gas_cost = input$gas_cost,
      electric_cost =  electric_cost_value,
      hours_used = input$tool_10,
      gas_kwh = 305.25, 
      gas_use = 7.5, 
      lifespan = 10
    )
  })
  
  
  ## 
  
  
  ### Tractor Calculations ###
  calculate_tractor_info <- function(tool_name, gas_cost, electric_cost, hours_used, gas_kwh, gas_use, lifespan) {
    # Cost per hour for gas tool
    cost_hour_gas <- gas_use * gas_cost # gal/hour * $/gal = $/hour
    
    # Electric kWh, adjusting for efficiency
    electric_kwh <- gas_kwh * 0.4 # assumes electric tractors are 60% more efficient than gas
    
    # Cost per hour for electric tool
    cost_hour_electric <- electric_kwh * electric_cost
    
    # Operational cost for electric tool
    op_cost_electric <- cost_hour_electric * hours_used 
    
    # Operational cost for gas tool
    op_cost_gas <- cost_hour_gas * hours_used 
    
    # Annual kWh used per year
    annual_kwh_used <- hours_used * electric_kwh
    op_be_gas <- cost_hour_gas / electric_kwh
    
    # Total operational cost over 10 years
    tractor_electric_costs <- numeric(10)
    
    # Calculate costs for each year
    for (year in 1:10) {
      # Increase electricity cost by 2% each year
      adjusted_electric_cost <- electric_cost * (1.02 ^ (year - 1))
      
      # Calculate operational cost for this year
      electric_cost_this_year <- annual_kwh_used * adjusted_electric_cost
      
      # Store the cost for this year
      tractor_electric_costs[year] <- electric_cost_this_year
    }
    
    op_cost_gas_total <-  op_cost_gas * 10
    op_cost_electric_total <- sum(tractor_electric_costs)
    electric_savings <- op_cost_gas_total - op_cost_electric_total
    
    print(list(
      tool = tool_name,
      op_be_gas = op_be_gas,
      annual_kwh_used = annual_kwh_used,
      op_cost_gas = op_cost_gas,
      op_cost_electric = op_cost_electric,
      op_cost_gas_total = op_cost_gas_total,
      op_cost_electric_total = op_cost_electric_total,
      electric_savings = electric_savings 
    ))
    
    
    list(
      tool = tool_name,
      op_be_gas = op_be_gas,
      annual_kwh_used = annual_kwh_used,
      op_cost_gas = op_cost_gas,
      op_cost_electric = op_cost_electric,
      op_cost_gas_total = op_cost_gas_total,
      op_cost_electric_total = op_cost_electric_total,
      electric_savings = electric_savings 
    )
  }
  
  # Combined reactive expression to calculate sums
  tool_sums <- reactive({
    tractor_infos <- list(tpruner_info(), tmower_info(), tsprayer_info(), tflail_info(), tnoimplement_info())
    op_cost_gas_sum <- sum(sapply(tractor_infos, function(info) info$op_cost_gas))
    op_cost_electric_sum <- sum(sapply(tractor_infos, function(info) info$op_cost_electric))
    op_cost_gas_total_sum <- sum(sapply(tractor_infos, function(info) info$op_cost_gas_total))
    op_cost_electric_total_sum <- sum(sapply(tractor_infos, function(info) info$op_cost_electric_total))
    electric_savings_sum <- sum(sapply(tractor_infos, function(info) info$electric_savings))
    annual_kwh_used_sum <- sum(sapply(tractor_infos, function(info) info$annual_kwh_used))
    op_be_gas_avg <- mean(sapply(tractor_infos, function(info) info$op_be_gas))
    
    list(
      op_cost_gas_sum = op_cost_gas_sum, 
      op_cost_electric_sum = op_cost_electric_sum,
      op_cost_gas_total_sum = op_cost_gas_total_sum,
      op_cost_electric_total_sum = op_cost_electric_total_sum,
      electric_savings_sum = electric_savings_sum,
      annual_kwh_used_sum = annual_kwh_used_sum,
      op_be_gas_avg = op_be_gas_avg
    )
  })
  
  # Reactive expression to use summed values
  tractor_info <- reactive({
    # Get the summed values from tool_sums
    sum_values <- tool_sums()
    
    list(
      op_cost_gas_total = sum_values$op_cost_gas_total_sum,
      op_cost_electric_total = sum_values$op_cost_electric_total_sum,
      op_cost_electric = sum_values$op_cost_electric_sum,
      op_cost_gas = sum_values$op_cost_gas_sum,
      electric_savings = sum_values$electric_savings_sum,
      annual_kwh_used = sum_values$annual_kwh_used_sum,
      op_be_gas = sum_values$op_be_gas_avg
    )
  })
  
  
  
  ## Demand Df ##
  demand_dataframe<- reactive({
    electric_cost_value <- grid_charge()
    pickup_info_value <- pickup_info()
    worktruck_info_value <- worktruck_info()
    chainsaw_info_value <- chainsaw_info()
    forklift_info_value <- forklift_info()
    windmachine_info_value <- windmachine_info()
    atv_info_value <- atv_info()
    tractor_info_value <- tractor_info()
    
    #### Demand Df ####
    demand_graph_df <- data.frame(
      tool_name = c("Electricity Bill", "Work Truck", "Pickup Truck", "Chainsaw",
                    "Forklift", "Windmachine", "ATV", "Tractor"),
      op_be = c(electric_cost_value,
                
                pickup_info_value$op_be_gas,
                worktruck_info_value$op_be_gas,
                
                chainsaw_info_value$op_be_gas,
                
                forklift_info_value$op_be_gas,
                windmachine_info_value$op_be_gas,
                atv_info_value$op_be_gas, 
                
                tractor_info_value$op_be_gas),
      
      annual_kwh_tool = c(input$energy,
                          
                          pickup_info_value$annual_kwh_used,
                          worktruck_info_value$annual_kwh_used,
                          
                          chainsaw_info_value$annual_kwh_used,
                          
                          forklift_info_value$annual_kwh_used,
                          windmachine_info_value$annual_kwh_used,
                          atv_info_value$annual_kwh_used, 
                          
                          tractor_info_value$annual_kwh_used),
      
      
      op_cost_electric = c("N/A",
                           pickup_info_value$op_cost_electric,
                           worktruck_info_value$op_cost_electric,
                           
                           chainsaw_info_value$op_cost_electric,
                           
                           forklift_info_value$op_cost_electric,
                           windmachine_info_value$op_cost_electric,
                           atv_info_value$op_cost_electric,
                           
                           tractor_info_value$op_cost_electric),
      
      op_cost_gas = c("N/A",
                      pickup_info_value$op_cost_gas,
                      worktruck_info_value$op_cost_gas,
                      
                      chainsaw_info_value$op_cost_gas,
                      forklift_info_value$op_cost_gas,
                      windmachine_info_value$op_cost_gas,
                      atv_info_value$op_cost_gas,
                      
                      tractor_info_value$op_cost_gas),
      
      op_cost_electric_total = c("N/A",
                                 pickup_info_value$op_cost_electric_total,
                                 worktruck_info_value$op_cost_electric_total,
                                 chainsaw_info_value$op_cost_electric_total,
                                 
                                 forklift_info_value$op_cost_electric_total,
                                 windmachine_info_value$op_cost_electric_total,
                                 atv_info_value$op_cost_electric_total,
                                 tractor_info_value$op_cost_electric_total), 
      
      op_cost_gas_total = c("N/A",
                            pickup_info_value$op_cost_gas_total,
                            worktruck_info_value$op_cost_gas_total,
                            chainsaw_info_value$op_cost_gas_total,
                            
                            forklift_info_value$op_cost_gas_total,
                            windmachine_info_value$op_cost_gas_total,
                            atv_info_value$op_cost_gas_total,
                            tractor_info_value$op_cost_gas_total),
      electric_savings = c("N/A",
                           pickup_info_value$electric_savings,
                           worktruck_info_value$electric_savings,
                           chainsaw_info_value$electric_savings,
                           
                           forklift_info_value$electric_savings,
                           windmachine_info_value$electric_savings,
                           atv_info_value$electric_savings,
                           tractor_info_value$electric_savings)
    )
    
    
    demand_graph_df %>%
      arrange(desc(tool_name == "Electricity Bill"), desc(op_be)) %>%
      mutate(across(c(annual_kwh_tool,op_cost_electric, op_cost_gas), cumsum, .names = 'cumulative_{.col}'))
  })
  
  ### dynamic dataframe based on what is selected ###
  filtered_demand_df <- reactive({
    # Access the original data frame
    d_df <- demand_dataframe()
    
    # Retrieve the selected tools from the checkboxGroupInput
    selected_tools <- input$demand_choices
    
    # Filter the demand data frame based on checkboxGroupInput, keeping the "Electricity Bill" row
    filtered_df <- d_df %>%
      filter(tool_name == "Electricity Bill" | tool_name %in% selected_tools)
    
    # Sort and compute 
    filtered_df %>%
      arrange(desc(tool_name == "Electricity Bill"), desc(op_be)) %>%
      mutate(across(c(annual_kwh_tool, op_cost_electric, op_cost_gas, op_cost_electric_total, op_cost_gas_total, electric_savings), cumsum, .names = 'cumulative_{.col}'))
  })
  
  

  observe({
    filtered_demand_df_data <- filtered_demand_df()
    print(filtered_demand_df_data)
  })
  
  
  ## demand intersects grid line 
  demand_short_txt <- reactive ({
    demand_df <- filtered_demand_df()
    grid_charge <- grid_charge()
    excess_demand <- filter(demand_df, op_be < grid_charge)
    if (nrow(excess_demand) > 0) {
      message <- paste("This occurs when you electrify the following:", paste(excess_demand$tool_name, collapse=", "))
    } else {
      message <- "No tool has an operational cost per unit of energy less than the grid charge."
    }
    return(message)
    
  })
  
  output$demand_short_txt <- renderText({
    demand_short_txt()
  })
  
  
  
  ## new table for the report
  output$dynamic_demand_table2 <- renderTable({
    data2 <- filtered_demand_df()
    
    new_data2 <- data2[, c("tool_name", "op_be",
                           "op_cost_gas", "op_cost_electric",
                           "op_cost_gas_total", "op_cost_electric_total", 
                           "annual_kwh_tool", "electric_savings")]
    
    cols_to_convert <- c( "op_cost_gas", "op_cost_electric",
                          "op_cost_gas_total", "op_cost_electric_total", "electric_savings" )
    new_data2[cols_to_convert] <- lapply(new_data2[cols_to_convert], function(x) as.numeric(x))
    
    names(new_data2) <- c("Tool Name", "Operational Breakeven ($/kWh)",
                          "Annual Operational Cost - Gas ($)", "Annual Operational Cost - Electric ($)",
                          "10-yr Operational Cost - Gas ($)",  "10-yr Cost - Electric ($)", 
                          "Annual kWh", "Electric Savings")
    str(new_data2)
    
    new_data2[, -1] <- lapply(new_data2[, -1], function(x) {
      if (is.numeric(x)) {
        formatC(x, format = "f", big.mark = ",", digits = 2)
      } else {
        return ("ugh")
      }
    })
    
    new_data2
  })
  
  ## Render dynamic total text based on slider values for demand ##
  total_energy_demand_value <- reactive({
    demand_dataframe <- filtered_demand_df()
    total_demand <- sum(demand_dataframe$annual_kwh_tool)
    total_demand  # Return the total energy demand directly
  })
  
  output$total_energy_demand_text <- renderText({
    total_energy_demand_text_value <- total_energy_demand_value()
    formatted_demand_txt <- format(total_energy_demand_text_value, big.mark = ",", digits = 2, nmall = 0)
    total_energy_demand_text <- paste("\u2022 Total Energy Demanded:", formatted_demand_txt, "kWh")
    total_energy_demand_text
  })
  
  
  ## dyanmic text for each stage
  
  ####### demand stage text 2nd tab #######
  
  output$electricity_bill_text <- renderText({
    d_df <- demand_dataframe()
    annual_kwh <- d_df$annual_kwh_tool[1]
    formatted_annual_kwh <- format(annual_kwh, big.mark = ",", digits = 2, nsmall = 0)
    paste("\u2022 Current energy demanded from electricity bill:", formatted_annual_kwh, "kWh/year")
  })
  
  # Define a function to generate formatted text
  generate_demand_text <- function(index) {
    filtered_demand_df<- filtered_demand_df()
    
    # Check if index is valid
    if (index >= 1 && index <= nrow(filtered_demand_df)) {
      demand_text <-  filtered_demand_df$annual_kwh_tool[index]
      formatted_demand_text <- format(demand_text, big.mark = ",", digits = 2, nsmall = 0)
      return(paste("\u2022 Additional energy demanded from electric",  filtered_demand_df$tool_name[index],":", formatted_demand_text, "kWh/year"))
    } else {
      return(NULL)  
    }
  }
  
  ## Electricity pie chart
  output$electricity_pie_chart <- renderPlotly({
    filtered_demand_df <- filtered_demand_df()
    
    # Check if there are any data to plot
    if (nrow(filtered_demand_df) > 0) {
      # Extract the relevant data
      labels <- filtered_demand_df$tool_name
      values <- filtered_demand_df$annual_kwh_tool
      formatted_values <- formatC(values, format = "d", big.mark = ",")
      text_labels <- paste( formatted_values, "kWh")
      
      # Plotly pie chart
      plot_ly(
        labels = labels,
        values = values,
        type = 'pie',
        text = text_labels,
        textposition = 'inside',
        marker = list(
          colors = c( '#337AB7', "#28B78D", '#E6C229', '#F17105', '#D11149', '#E08D79', '#D4B2D8', '#C9CAD9'), 
          line = list(color = '#FFFFFF', width = 1)
        ),
        hoverinfo = 'label+percent+value',
        hovertemplate = '%{label}<br>%{percent:.2%}<br>%{value:,} kWh<extra></extra>', 
        insidetextfont = list(color = '#FFFFFF', size = 12),
        outsidetextfont = list(color = '#FFFFFF', size = 12),
        title = "Energy Demand Breakdown"
      )
    } else {
      # Return an empty plot if there's no data
      
      plot_ly() %>%
        layout(
          annotations = list(
            text = "Please choose demand options from the right",
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 20, color = "red")
          ),)
    }
  })
  
  
  
  
  
  # Loop through indices 2 to 8 and create reactive outputs
  lapply(2:8, function(i) {
    output_name <- paste0("demand_text", i)
    
    output[[output_name]] <- renderText({
      generate_demand_text(i)
    })
  })
  
  
  
  ############# Grid charge in kwh based on provider ########################
  #######################################################################
  
  grid_charge <- reactive({
    switch(input$electricity_provider, 
           "Xcel" = 0.065, # Xcel <-- average-ish between summer and winter
           "Grand Valley Power" = 0.10, # Grandvalley
           "DMEA" = 0.11, # DMEA
           "Empire Electric" = 0.105, # Empire
           "Other" = input$other_grid_kwh 
    )
  })
  
  output$kwh_rate_text <- renderText({
    paste("The cost per kWh rate for", input$electricity_provider, ":")
  })
  
  output$kwh_rate_text2 <- renderText({
    paste(grid_charge())
  })
 
  ############## Annual Sunlight based on County ################################
  #############################################################################
  annual_sunlight <- reactive({
    county_sunlight  <- switch(input$county,
                               "Mesa" = 6.5, # Mesa
                               "Delta" = 6.0, # Delta
                               "Montezuma" = 5, # Montezuma
                               "Montrose" = 6.4, # Montrose
                               "Other" = 6 # other  <-- Uses an average across the counties
    )
    county_sunlight * 365
  })
  
  ############### Costs, Output and $/kwh for the four areas one can install Solar ###############
  ###############################################################################################
  
  ########### Rooftop Solar ########
  rooftop_pv_info <- reactive({
    initial_grid_charge <- grid_charge()
    
    # n_years <- 25
    # grid_increase_factor <- 1.02
    # energy_decrease_factor <- 0.995
    # discount_rate <- 0.03
    
    rooftop_area <- input$rooftop  # available square feet for the solar to go on
    rooftop_panel_sqft <- 17.8  # sqft one panel takes up
    rooftop_panel_output <- 400.00  # output in watts from a single panel
    total_panels <- rooftop_area / rooftop_panel_sqft  # total panels that can fit on the rooftop
    rooftop_capacity_watts <- total_panels * rooftop_panel_output  # total output in watts
    rooftop_capacity_kw <-   rooftop_capacity_watts / 1000
    rooftop_kwh_annual <- (rooftop_capacity_kw * annual_sunlight()) 
    rooftop_kwh_total <- sum(rooftop_kwh_annual*(1-0.005)^(0:25))
    
    rooftop_cost_install_total <- rooftop_capacity_watts * 3.50  # total output in watts times the cost per watt of such an installation
    
    rooftop_pt <- initial_grid_charge * (1.02^ (0:25))
    rooftop_qt <- rooftop_kwh_annual * ( 0.995 ^ (0:25 ))
    # Calculate rt for each year
    rooftop_rt <- rooftop_pt * rooftop_qt
    # Discount rt to present value
    rooftop_rdt <- sum(rooftop_rt * (1 / (1 + 0.03) ^ (0:25)))
    rooftop_npv <- rooftop_rdt - rooftop_cost_install_total
    rooftop_cost_total <- rooftop_cost_install_total + ((rooftop_cost_install_total* 0.05 ) * 24)
    rooftop_cost_per_kwh <-  rooftop_cost_total / rooftop_kwh_total ## includes installation cost and 5% of intial installation costs for 35 years
    
    print(rooftop_pt)
    print(rooftop_qt)
    print(rooftop_rt)
    print(rooftop_rdt)
    
    list(
      rooftop_npv = rooftop_npv,
      rooftop_total_panels = total_panels,
      rooftop_capacity_kw = rooftop_capacity_kw, 
      rooftop_kwh_annual = rooftop_kwh_annual,
      rooftop_kwh_total = rooftop_kwh_total, 
      rooftop_cost_per_kwh = rooftop_cost_per_kwh,
      rooftop_install_cost = rooftop_cost_install_total
    )
  })
  
  ########### Solar over idleland ########
  idleland_pv_info <- reactive({
    initial_grid_charge <- grid_charge()
    idle_area <- input$idle * 43560.00  # 1 acre = 43560 square feet
    idle_panel_sqft <- 5.4* (3.3+7.9) # adding row spacing to the width of the panel 
    idle_panel_output <- 400.00  # watts
    total_panels_idle <- idle_area / idle_panel_sqft
    idle_capacity_watts<- total_panels_idle * idle_panel_output # total power capacity in W
    idle_capacity_kw <- idle_capacity_watts / 1000 # total power capcity in kW
    idleland_kwh_annual <-  idle_capacity_kw * annual_sunlight()
    idleland_kwh_total <- sum(idleland_kwh_annual*(1-0.005)^(0:25))
    
    idleland_cost_install_total <-  idle_capacity_watts * 2.00
    
    # Determining npv
    idleland_pt <- initial_grid_charge * (1.02^ (0:25 ))
    idleland_qt <- idleland_kwh_annual * ( 0.995 ^ (0:25 ))
    # Calculate rt for each year
    idleland_rt <- idleland_pt * idleland_qt
    # Discount rt to present value
    idleland_rdt <- sum(idleland_rt * (1 / (1 + 0.03) ^ (0:25 )))
    idleland_npv <- idleland_rdt - idleland_cost_install_total
    
    idleland_cost_total <- idleland_cost_install_total + ((idleland_cost_install_total* 0.05 ) * 24)
    idleland_cost_per_kwh <- idleland_cost_total / idleland_kwh_total
    
    print(idleland_pt)
    print(idleland_qt)
    print(idleland_rt)
    print(idleland_rdt)
    
    list(
      idleland_npv = idleland_npv,
      idleland_total_panels = total_panels_idle,
      idleland_capacity_kw  = idle_capacity_kw, 
      idleland_kwh_annual = idleland_kwh_annual,
      idleland_kwh_total = idleland_kwh_total, 
      idleland_cost_per_kwh = idleland_cost_per_kwh,
      idleland_install_cost = idleland_cost_install_total
    )
  })
  
  ####### Solar Irrigation Pumps ############
  
  irrigation_pv_info <- reactive({
    # Define default values
    irrigation_kwh_annual <- 0
    irrigation_cost_total <- 0
    irrigation_cost_per_kwh <- 0
    irrigation_kwh_total <- 0
    irrigation_capacity_kw <- 0
    irrigation_install_cost <- 0
    irrigation_npv <- 0
    
    if (input$irrigation != "No") {
      output_per_panel <- 400 
      cost_per_watt <- 2.00
      kwh_input_irrigation_pump <- input$well_kwh
      #monthly_grid_charge <- grid_connect_charge()
      annual_sunlight_hours <- annual_sunlight()
      initial_grid_charge <- grid_charge()
      
      # Calculate energy per panel per year in kWh
      annnual_kwh_per_panel <- (output_per_panel * annual_sunlight_hours) / 1000
      
      # Calculate number of panels needed
      irrigation_number_of_panels <- floor(kwh_input_irrigation_pump / annnual_kwh_per_panel)
      
      irrigation_capacity_kw <- (irrigation_number_of_panels * output_per_panel)/1000
      
      # Calculate actual kWh produced per year
      irrigation_kwh_annual <- (irrigation_number_of_panels * output_per_panel * annual_sunlight_hours) / 1000
      
      # Calculate total kWh produced over 25 years accounting for degradation
      irrigation_kwh_total <- sum(irrigation_kwh_annual * (1 - 0.005)^(1:25))
      
      # Calculate initial installation cost
      irrigation_install_cost <- irrigation_number_of_panels * output_per_panel * cost_per_watt
      
      # Calculate total cost over 25 years
      irrigation_cost_total <- irrigation_install_cost + ((irrigation_install_cost * 0.05) * 24)
      
      # Calculate cost per kWh
      irrigation_cost_per_kwh <- irrigation_cost_total / irrigation_kwh_total
      
      # Determining npv
      irrigation_pt <- initial_grid_charge * (1.02^ (0:(25 - 1)))
      irrigation_qt <- irrigation_kwh_annual * ( 0.995 ^ (0:(25 - 1)))
      # Calculate rt for each year
      irrigation_rt <- irrigation_pt * irrigation_qt
      # Discount rt to present value
      irrigation_rdt <- sum(irrigation_rt * (1 / (1 + 0.03) ^ (0:(25 - 1))))
      irrigation_npv <- irrigation_rdt - irrigation_install_cost
      
      
    }
    
    # Return the list
    list(
      irrigation_capacity_kw =  irrigation_capacity_kw, 
      irrigation_kwh_annual = irrigation_kwh_annual,
      irrigation_kwh_total = irrigation_kwh_total,
      irrigation_npv = irrigation_npv,
      irrigation_cost_per_kwh = irrigation_cost_per_kwh, 
      irrigation_install_cost = irrigation_install_cost
    )
  })
  
  
  
  ####### Agrivoltaics - Solar over peach ##############
  peach_pv_info <- reactive({
    initial_grid_charge <- grid_charge()
    # Define default values 
    peach_npv <- 0
    peach_kwh_annual <- 0
    peach_cost_per_kwh <- 0
    peach_capacity_kw <- 0
    peach_kwh_total <- 0
    peach_install_cost <- 0
    
    
    if (input$acres_peach != 0) {
      # Proceed with calculations
      # Monthly_grid_charge <- grid_connect_charge()
      peach_area <- input$acres_peach
      peach_kwh_annual <- input$voltaic_annual_kwh
      peach_install_cost <-  input$voltaic_kwh *input$voltaic_annual_kwh
      peach_cost_per_kwh <- input$voltaic_kwh
      peach_capacity_kw <-  input$voltaic_annual_kwh/annual_sunlight()
      peach_kwh_total <- sum(input$voltaic_annual_kwh * (1 - 0.005)^(1:25))
      
      discount_single_value <- function(value, discount_rate, years) {
        discounted_value <- value / (1 + discount_rate)^years
        return(discounted_value)
      }
      
      discounted_voltaiceffect <- sapply(0:24, function(year) {
        discount_single_value(input$voltaic_effect, discount_rate = 0.03, years = year)
      })
      
      ## Determining npv
      peach_pt <- initial_grid_charge * (1.02^ (0:(25 - 1))) + discounted_voltaiceffect
      peach_qt <- peach_kwh_annual * ( 0.995 ^ (0:(25 - 1)))
      # Calculate rt for each year
      peach_rt <- peach_pt * peach_qt
      # Discount rt to present value
      peach_rdt <- sum(peach_rt * (1 / (1 + 0.03) ^ (0:(25 - 1))))
      peach_npv <- peach_rdt - peach_install_cost

    }
    
    # Return the list
    list(
      peach_npv = peach_npv,
      peach_capacity_kw = peach_capacity_kw,
      peach_kwh_total = peach_kwh_total,
      peach_kwh_annual = peach_kwh_annual,
      peach_cost_per_kwh = peach_cost_per_kwh,
      peach_install_cost = peach_install_cost
    )
  })
  
  
  
  
  ######################################################################################################
  # Ranking the different PV arrays by their cost per kwh
  
  solar_supply_df <- reactive({
    peach_info <- peach_pv_info()
    rooftop_info <- rooftop_pv_info()
    idleland_info <- idleland_pv_info()
    irrigation_info <- irrigation_pv_info()
    all_dfm <- data.frame(
      type = c('Rooftop PV', 'Idle land PV', 'Irrigation Pump PV', 'Agrivoltaics'),
      npv = c(rooftop_info$rooftop_npv,
              idleland_info$idleland_npv,
              irrigation_info$irrigation_npv,
              peach_info$peach_npv),
      
      install_cost = c(rooftop_info$rooftop_install_cost,
                       idleland_info$idleland_install_cost,
                       irrigation_info$irrigation_install_cost, 
                       peach_info$peach_install_cost),
      
      kw_capacity = c(rooftop_info$rooftop_capacity_kw, 
                      idleland_info$idleland_capacity_kw,
                      irrigation_info$irrigation_capacity_kw,
                      peach_info$peach_capacity_kw),
      
      annual_kwh =  c(rooftop_info$rooftop_kwh_annual, 
                      idleland_info$idleland_kwh_annual,
                      irrigation_info$irrigation_kwh_annual,
                      peach_info$peach_kwh_annual),
      
      total_kwh = c(rooftop_info$rooftop_kwh_total, 
                    idleland_info$idleland_kwh_total,
                    irrigation_info$irrigation_kwh_total,
                    peach_info$peach_kwh_total),
      
      cost_per_kwh = c(rooftop_info$rooftop_cost_per_kwh, 
                       idleland_info$idleland_cost_per_kwh,
                       irrigation_info$irrigation_cost_per_kwh,
                       peach_info$peach_cost_per_kwh)
      
    ) %>% arrange(cost_per_kwh)  # cheapest <- (all_dfm %>% arrange(cost_per_kwh))[1,]
  })
 
  
  # New supply df based on input selection
  supply_select_df <- reactive({
    s_df <- solar_supply_df()
    
    filtered_supply <- s_df %>%
      filter(type %in% input$supply_choices) %>%
      arrange(cost_per_kwh) %>%
      mutate(across(c(npv, kw_capacity, annual_kwh, total_kwh, install_cost), cumsum, .names = 'cumulative_{.col}'))
  })
  

  
  ### where supply line exceeds grid charge ###
  excess_supply_text <- reactive({
    solar_supply_df_data <- supply_select_df()
    grid_charge <- grid_charge()
    print(grid_charge)
    excess_supply <- filter(solar_supply_df_data, cost_per_kwh > grid_charge)
    if (nrow(excess_supply) > 0) {
      message <- paste( "This occurs when you install:", paste(excess_supply$type, collapse=", "))
    } else {
      message <- "No supply exceeds grid charge."
    }
    return(message)
  })
  
  output$excess_supply_message <- renderText({
    excess_supply_text()
  })
  
  
  
  ## new table
  output$supply_dynamic_table <- renderTable({6
    data <- supply_select_df()
    #rename columns
    new_data <- data[, c("type","install_cost",  "npv", "kw_capacity", "annual_kwh", "total_kwh", 
                         "cost_per_kwh", "cumulative_install_cost", "cumulative_npv", "cumulative_kw_capacity", 
                         "cumulative_annual_kwh", "cumulative_total_kwh")]
    
    names(new_data) <- c("Type","Installation Cost ($)", "NPV ($)", "kW Capacity", "Annual kWh", "Lifetime kWh", 
                         "Cost per kWh ($/kWh)", " Cumulative Installation Cost ($)", "Cumulative NPV ($)", "Cumulative kW Capacity", 
                         "Cumulative Annual kWh", "Cumulative Lifetime kWh")
    
    new_data[, -1] <- lapply(new_data[, -1], function(x) {
      if (is.numeric(x)) {
        formatC(x, format = "f", big.mark = ",", digits = 2)
      } else {
        x
      }
    })
    new_data
  })
  
  ######## dynamic value
  annual_cumulative_energy <- reactive({
    s_df <- supply_select_df()
    
    if (nrow(s_df) == 0 || all(is.na(s_df$annual_kwh))) {
      annual_cumulative_energy_value <- 0
    } else {
      annual_cumulative_energy_value <- sum(s_df$annual_kwh, na.rm = TRUE)
    }

    return(annual_cumulative_energy_value)
  })
  
  output$annual_cumulative_energy_text <- renderText({
    annual_cumulative_energy_value <- annual_cumulative_energy()
    annual_cumulative_energy_value <- formatC(annual_cumulative_energy_value, format = "f", big.mark = ",", digits = 2)
    annual_cumulative_energy_text <- paste("\u2022 Cumulative annual kWh supplied from all systems:", annual_cumulative_energy_value)
    return(annual_cumulative_energy_text)
  })
  
  # Render the dynamic annual_cumulative_energy_text
  output$supply_kwh_text1 <- renderText({
    s_df <- supply_select_df()
    if (nrow(s_df) == 0 || s_df$annual_kwh[1] == 0) {
      supply_kwh_text1 <- "\u2022 Select PV array options from the checkbox to see energy supply details."
    } else {
      supply_kwh_text1 <- ""
    }
    return(supply_kwh_text1)
  })
  
  
  # Function to generate supply kWh text
  generate_supply_text <- function(index) {
    s_df <- supply_select_df()
    if (nrow(s_df) > 0 && length(s_df$annual_kwh) >= index) {
      supply_kwh_value <- s_df$annual_kwh[index]
      supply_kwh_value <- formatC(supply_kwh_value, format = "f", big.mark = ",", digits = 2) 
      supply_kwh_text <- paste("\u2022 Annual kWh supplied from", s_df$type[index], "PV:", supply_kwh_value)
      return(supply_kwh_text)
    } else {
      return(NULL)  # Return NULL if index is out of range or s_df is empty
    }
  }
  
  # Loop through indices 2 to 4 and create reactive outputs
  lapply(2:4, function(i) {
    output_name <- paste0("supply_kwh_text", i)
    
    output[[output_name]] <- renderText({
      generate_supply_text(i)
    })
  })
  
  
  ######### SUpply pie chart ########
  output$supply_pie_chart <- renderPlotly({
    s_df <- supply_select_df()
    
    # Check if there are any data to plot
    if (nrow(s_df) > 0) {
      # Extract the relevant data
      labels <- s_df$type
      values <- s_df$annual_kwh
      
      # Format the kWh values with commas and two decimal places
      formatted_values <- formatC(values, format = "f", big.mark = ",", digits = 2)
      
      # Combine labels with formatted kWh values for display
      text_labels <- paste(formatted_values, "kWh")
      
      # Create the Plotly pie chart
      plot_ly(
        labels = labels,
        values = values,
        type = 'pie',
        text = text_labels,
        textposition = 'inside',
        marker = list(
          colors = c('#3498DB', '#2ECC71', '#E74C3C', '#F39C12'), 
          #'#9B59B6', '#1ABC9C'
          line = list(color = '#FFFFFF', width = 1)
        ),
        hoverinfo = 'label+percent+value',
        hovertemplate = '%{label}<br>%{percent:.2%}<br>%{value:,} kWh<extra></extra>',
        insidetextfont = list(color = '#FFFFFF', size = 12),
        outsidetextfont = list(color = '#FFFFFF', size = 12),
        title = "Energy Supply Breakdown"
      )
    } else {
      # Return an empty plot if there's no data
      plotly_empty()
      
    }
  })
  
  
  
  ####### Dynamic Energy Balance text ######
  energy_balance <- reactive({
    supply_value <- annual_cumulative_energy()
    demand_value <- total_energy_demand_value()
    if (is.na(supply_value) || is.na(demand_value)) {
      return(NA)
    }
    energy_balance_val <- supply_value - demand_value
    return(energy_balance_val)
  })
  
  output$energy_balance <- renderText({
    energy_balance_val <- energy_balance()
    if (is.na(energy_balance_val)) {
      return("Energy Balance: N/A")
    }
    formatted_energy_balance <- format(energy_balance_val, big.mark = ",", digits = 2, nsmall = 2)
    paste("\u2022 Net Energy Supply:", formatted_energy_balance, "kWh")
  })
  
  
  
  ####### 120% of demand text ####### 
  output$energy_imbalance_text <- renderText({
    supply_value<- annual_cumulative_energy()
    demand_value <- total_energy_demand_value()
    
    if (supply_value > 1.2 * demand_value) {
      warning_message <- "\u2022 Note: Supply exceeds 120% of demand! This option may require regulatory approval." ### change the message
    } else {
      warning_message <- NULL
    }
    
  })
  
  ##### Tab 3 Supply limitations text #####
  output$supply_exceed_demand_txt <- renderText({
    s_df <- supply_select_df()
    supply_value <- annual_cumulative_energy()
    demand_value <- total_energy_demand_value()
    
    if (supply_value > 1.2 * demand_value) {
      exceeding_types <- paste(s_df$type[s_df$cumulative_total_kwh > 1.2 * demand_value], collapse = ", ")
      tab3_message <- paste("The following PV array options result in supply that exceeds 120% demand:", exceeding_types)
    } else {
      tab3_message <- "Supply does not exceed 120% of demand."
    }
    tab3_message
  })
  
  
  ####### dynamic text for total cost #######
  total_pv_cost <- reactive({
    data <- supply_select_df()
    total_pv_cost_value <- sum(data$total_cost)
    print("total pv cost")
    print(total_pv_cost_value)
    return(total_pv_cost_value)
  })
  
  total_install_cost <- reactive({
    s_data <- supply_select_df()
    total_install_cost_value <- sum(s_data$install_cost)
    if (!is.numeric(total_install_cost_value)) {
      stop("Error: total_install_cost_value is not numeric")
    }
    return(total_install_cost_value)
  })
  
  #################################################
  ############# Dynamic table #####################
  output$econ_table <- renderTable({
    supply_select_df_data <- supply_select_df()
    total_cost <- total_pv_cost()
    total_installation_cost <- total_install_cost()
    chosen_percent <- 1 - (input$cyo_percent / 100)
    initial_kwh_output <- sum(supply_select_df_data$annual_kwh)
    initial_grid_charge <- grid_charge()  
    
    install_30_value <- total_installation_cost * 0.7
    install_70_value <- total_installation_cost * 0.3
    install_ptc_value <- total_installation_cost - (initial_kwh_output * 0.0275) 
    install_cyo_value <- total_installation_cost * chosen_percent
    # formatted installation cost
    installation_cost_form <- format(total_installation_cost, big.mark = ",",  nsmall = 2, scientific = FALSE)
    install_30 <- format(install_30_value, big.mark = ",", nsmall = 2, scientific = FALSE)
    install_70 <- format(install_70_value, big.mark = ",", nsmall = 2, scientific = FALSE)
    install_ptc <- format(install_ptc_value, big.mark = ",", nsmall = 2, scientific = FALSE)
    install_cyo <- format(install_cyo_value, big.mark = ",", nsmall = 2, scientific = FALSE)
    
    
    n_years <- 25
    ptc_years <- 10
    ptc_per_kWh <- 0.0275
    grid_increase_factor <- 1.02
    energy_decrease_factor <- 0.995
    discount_rate <- 0.03
    
    # Calculate pt and qt for each year
    pt <- initial_grid_charge * (grid_increase_factor ^ (0:n_years )) # expected rpice of electricity each year assuming 2% increase
    qt <- initial_kwh_output * (energy_decrease_factor ^ (0:n_years )) # expected energy production per year
    # Calculate rt for each year --> revenue for each year
    rt <- pt * qt
    # Discount future revenues to present value
    rdt <- sum(rt * (1 / (1 + discount_rate) ^ (0:n_years)))
    avg_rdt <- rdt / 25
    
    # rdt under the ptc
    # Vectors to store yearly values
    grid_price <- numeric(n_years)
    kWh_output <- numeric(n_years)
    revenue <- numeric(n_years)
    discounted_revenue <- numeric(n_years)
    
    # Calculate values for each year
    for (year in 0:n_years) {
      grid_price[year] <- initial_grid_charge * (grid_increase_factor)^(year )
      kWh_output[year] <- initial_kwh_output * (energy_decrease_factor)^(year )
      revenue[year] <- grid_price[year] * kWh_output[year]
      # Add credit for the first 10 years
      if (year <= ptc_years) {
        revenue[year] <- revenue[year] + ptc_per_kWh * kWh_output[year]
      }
      # Calculate discounted revenue
      discounted_revenue[year] <- revenue[year] / (1 + discount_rate)^(year )
    }
    # Sum the discounted revenue over the total period
    rdt_ptc <- sum(discounted_revenue)
    avg_rdt_ptc <-sum(discounted_revenue)/ 25
    #cumulative_npv
    base_npv <-rdt - total_installation_cost
    npv_30 <- rdt - install_30_value
    npv_70 <- rdt - install_70_value
    npv_ptc <- rdt_ptc - total_installation_cost
    npv_cyo <- rdt - install_cyo_value
    # formatted NPV
    base_npv_form <- format(base_npv, big.mark = ",",  nsmall = 0, scientific = FALSE)
    npv_30_form <- format(npv_30, big.mark = ",", nsmall = 0, scientific = FALSE)
    npv_70_form <- format(npv_70, big.mark = ",",  nsmall = 0, scientific = FALSE)
    npv_ptc_form <- format(npv_ptc, big.mark = ",",  nsmall = 0, scientific = FALSE)
    npv_cyo_form <- format(npv_cyo, big.mark = ",",  nsmall = 0, scientific = FALSE)
    
    # Calculate table variables
    base_pb <- ceiling (total_installation_cost/ avg_rdt)
    pb_30 <-  ceiling(install_30_value / avg_rdt)
    pb_70 <-  ceiling(install_70_value / avg_rdt)
    pb_ptc <- ceiling(total_installation_cost / avg_rdt_ptc)
    pb_cyo <- ceiling(install_cyo_value/ avg_rdt)
    # formatted pb
    base_pb_form <- format(base_pb, digits = 3, nsmall = 0)
    pb_30_form <- format(pb_30, nsmall = 0)
    pb_70_form <- format(pb_70, digits = 3, nsmall = 0)
    pb_ptc_form <- format(pb_ptc, digits = 3, nsmall = 0)
    pb_cyo_form <- format(pb_cyo, digits = 3, nsmall = 0)
    
    
    base_roi <- ((rdt - total_installation_cost) / total_installation_cost) * 100
    roi_30 <- ((rdt - install_30_value) / install_30_value) * 100
    roi_70 <- ((rdt - install_70_value) / install_70_value) * 100
    roi_ptc <- ((rdt - install_ptc_value) / install_ptc_value) * 100
    roi_cyo <- ((rdt - install_cyo_value) / install_cyo_value) * 100
    # formatted roi
    base_roi_form <- format(base_roi, digits = 4, nsmall = 1)
    roi_30_form <-  format(roi_30, digits = 4, nsmall = 1)
    roi_70_form <-  format(roi_70, digits = 4, nsmall = 1)
    roi_ptc_form <-format(roi_ptc, digits = 4, nsmall = 1)
    roi_cyo_form <-format(roi_cyo, digits = 4, nsmall = 1)
    
    data <- matrix(c(
      installation_cost_form, install_30, install_70, install_ptc, install_cyo, 
      base_npv_form, npv_30_form, npv_70_form, npv_ptc_form, npv_cyo_form,
      base_pb_form, pb_30_form, pb_70_form, pb_ptc_form, pb_cyo_form,
      base_roi_form, roi_30_form, roi_70_form, roi_ptc_form, roi_cyo_form
    ), nrow = 4, ncol = 5, byrow = TRUE)
    
    colnames(data) <- c("Base Scenario (No Discounts)",
                        "With 30% of Installation Costs Covered (e.g. ITC)",
                        "With 70% of Installation Costs Covered (e.g. REAP)",
                        "With 2.75 cents given per kWh produced (e.g. PTC)",
                        paste("With", input$cyo_percent, "% of Installation Costs Covered"))
    
    rownames(data) <- c("Total Installation Cost ($)", "Net Present Value ($)", "Payback Period (Years)", "ROI (%)")
    
    data
  }, include.rownames = TRUE)
  
  
  
  ############################################
  ################ Main Graphic ##############
  
  
  ## 120% Line Button
  show_120_line <- reactiveVal(FALSE)
  observeEvent(input$toggle_button, {
    show_120_line(!show_120_line())
  })
  
  ## Solar Advantage Button
  solar_advantage_area <- reactiveVal(FALSE)
  observeEvent(input$solar_advantage_button, {
    solar_advantage_area(!solar_advantage_area())
  })
  
  ## Electric Advantage Button
  show_electric_advantage <- reactiveVal(FALSE)
  observeEvent(input$electric_advantage_button, {
    show_electric_advantage(!show_electric_advantage())
  })
  
  ## Electrical Disadvantage 
  show_electric_disadvantage <- reactiveVal(FALSE)
  observeEvent(input$electric_disadvantage_button, {
    show_electric_disadvantage(!show_electric_disadvantage())
  })
  
  output$main_graphic <- renderPlotly({
    grid_charge_value <- as.numeric(grid_charge())
    cost_share_proportion <- reactive({
      1 - (input$cyo_percent / 100)
    })
    
    # New supply df
    supply_df2 <- supply_select_df()
    supply_zero_val_row <- supply_df2[1, ]
    supply_zero_val_row$cumulative_annual_kwh <- input$energy-100 # Ensure no zero values for log10
    #supply_zero_val_row$cumulative_annual_kwh <- 0
    supply_zero_val_row$cost_per_kwh <- supply_df2$cost_per_kwh[1]
    supply_df2 <- supply_df2 %>% add_row(supply_zero_val_row, .before = 1)
    print(supply_df2)
    
    # Adding a row to the demand df to have it start from 0
    demand_dataframe_df <- filtered_demand_df()
    zero_val_row <- demand_dataframe_df[1, ]
    zero_val_row$cumulative_annual_kwh_tool <- input$energy-100 # Ensure no zero values for log10
    # zero_val_row$cumulative_annual_kwh_tool <- 0
    demand_dataframe_df <- demand_dataframe_df %>% add_row(zero_val_row, .before = 1)
    print(demand_dataframe_df)
    
    # Calculating 120% of demand
    max_demand_x <- max(demand_dataframe_df$cumulative_annual_kwh_tool, na.rm = TRUE)
    x_120 <- max_demand_x * 1.2
    
    # Create the base plot
    p <- ggplot() +
      # Supply steps
      geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh, color = "Supply"),
                data = supply_df2, direction = "vh", linewidth = 1.5) +
      geom_point(aes(x = cumulative_annual_kwh, y = cost_per_kwh, color = "Supply",
                     text = paste("kWh Supplied:", scales::comma(cumulative_annual_kwh, accuracy = 0.01),
                                  "<br> Cost per kWh:", scales::dollar(cost_per_kwh, accuracy = 0.01),
                                  "<br> Type:", type)),
                 data = supply_df2, size = .5) +
      # Demand steps
      geom_step(aes(x = cumulative_annual_kwh_tool, y = op_be, color = "Demand"),
                data = demand_dataframe_df, direction = "vh", linewidth = 1.5) +
      geom_point(aes(x = cumulative_annual_kwh_tool, y = op_be, color = "Demand",
                     text = paste("kWh Demanded:", scales::comma(cumulative_annual_kwh_tool, accuracy = 0.01),
                                  "<br> Operational Breakeven:", scales::dollar(op_be, accuracy = 0.01),
                                  "<br> Tool:", tool_name
                                  #  ,"<br> As long as the grid price is lower than", op_be, "there is an electric advantage."
                     )),
                 data = demand_dataframe_df, size = .5) +
      
      # Grid charge line
      geom_hline(aes(yintercept = grid_charge_value, color = "Grid Charge"),
                 linetype = "solid", linewidth = 1.5)
    
    # Additional layers
    if (solar_advantage_area() && nrow(supply_df2) > 1) {
      # Filter the data to include only rows where a solar advantage exists
      supply_df2_filtered <- supply_df2 %>%
        dplyr::filter(cost_per_kwh <= grid_charge_value)
      
      # Check if the filtered data has any rows
      if (nrow(supply_df2_filtered) > 0) {
        p <- p +
          geom_rect(aes(xmin = cumulative_annual_kwh,
                        xmax = dplyr::lag(cumulative_annual_kwh),
                        ymin = cost_per_kwh,
                        ymax = grid_charge_value),
                    data = supply_df2_filtered, fill = "light blue", alpha = 0.4)
      }
    }
    
    if (show_electric_advantage()) {
      p <- p +
        geom_rect(aes(xmin = dplyr::lag(cumulative_annual_kwh_tool), xmax = cumulative_annual_kwh_tool,
                      ymin = ifelse(op_be >= grid_charge_value, grid_charge_value, NA), ymax = op_be),
                  data = demand_dataframe_df, fill = "light green", alpha = 0.4)
    }
    if (show_electric_disadvantage()) {
      p <- p +
        geom_rect(aes(xmin = dplyr::lag(cumulative_annual_kwh_tool), xmax = cumulative_annual_kwh_tool,
                      ymin = ifelse(op_be <= grid_charge_value, grid_charge_value, NA), ymax = op_be),
                  data = demand_dataframe_df, fill = "red", alpha = 0.4)
    }
    if (show_120_line()) {
      p <- p +
        geom_vline(aes(xintercept = x_120, color = "Demand 120"),
                   linetype = "solid", linewidth = 1.2)
    }
    
    # Additional layers based on the financing options
    if (input$subsidy_option == "30% External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * 0.7, direction = "vh", color = "30% External Financing"),
                         data = supply_df2, linetype = "dashed", linewidth = 1.5)
    } else if (input$subsidy_option == "70% External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * 0.3, direction = "vh", color = "70% External Financing"),
                         data = supply_df2, linetype = "dotted", linewidth = 1.5)
    } else if (input$subsidy_option == "Chosen External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * cost_share_proportion(), direction = "vh", color = "Chosen External Financing"),
                         data = supply_df2, linetype = "dotted", linewidth = 1.5)}
    
    # Finalize plot with labels and theme
    p <- p + labs(title = "Graph of Supply and Demand Potential",
                  x = "Cumulative Annual kWh",
                  y = "$/kWh") +
      scale_x_continuous(
        labels = function(x) format(x, scientific = FALSE, big.mark = ","),
        #limits = c(input$energy-100, NA)
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 20, vjust = 1.5)) +
      scale_color_manual(name = "Line Legend",
                         values = c("Supply" = "blue",
                                    "Demand" = "black",
                                    "Grid Charge" = "dark green",
                                    "Demand 120" = "red", 
                                    "30% External Financing" = "purple", 
                                    "70% External Financing" = "orange", 
                                    "Chosen External Financing" = "light green"))
    
    # Convert to plotly object with tooltips
    p_plotly <- plotly::ggplotly(p, tooltip = "text")
    p_plotly
    
  })
  
  
  
  
  
  
  
  ######################## tab 3 report graph ##################################
  output$main_graphic2 <- renderPlotly({
    grid_charge_value <- as.numeric(grid_charge())
    cost_share_proportion <- reactive({
      1 - (input$cyo_percent / 100)
    })
    
    # New supply df
    supply_df2 <- supply_select_df()
    supply_zero_val_row <- supply_df2[1, ]
    supply_zero_val_row$cumulative_annual_kwh <-  input$energy-100
    supply_zero_val_row$cost_per_kwh <- supply_df2$cost_per_kwh[1]
    supply_df2 <- supply_df2 %>% add_row(supply_zero_val_row, .before = 1)
    print(supply_df2)
    
    # New demand df
    demand_dataframe_df <- filtered_demand_df()
    zero_val_row <- demand_dataframe_df[1, ]
    zero_val_row$cumulative_annual_kwh_tool <- input$energy-100 
    demand_dataframe_df <- demand_dataframe_df %>% add_row(zero_val_row, .before = 1)
    print(demand_dataframe_df)
    
    # Calculating 120% of demand
    max_demand_x <- max(demand_dataframe_df$cumulative_annual_kwh_tool, na.rm = TRUE)
    x_120 <- max_demand_x * 1.2
    
    # Create the base plot
    p <- ggplot() +
      # Supply steps
      geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh, color = "Supply"),
                data = supply_df2, direction = "vh", linewidth = 1.5) +
      geom_point(aes(x = cumulative_annual_kwh, y = cost_per_kwh, color = "Supply",
                     text = paste("kWh Supplied:", scales::comma(cumulative_annual_kwh, accuracy = 0.01),
                                  "<br> Cost per kWh:", scales::dollar(cost_per_kwh, accuracy = 0.01),
                                  "<br> Type:", type)),
                 data = supply_df2, size = .5) +
      # Demand steps
      geom_step(aes(x = cumulative_annual_kwh_tool, y = op_be, color = "Demand"),
                data = demand_dataframe_df, direction = "vh", linewidth = 1.5) +
      geom_point(aes(x = cumulative_annual_kwh_tool, y = op_be, color = "Demand",
                     text = paste("kWh Demanded:", scales::comma(cumulative_annual_kwh_tool, accuracy = 0.01),
                                  "<br> Operational Breakeven:", scales::dollar(op_be, accuracy = 0.01),
                                  "<br> Tool:", tool_name
                                  #  ,"<br> As long as the grid price is lower than", op_be, "there is an electric advantage."
                     )),
                 data = demand_dataframe_df, size = .5) +
      
      # Grid charge line
      geom_hline(aes(yintercept = grid_charge_value, color = "Grid Charge"),
                 linetype = "solid", linewidth = 1.5)
    
    # Additional layers
    if (solar_advantage_area() && nrow(supply_df2) > 1) {
      p <- p +
        geom_rect(aes(xmin = cumulative_annual_kwh, xmax = dplyr::lag(cumulative_annual_kwh),
                      ymin = ifelse(cost_per_kwh <= grid_charge_value, cost_per_kwh, NA),
                      ymax = grid_charge_value),
                  data = supply_df2, fill = "light blue", alpha = 0.4)
    }
    if (show_electric_advantage()) {
      p <- p +
        geom_rect(aes(xmin = dplyr::lag(cumulative_annual_kwh_tool), xmax = cumulative_annual_kwh_tool,
                      ymin = ifelse(op_be >= grid_charge_value, grid_charge_value, NA), ymax = op_be),
                  data = demand_dataframe_df, fill = "light green", alpha = 0.4)
    }
    if (show_electric_disadvantage()) {
      p <- p +
        geom_rect(aes(xmin = dplyr::lag(cumulative_annual_kwh_tool), xmax = cumulative_annual_kwh_tool,
                      ymin = ifelse(op_be <= grid_charge_value, grid_charge_value, NA), ymax = op_be),
                  data = demand_dataframe_df, fill = "red", alpha = 0.4)
    }
    if (show_120_line()) {
      p <- p +
        geom_vline(aes(xintercept = x_120, color = "Demand 120"),
                   linetype = "solid", linewidth = 1.2)
    }
    
    # Additional layers based on the financing options
    if (input$subsidy_option == "30% External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * 0.7, direction = "vh", color = "30% External Financing"),
                         data = supply_df2, linetype = "dashed", linewidth = 1.5)
    } else if (input$subsidy_option == "70% External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * 0.3, direction = "vh", color = "70% External Financing"),
                         data = supply_df2, linetype = "dotted", linewidth = 1.5)
    } else if (input$subsidy_option == "Chosen External Financing") {
      p <- p + geom_step(aes(x = cumulative_annual_kwh, y = cost_per_kwh * cost_share_proportion(), direction = "vh", color = "Chosen External Financing"),
                         data = supply_df2, linetype = "dotted", linewidth = 1.5)}
    
    # Finalize plot with labels and theme
    p <- p + labs(title = "Graph of Supply and Demand Potential",
                  x = "Cumulative Annual kWh",
                  y = "$/kWh") +
      scale_x_continuous(
        labels = function(x) format(x, scientific = FALSE, big.mark = ","),
        #limits = c(input$energy-100, NA)
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 20, vjust = 1.5)) +
      scale_color_manual(name = "Line Legend",
                         values = c("Supply" = "blue",
                                    "Demand" = "black",
                                    "Grid Charge" = "dark green",
                                    "Demand 120" = "red", 
                                    "30% External Financing" = "purple", 
                                    "70% External Financing" = "orange", 
                                    "Chosen External Financing" = "light green"))
    
    # Convert to plotly object with tooltips
    p_plotly <- plotly::ggplotly(p, tooltip = "text")
    p_plotly
    
  })
  
  
  
  
  
 
  ####################################################
  ################# Tab 3 - Server ###################
  ####################################################
  
  
  # Screenshot captures results when the button is clicked
  observeEvent(input$download_report, {
    screenshot(
      id = "tabsset",
      filename = "summary_report",
      download = TRUE,
      scale = 2
    ) })
  
  
  ##### Questionnaire Summary #####
  # general farm questions summary 
  output$summary_county <- renderText({input$county})
  output$summary_electricity_provider <- renderText({input$electricity_provider })
  output$summary_other_grid_kwh <- renderText({ input$other_grid_kwh })
  output$summary_peach <- renderText({ input$peach })
  output$summary_energy <- renderText({
    format(input$energy, big.mark = ",")
  })
  output$summary_annualsun <- renderText({annual_sunlight()})
  output$summary_irrigation <- renderText({ input$irrigation })
  output$summary_well_kwh <- renderText({ 
    formatted_kwh <- format(as.numeric(input$well_kwh), big.mark = ",", scientific = FALSE)
    formatted_kwh
  })
  
  
  
  # tools summary
  output$summary_tool_1 <- renderText({ input$tool_1 })
  output$summary_tool_2 <- renderText({ input$tool_2 })
  output$summary_tool_3 <- renderText({ input$tool_3 })
  output$summary_tool_4 <- renderText({ input$tool_4 })
  output$summary_tool_5 <- renderText({ input$tool_5 })
  output$sum_small_tools <- renderText({ input$tool_1  + input$tool_3 + input$tool_4 + input$tool_5})
  
  # tractor summary
  output$summary_tool_6 <- renderText({ input$tool_6 })
  output$summary_tool_7 <- renderText({ input$tool_7 })
  output$summary_tool_8 <- renderText({ input$tool_8 })
  output$summary_tool_9 <- renderText({ input$tool_9 })
  output$sum_tractor_implements <- renderText({input$tool_6 + input$tool_7 + input$tool_8 + input$tool_9 + input$tool_10})
  
  # vehicle summary
  output$summary_vehicle_1 <- renderText({ input$vehicle_1 })
  output$summary_vehicle_2 <- renderText({ input$vehicle_2 })
  output$sum_vehicles <- renderText({ 
    format(input$vehicle_1 + input$vehicle_2, big.mark = ",")})
  
  # supply-side summary
  output$summary_rooftop <- renderText({
    format(input$rooftop, big.mark = ",")})
  output$summary_idle <- renderText({ input$idle })
  output$summary_acres_peach <- renderText({input$acres_peach })
  
  
  
  ### disclaimer code ###
  clicked <- reactiveVal(FALSE)
  
  # Observe button clicks and set clicked to TRUE
  observeEvent(input$q_result_bttn, {
    clicked(TRUE)
  })
  
  observeEvent(input$sumtables_bttn, {
    clicked(TRUE)
  })
  
  observeEvent(input$graph_q1_bttn, {
    clicked(TRUE)
  })
  
  observeEvent(input$add_info_bttn, {
    clicked(TRUE)
  })
  
  # Render disclaimer conditionally
  output$disclaimer_ui <- renderUI({
    if (clicked()) {
      fluidRow(
        column(12, p(style = "color:red;", 
                     "Disclaimer! Results provided from this tool are intended for estimation purposes only and
           should not be the only resource used in final decision making.")
        )
      )
    }})
  
  ###########################################################
  ################ downloading the report ###################
  output$download_report<- downloadHandler(
    filename = "farm_solar_tool_results.html", 
    content = function(file) {
      tempReport <- file.path(tempdir(),  "farm_solar_tool_results.Rmd")
      file.copy("farm_solar_tool_results.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown ::render(tempReport, output_file = file, 
                         params = params,
                         envir = new.env(parent = globalenv()))
    }
  )
  observeEvent(input$screenshot1, {
    screenshot()
  })
}


# Run the application
shinyApp(ui, server)
