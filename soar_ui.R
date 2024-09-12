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

# css styling
css <- "
  .no-bold {
    font-weight: normal;
  }
  
  .highlight {
	          background-color: yellow;
	        }


"


# Define UI
ui <- fluidPage(
  useShinyjs(), 
  shiny::tags$head(shiny::tags$style(HTML(css))),
  titlePanel("S.O.A.R"),
  div(style = "text-align: left;", 
      h2("Solar Orchard Analysis and Recommendation Tool", style = "font-size: 17px;")),
  dashboardBody(
    
    
    tabsetPanel(
      id = "tabset",
      
      ##############################################################
      ################## Intro Tab UI ##################################
      tabPanel("Introductory Page",
               
               h2("SOAR: Solar Orchard Analysis and Recommendation Tool", style ="text-align:center;"),
               br(),
               
               fluidRow(
                 column(
                   width = 4, offset = 1,
                   tags$img(id = "logo", src = "CSU-Symbol.png", class = "center",
                            style = "height: 300px; display: block; margin-right:10px;")
                   
                 ),
                 
                 column(
                   width = 6,
                   tags$p("Welcome to CSU's Solar Orchard Decision Support Tool. This resource helps peach growers evaluate
           their energy needs, potential solar power generation, and the economic feasibility of photovoltaic (PV)
           installations on their orchards.",
                          class = "centertext",
                          align = "center",
                          style = "font-size: 22px; margin-bottom: 20px;"),
                   
                   
                   tags$p("This tool is meant to give an estimation of results to guide further exploration and
           consultation with solar energy installers and should not be used for final-decision making
           or as a substitute for professional advice.",
                          class = "centertext",
                          align = "center",
                          style = "font-size: 22px; margin-bottom: 20px;")
                 )),
               
               
               fluidRow(
                 column(
                   width = 10, offset = 1,
                   tags$p("Developed by: Samantha Bryan, Dana Hoag, Brad Tonnessen, Dan Mooney and Amit Munshi",
                          class = "centertext",
                          align = "center",
                          style = "font-size: 18px;")
                 )
               ),
               br(), 
               fluidRow(
                 column(
                   width = 10, offset = 1,
                   actionBttn("start_tool_btn", "Go To App", style = "unite", color = "success", size = "md", block = TRUE)
                 )
               )
               
      ),
      
      ##############################################################
      ################## Tab 1 UI ##################################
      
      tabPanel("Questionnaire", 
               
               
               h2("Introduction"),
               
               p(tags$span("This tool is used to determine if solar power is a good option for you. 
                 The tool begins by evaluating your current energy consumption and potential energy production through a questionnaire.
                 After completing the questionnaire, you'll have access to an 
                 inter   active modeling analysis page that allows you to adjust energy supply and demand projections 
                 over time to identify an energy solution that aligns with your requirements and budget. 
                 Ultimately, this tool empowers you to make an informed decision on whether adopting solar power 
                 is a viable and cost-effective solution tailored to your unique energy needs and circumstances." , 
                           style = "font-size: 1.2em;")),
               h3("Directions"),
               p(tags$span("1. Complete the Questionairre. The tool comes with preset values based on a 100-acre farm, but you can adjust these to fit the specifics of your own farm.", style = "font-size: 1.2em;")),
               p(tags$span("2. Proceed to the 'Analysis' tab to assess your potential energy supply and demand using interactive modeling tools. ", style = "font-size: 1.2em;")), 
               p(tags$span("3. Go to the 'Report' tab to retrieve a full summary of the tool results.", style = "font-size: 1.2em;")),
               
               
               br(),
               actionBttn("start_survey_btn", "Start Questionnaire", style = "unite", color = "primary", size = 
                            "sm"),
               
               #The Questions
               conditionalPanel(
                 condition = "output.showSurvey",
                 br(),
                 h3("General Farm Questions"),
                 fluidRow(
                   column(6, p("Which Western Colorado County are you located in?")),
                   column(4, selectInput("county", label = NULL,
                                         choices = list ("Mesa" , "Delta" , "Montezuma" , "Montrose" , "Other" ),
                                         selected = "Mesa"))
                 ),
                 fluidRow(
                   column(6, p("Which company is your electricity provider?")),
                   column(4, selectInput("electricity_provider", label = NULL, 
                                         choices = list ("Xcel", "Grand Valley Power", "DMEA", "Empire Electric", "Other"),
                                         selected = "Xcel")),
                   conditionalPanel(
                     condition = "input.electricity_provider == 'Other'", 
                     column(4, numericInput("other_grid_kwh", HTML("<span class = 'no-bold'>What is the charge per kwh you pay ($/kwh)?</span>"), value = 0)), 
                         )
                 ),
                 fluidRow(
                   column(6, "How many acres of peaches do you have in total?"),
                   column(4, numericInput("peach", NULL, value = 30)), # 30
                   column(2, "acres")
                 ),
                 fluidRow(
                   column(6, "How much electricity do you use in a year in kwh?"),
                   column(4, numericInput("energy", NULL, value = 12000)), # 12000
                   column(2, "kwh")
                 ),
                 fluidRow(
                   column(4, "Do you irrigate using pumps?"),
                   column(3, radioButtons("irrigation", label = NULL, choices = c("Yes", "No"), selected = "Yes")),
                 ),
                 conditionalPanel(
                   condition = "input.irrigation == 'Yes'",
                   column(6, numericInput("well_kwh", HTML("<span class='no-bold'>How many kwh are used in irrigation for a year?</span>"), value = 5000)) #5000
                 ),
                 
                 fluidRow( p("")  ),
                 
                 fluidRow(
                   column(6, "How much do you pay for gas per gallon?"),
                   column(4, numericInput("gas_cost", NULL, value = 3.25)), # 3.25
                   column(2, "$/gal")
                 ),
                 fluidRow(
                   column(6, "How much do you pay for diesel per gallon?"),
                   column(4, numericInput("diesel_cost", NULL, value = 3.5)), #3.50
                   column(2, "$/gal")
                 ),
                 
                 br(),
                 
                 fluidRow(
                   column(12, h3("Tools and Vehicles"), align = "left"),
                 ),
                 
                 fluidRow(
                   ###### Small Tools #########
                   column(4, wellPanel(
                     h4("Farm Tools"),
                     p("Indicate how many hours per year you use the following tools."),
                     numericInput("tool_1", "Chainsaw:", value = 150), #150
                     numericInput("tool_3", "Forklift:", value = 400), #400
                     numericInput("tool_4", "Windmachine:", value = 35), #35
                     numericInput("tool_5", "ATV:", value = 385) #385
                     
                   )),
                   
                   ###### Tractor Implements ######
                   column (4, wellPanel(
                     h4("Tractor Implements"),
                     p("Indicate how many hours per year you use the following tractor implements."),
                     numericInput("tool_6", "PTO Pruner:", value =0),
                     numericInput("tool_7", "Mower:", value = 320), #300
                     numericInput("tool_8", "Sprayer:", value = 260), #300
                     numericInput("tool_9", "Flail Mower:", value = 100), #80
                     numericInput("tool_10", "Tractor w/o Implements:", value = 300) #300
                   )),
                   
                   ###### Vehicles #####
                   column (4, wellPanel(
                     h4("Vehicles"),
                     p("Indicate how many miles per year you use the following vehicles."),
                     numericInput("vehicle_1", "Pickup Truck:", value = 20000), #20000
                     numericInput("vehicle_2", "Small Work Truck (ex. F650):", value = 10000) #10000
                   ))
                 ),
                 
                 ###### Supply Side Buildings #####
                 h3("Available Land and Buildings Space for Solar"),
                 fluidRow(
                   column(6, "How many square feet of rooftop space do you have available for a solar array?"),
                   column(4, numericInput("rooftop", NULL, value = 1000)) ,  ## 1000
                   column(2, "sq.ft")
                 ),
                 fluidRow(
                   column(6, "How many acres of idle land do you have available for a solar array?"),
                   column(4, numericInput("idle", NULL, value = .5, min = 0)), ## 1
                   column(2, "acres")
                 ),
                 fluidRow(
                   column(6, "How many acres of peaches do you have available to place a solar array above (agrivoltaics)?"),
                   column(4, numericInput("acres_peach", NULL, value = 0.1, min = 0)), ## 0.1
                   column(2, "acres")
                 ),
                 conditionalPanel(
                   condition = "input.acres_peach != 0",
                   fluidRow(
                     column(6, numericInput("voltaic_kwh", HTML("<span class='no-bold'>What is the cost per kWh to install an agrivoltaic system on your orchard?</span>"), value = 0.09)), # 0.09
                     column(6, numericInput("voltaic_effect", HTML("<span class='no-bold'>What is the net increase or decrease of an agrivoltaic system on your yield of peaches per year ($)</span>"), value = 0)),
                     column(6, numericInput("voltaic_annual_kwh", HTML("<span class='no-bold'>What is the expected annual kWh output 
                                                                       from the agrivoltaic array?)</span>"), value = 5000)) #5000
                   )
                   
                 ),
                 
                 
                 br(),
                 actionBttn("end_questions", "Finish Questionnaire", style = "unite", color = "default", size = "sm"),
                 br(),
                 br(),
               ),  
               
      ),
      
      ##############################################################
      ################## Analysis Tab UI ##################################
      
      tabPanel("Analysis", 
               #### Tab 2 Directions ####
               fluidRow(
                 column(width = 12,
                        div(style = "display: flex; align-items: flex-end; gap: 10px;",
                            h3("Directions", id = "directions", style = "margin: 0;"),
                            actionLink("direction_btn", "Show/Hide Info", 
                                       style = "text-decoration: underline; color: #347bb7; font-size: 0.9em;")
                        )
                 )
               ),
               br(),
               
               #The Questions
               conditionalPanel(
                 condition = "output.showDirection",
                 fluidRow(
                   column(width = 12,
                          p(tags$span("1. The interactive graph below is made up of the following components to show the potential energy supply and demand on your farm over time and under different financing scenarios.", style = "font-size:1.3em;"))
                   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("a. Demand Options ", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_dslider_btn", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "condition = output.showDslider",
                            p(tags$span(
                              "The list of demand-side options shows motorized farm tools and vehicles that can be electrified. The checked options are then displayed on the graph 
                           with the options ordered from the most financially advantageous gas/diesel to electric conversion. Check the boxes to 
                               explore different electrification scenarios and see the impact on your future energy demanded in kWh.",
                              style = "font-size:1.2em; display: block; margin-left: 20px;"
                            ))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("b. Supply Options", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_sslider_btn", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "condition = output.showSslider",
                            p(tags$span(
                              "The list of supply options shows the different locations solar can be placed on a farm. The checked options are then displayed on the graph 
                           ordered from the lowest to the highest cost per kilowatt-hour ($/kWh). Check the boxes to 
                               explore different installation scenarios and see the impact on potential energy supplied in kWh.",
                              style = "font-size:1.2em; display: block; margin-left: 20px;  "
                            ))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("c. Financing Drop Down Menu", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_dropdown_btn", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "condition = output.showdropdown",
                            p(tags$span(
                              " Clicking on the drop down menu yay",
                              style = "font-size:1.2em; display: block; margin-left: 20px;  " 
                            ))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("d. Solar Advantage Button ", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_more_btn", "Info")) ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "condition = output.showSAbutton",
                            p(tags$span(
                              "This button highlights the portion on the graph where installing additional PV solar 
                          panels would result in lower costs per kilowatt-hour than drawing electricity from the grid 
                          at the current utility rates. Once the highlighted area ends, it indicates that further solar 
                          installation would no longer be cost-effective, and drawing from the grid would be more economic.
                          Click the button to show and hide this area.",
                              style = "font-size:1.2em; display: block; margin-left: 20px;"
                            ))  )   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("d. Electric Advantage Button ", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_electric_btn", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "condition = output.showEAbutton",
                            p(tags$span(
                              "This button highlights the portion of the graph where using electric tools would be more cost-effective 
                      than their gas counterparts, regardless of whether the electricity is drawn from the grid or from solar panels. Click the button to show or hide this area.",
                              style = "font-size:1.2em; display: block; margin-left: 20px;"
                            ))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("e. Electric Disadvantage Button ", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("ed_button", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "output.showEDbutton",
                            p(tags$span(
                              "This button highlights the portion of the graph where using electric tools would be less cost-effective 
                    than their gas counterparts, regardless of whether the electricity is drawn from the grid or from solar panels.
                         Click the button to show or hide this area."
                              
                              
                            ), style = "font-size:1.2em; display: block; margin-left: 20px;")
                            
                          )
                   )
                 ),
                 
                 
                 fluidRow(
                   column(width = 4,
                          p(tags$span("f. 120% Demand Button ", style = "font-size:1.2em; display: block; margin-left: 20px;"))
                   ),
                   column(width = 4,
                          actionLink("show_120demand_btn", "Info")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = "output.moredemand_btn",
                            p(tags$span(
                              "This button highlights the 120% demand line on the graph, which represents the maximum energy 
                          supply allowed for non-energy distributors under current PV regulations. Any supply exceeding 120% of your 
                          farm's demand requires individual exemption. Click the button to show or hide this line",
                              style = "font-size:1.2em; display: block; margin-left: 20px;"
                            ))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          p(tags$span("2.To further interact with analyze the graph, hover the mouse over the lines and toggle the legend items on and off by clicking on them in the legend.",  style = "font-size:1.3em;"))
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          p(tags$span("3. Analyze the cost and return table located below the graph to assess the economic impact of your chosen electrification and
                                  solar installation scenarios under different financing options.",  style = "font-size:1.3em;"))
                   )
                 ),
               ), 
               br(), 
               #### The Graph UI ####
               plotlyOutput("main_graphic"),
               br(), 
               fluidRow(
                 column(9, 
                        
                        p(tags$span("Click the buttons below to show or hide the following aspects", style = "font-size:1.0em; display: block; font-weight:bold;" )),
                        actionButton("toggle_button", "120% Demand Line"),
                        tags$style(HTML(".clicked-button {
            background-color: lightorange;
            color: black; }")),
                        actionButton("solar_advantage_button", "Solar Advantage", class = "initial-button"),
                        actionButton("electric_advantage_button", "Electric Advantage", class = "initial-button"),
                        actionButton("electric_disadvantage_button", "Electric Disadvantage", class = "initial-button")
                 ),
                 column(3, 
                        selectInput("subsidy_option", "Choose a Financing Option:",
                                    choices = c("None", "70% External Financing", "30% External Financing", "Chosen External Financing"),
                        )
                 )
               ),
               
               uiOutput("graphic_demand"),
               uiOutput("solar_text"),
               uiOutput("electric_adv_text"),
               uiOutput("electric_disadv_text"),
               
               
               tags$style(HTML("
            .initial-button {
              background-color: transparent;
              color: black;
            }
            .solar-button {
              background-color: lightblue;
              color: black;
            }
            .electric-button {
              background-color: lightgreen;
              color: black;
            }
            .disadv-button {
              background-color: #ffcccc;
              color: black;
            }
            .demand-button {
              background-color: red;
              color: black;
            }
          ")),
               
               br(),
               br(), 
               
               
               
               fluidRow(
                 column(6,awesomeCheckboxGroup(inputId = "demand_choices",
                                               label = "Electrification Options",
                                               choices = c("Chainsaw", "Forklift", "Windmachine", "ATV", "Tractor", "Pickup Truck", "Work Truck"),
                                               inline = TRUE,
                                               status = "primary"
                 ) ),
                 column(6,awesomeCheckboxGroup(inputId = "supply_choices",
                                               label = "PV Location Options",
                                               choices = c("Rooftop PV", "Idle land PV", "Irrigation Pump PV", "Agrivoltaics"),
                                               inline = TRUE,
                                               status = "primary")),
                 
         
                 
               ),
               
               
               fluidRow( # adjusts the placement of things
                 
        
                 column(6,
                        strong("Demand Summary"),
                        textOutput("total_energy_demand_text"),
                        plotlyOutput("electricity_pie_chart", width = "400px", height = "400px")),
                 
                 
                 
                 column(6, 
                        strong("Supply Summary"),
                        
                        # Dynamic text output based on supply slider value
                        textOutput("annual_cumulative_energy_text"),
                        textOutput("supply_kwh_text1"), 
                        plotlyOutput("supply_pie_chart", width = "400px", height = "400px"),
          
                 )
               ),
               
               
               br(),
               fluidRow(
                 column(6, 
                        p("")),
                 column(6,
                        strong("Net Solar Energy Supply"),
                        
                        textOutput("energy_balance"),
                        textOutput("energy_imbalance_text")
                 )
               ),
               br(), 
               
               fluidRow(
                 column(12, align = "center", style = "text-align: center; margin-top: 50px;",
                        h3("Cost and Return Table"),
                        tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;")
                 )
               ),
               
               tableOutput("econ_table"),
               fluidRow(
                 column(6,
                        numericInput("cyo_percent", label= "Chose the cost share of PV to be externally covered:", min = 0, max = 100, value = 80)
                        
                 )),
               
               fluidRow(
                 column(12,
                        tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;"))
               ),
               
               tableOutput("tool_table"), 
               br(),
               actionBttn("go_to_tab3", "Go to the 'Report' tab", style = "unite", color = "primary", size = "sm"),
               p(""),
               br(), 
               
               
               
      ), 
      
      ##############################################################
      ################## Report Tab UI ##################################
      
      
      tabPanel("Report",
               br(), 
               fluidRow(
                 column(
                   width = 12,
                   style = "display: flex; justify-content: center; align-items: center;",
                   actionBttn("q_result_bttn", "Questionnaire Results Summary", style = "stretch", color = "success")
                 )
               ),
               
               conditionalPanel(
                 condition = "output.showq_result_bttn",
                 
                 
                 h4 ("General Farm Information"),
                 fluidRow(
                   column(4, p("County:")),
                   column(6, textOutput("summary_county"))
                 ),
                 
                 fluidRow(
                   column(4, p("Electricity Provider:")),
                   column(6, textOutput("summary_electricity_provider"))
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.electricity_provider == 'Other'",
                     column(4, p("Grid Charge per kWh ($/kWh):")),
                     column(6, textOutput("summary_other_grid_kwh"))
                   ), 
                   conditionalPanel(
                     condition = "input.electricity_provider != 'Other'",
                     column(4, textOutput("kwh_rate_text")),
                     column(6, textOutput("kwh_rate_text2"))
                   )
                 ),
       
                 
                 fluidRow(
                   column(4, p("Acres of Peaches:")),
                   column(6, textOutput("summary_peach"))
                 ),
                 
                 fluidRow(
                   column(4, p("Annual Electricity Usage (kWh/year):")),
                   column(6, textOutput("summary_energy"))
                 ),
                 
                 fluidRow(
                   column(4, p("Annual Sunlight per Year:")),
                   column(6, textOutput("summary_annualsun"))
                 ),
                 
                 fluidRow(
                   column(4, p("Irrigate Using Wells:")),
                   column(6, textOutput("summary_irrigation"))
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.irrigation == 'Yes'",
                     column(4, p("KWh used in irrigation per year:")),
                     column(6, textOutput("summary_well_kwh"))
                   )
                   
                 ),
                 h4("Annual Tool and Vehicle Usage"), 
                 
                 fluidRow(
                   column(4, p("Total Tool Operational Hours Per Year:")),
                   column(6, textOutput("sum_small_tools"))
                 ),
                 
                 fluidRow(
                   column(4, p("Total Tractor Implement Hours Per Year:")),
                   column(6, textOutput("sum_tractor_implements"))
                 ),
                 
                 fluidRow(
                   column(4, p("Total Miles Driven by Farm Vehicles:")),
                   column(6, textOutput("sum_vehicles"))
                 ),
                 
                 h4("Available PV Space"),
                 fluidRow(
                   column(4, p("Rooftop PV Area:")),
                   column(6, textOutput("summary_rooftop"))
                 ),
                 fluidRow(
                   column(4, p("Idleland PV Acreage:")),
                   column(6, textOutput("summary_idle"))
                 ),
                 fluidRow(
                   column(4, p("Agrivoltaic PV Acreage:")),
                   column(6, textOutput("summary_acres_peach"))
                 ),
               ), 
               
               br(), 
               
               fluidRow(
                 column(
                   width = 12,
                   style = "display: flex; justify-content: center; align-items: center;",
                   actionBttn("sumtables_bttn", "Supply and Demand Summary Tables", style = "stretch", color = "success")
                 )
               ),
               
               conditionalPanel(
                 condition = "output.showsumtables_bttn",
                 fluidRow(
                   column(
                     width = 12, offset = 1,
                     p(style = "margin-top: 20px;",
                       "The following Supply and Demand Summary tables display the results under the scenario where no external financing is considered."),
                   )
                   
                 ),
                 
                 fluidRow(
                   column(12, align = "center", style = "text-align: center; margin-top: 50px;",
                          h3("Supply-Side Summary Table"),
                          tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;")
                   )
                 ),
                 tableOutput("supply_dynamic_table"),
                 
                 
                 fluidRow(
                   column(12,
                          tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;"))
                 ),
              
                 
                 fluidRow(
                   column(12, align = "center", style = "text-align: center; margin-top: 50px;",
                          h3("Demand-Side Summary Table"),
                          tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;")
                   )
                 ),
                 tableOutput("dynamic_demand_table2"),
                 fluidRow(
                   column(12,
                          tags$hr(style = "border: 1px solid black; width: 100%; margin: 15px auto;"))
                   
                 ),
               ),
               
               br(),
               fluidRow(
                 column(
                   width = 12,
                   style = "display: flex; justify-content: center; align-items: center;",
                   actionBttn("graph_q1_bttn", "How to Interpret the Graph", style = "stretch", color = "success")
                 )
               ),
               
               conditionalPanel(
                 condition = "output.showgraph_q1_bttn",
                 br(),
                 plotlyOutput("main_graphic2"),
                 br(),
                 strong("Where does the supply line cross the grid-charge line?"),
                 p("The intersection of the supply line and the grid-charge line indicates where you should no longer install solar, as this means it costs more to install solar than to draw electricity from the grid."),
                 textOutput("excess_supply_message"), 
                 
                 br(), 
                 strong("Where does the demand line cross the grid-charge line?"),
                 p("The intersection of the demand line and the grid-charge line indicates where you should no longer electrify your tools and vehicles, as this means it costs more to electricity these items than use the gas-version."),
                 textOutput("demand_short_txt"),
                 
                 br(),
                 strong("Where does the supply line cross the 120% line?"),
                 p("The intersection of the supply line and the 120% line indicates where you should stop installing solar, as this means you will produce too much supply and will be classified as an energy producer."),
                 textOutput("supply_exceed_demand_txt"),
                 
                 br(), 
                 strong("Where do the financing option lines fall?"),
                 p("The financing options lines illustrate the extent of external financing needed to lower the supply line below the grid-charge line.
                Once this situation occurs, it indicates that there is a solar advantage and thus installing solar can lead to cost savings compared to only drawing from the grid."),
                 textOutput("external_fin_txt")
               ),
               
               br(), 
      
               fluidRow(
                 column(
                   width = 12,
                   style = "display: flex; justify-content: center; align-items: center;",
                   actionBttn("add_info_bttn", "Additional Information and Contacts Details", style = "stretch", color = "success")
                 )
               ),
               
               conditionalPanel(
                 condition = "output.showadd_info_bttn",
                 h4("Grid Contraints Considerations"),
                 tags$ul(
                   tags$li("The 120% demand line highlights a potential grid constraint that farmers may encounter. 
             Additionally, there may be a 50 kW capacity limit, which farmers can consider by reviewing the supply summary table under the 'kW Capacity' column."),
                   p(),
                   tags$li("Additional information regarding grid constraints can be found at the utility provider's websites linked below.",
                           tags$ul(
                             tags$li(tags$a(href = "https://co.my.xcelenergy.com/s/renewable/net-metering", "Xcel", target = "_blank")),
                             tags$li(tags$a(href = "https://gvp.org/solar", "Grand Valley Power", target = "_blank")),
                             tags$li(tags$a(href = "https://cdn.prod.website-files.com/65565f41cd0fa927524eb8ed/65cfeb8dca9518683c7e60f3_2023-09-26-net-metering-procedure.pdf", 
                                            "DMEA", target = "_blank")),
                             tags$li(tags$a(href = "https://eea.coop/net-meter-frequently-asked-questions-faqs", "Empire Electric", target = "_blank"))
                           )
                   )
                 ),
                 br(), 
                 h4("Assumptions"),
                 tags$ul(
                   tags$li("This tool does not consider any battery storage options and assumes the farmer will remain tied to the grid."),
                   p(),
                   tags$li("It is assumed that all farmers will opt for continuous rollover credits rather than an annual true-up.
                        With continuous rollover, any excess energy generated by solar panels is credited and carried over to the
                        next billing cycle, allowing farmers to offset future energy use at the grid price. With an annual true-up, excess energy credits are settled once a year at whole-sale electricity rates."),
                 ),
                 br(), 
                 h4("Limitations"),
                 tags$ul(
                   tags$li("The amount of energy output from the panels may vary based on location, the directions the panels are facing and daily weather patterns, only an estimation can be provided in this tool."),
                   p(), 
                   tags$li("Energy demand will fluctuate based on the brand of tools and vehicle as different brands have varying efficiencies."),
                   p(),
                   tags$li("This tool is meant to give an estimation of supply and demand and can not be used to replace any professional advice."),
                 ),
                 h4("Contact Information"), 
                 p("If you have any questions or concerns, please contact Brad Tonnessen at: Brad.Tonnessen@colostate.edu")
               ),

               
               uiOutput("disclaimer_ui"),
               
               actionButton("screenshot1", "Download Summary Report", icon = icon("download")),
               
               p(""),
               br()
               
               
               
      ),

      
      
      
      ###  action buttons to go to the top of the third tab ###
      tags$script(HTML("
    $(document).on('click', '#go_to_tab3', function() {
      $('a[data-value=\"Report\"]').click();
      $('html, body').animate({ scrollTop: 0 }, 'slow');
    });
    $(document).on('click', '#end_questions', function() {
      $('a[data-value=\"Analysis\"]').click();
      $('html, body').animate({ scrollTop: 0 }, 'slow');
    });
  "))
      
      
    )
  )
)


##### end of ui #####
