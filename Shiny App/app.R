

###########
library(tidyverse)
library(plotly)
library(gt)
library(lubridate)
library(shinythemes)
library(moonBook)
library(webr)
library(sjPlot)
library(DT)
library(ggrepel)



# Useful function for goodness of fit plots
remove_geom <- function(ggplot2_object, geom_type) {
    # Delete layers that match the requested type.
    layers <- lapply(ggplot2_object$layers, function(x) {
        if (class(x$geom)[1] == geom_type) {
            NULL
        } else {
            x
        }
    })
    # Delete the unwanted layers.
    layers <- layers[!sapply(layers, is.null)]
    ggplot2_object$layers <- layers
    ggplot2_object
}




dental_levels = c("less than 6 months", "between 6 and 12 months" ,"between 12 months and 2 years","more than 2 years",NA)
floss_levels = c("less than once a week","weekly" ,"most days","every day")
stress_levels = 0:10
season_levels = c("autumn", "winter", "spring","summer")


df = readr:: read_csv("cleaned_survey.csv") %>% 
    janitor::clean_names() %>% 
    mutate_if(is.character,.funs = tolower) %>% select(-x1)

df = df %>% 
    dplyr:: mutate(dental_frequency = factor(dental_frequency, levels = dental_levels),
           floss_frequency = factor(floss_frequency, levels = floss_levels),
           stress_level = factor(stress_level, levels = stress_levels),
           fav_season = factor(fav_season,levels=season_levels),
           postcode = as.character(postcode)
    )

colnames(df)[colnames(df)=="child_dog_cat_ownership"] = "child_dog_or_cat_ownership"
colnames(df)[colnames(df)=="fav_season"] = "favourite_season"
colnames(df)[colnames(df)=="fav_socials"] = "favourite_social_media"
colnames(df)[colnames(df)=="eye_col"] = "eye_colour"

colnames(df) = gsub("_"," ", colnames(df) ) %>% str_to_title()

categories = c("gender", "postcode","dental_frequency", "favourite_social_media", "child_dog_or_cat_ownership", "lives_with_parents","eye_colour","asthma_positive","favourite_season","floss_frequency","eyeware_positive", "dominant_hand","steak_preference","stress_level" )
categories = gsub("_"," ", categories) %>% str_to_title()

quant = setdiff(colnames(df), categories) %>% rev()




observed = df %>% dplyr:: filter(!is.na(`Covid Tests`)) %>% pull(`Covid Tests`)
sorted_obs = observed %>% unique() %>% sort()
H0_expected = (sorted_obs %>% dpois(lambda = mean(observed))*length(observed)) %>% t() %>%   as.data.frame()
colnames(H0_expected) = sorted_obs 
new_observed = table(observed)[1:2] %>% append(sum(table(observed)[3:8]))

adjusted_counts = c(0,1) %>% dpois(lambda = mean(observed)) 

H0_probs = adjusted_counts %>% append(1 - sum(adjusted_counts))

test_cov = chisq.test(new_observed, p = H0_probs)
test_cov$parameter = 1
test_cov$p.value = 1- pchisq(test_cov$statistic, df=1)

cov_tab = test_cov$expected %>% t() %>% as.data.frame() %>% round(1) %>%  gt() %>%
    tab_spanner(label = "Counts of the number of covid tests", columns = vars(`0`,`1`, V3) ) %>%
    cols_label(
        V3 = ">= 2"
    ) %>% 
    tab_source_note(md("Expected counts under the null hypothesis in the goodness of fit test")) 


###

library(shiny)


# Define UI for application
ui <- fluidPage(
    navbarPage("DATA2902: Hypothesis Testing" ,
       tabPanel("About",icon = icon("info-circle"),  htmlOutput("about")),       
        tabPanel("Dataset Exploration and Visualisations", icon = icon("bar-chart"), 
        # Sidebar with a slider input for number of bins 
            sidebarLayout(
              conditionalPanel("input.tabselected == 1",
                sidebarPanel(
                    selectizeInput( "plot","Choose Plot", c("comparative boxplot (two variables)","boxplot(one variable)", "barplot","histogram", "scatterplot")
                    ),
                    
                    conditionalPanel( condition = "input.plot == 'histogram'||input.plot == 'boxplot(one variable)'", selectizeInput( "q1", "Choose quantitative variable:", quant )
                    ),
                    
                    conditionalPanel( condition = "input.plot == 'barplot'", selectizeInput( "c1", "Choose category variable:", categories)
                    ),
                    conditionalPanel( condition = "input.plot == 'comparative boxplot (two variables)'", selectizeInput( "c2", "Choose category variable:", categories), 
                                      selectizeInput( "q2","Choose quantitative variable:", quant )
                    ),
                     conditionalPanel( condition = "input.plot == 'scatterplot'", 
                                      selectizeInput( "qx", "Choose x quantitative variable:", quant), selectizeInput( "qy","Choose y quantitative variable:", quant)
                    )
                )
              ),
                mainPanel(tabsetPanel(id = "tabselected",
                    tabPanel("Dataset",
                             DT:: dataTableOutput("datatable",width = "150%")#, width = "80%", height = "100%")
                    ),
                    tabPanel("Plots", value = 1,
                             div(style='height:600px; overflow-x: scroll',
                        plotly:: plotlyOutput("trialPlot", width = "80%", height = "100%")
                            )
                    )

                    
                    )
                )
            )
        ),
        tabPanel("Goodness of Fit Test",
             sidebarLayout(
                 
                 sidebarPanel(
                     selectizeInput("distribution", "Choose distribution (under H0):", c("uniform", 'poisson')
                     ),
                     conditionalPanel("input.distribution == 'uniform'",
                         selectizeInput( "uniform","Choose variable", categories %>% append("Covid Tests")
                         )
                    ),
                    conditionalPanel("input.distribution == 'poisson'",
                                     selectizeInput( "poisson","Choose variable", c("Covid Tests")
                                     )
                    ),
                    checkboxInput('obsbar', "Show observed counts?", value = FALSE, width = NULL),
                    checkboxInput('expected', "Show expected counts under the null hypothesis?", value = FALSE, width = NULL)
                    
                 ),
                 
                 mainPanel(
                     conditionalPanel("input.obsbar == 1",titlePanel("Observed Counts Table"), gt_output("goodness_obs")),#div( style='height:500px; overflow-x: scroll',plotlyOutput("goodness_bar"))
                     titlePanel("Are expected cell count assumptions met ?"),
                     verbatimTextOutput("a_met"),
                     conditionalPanel("input.expected == 1",titlePanel("Expected Counts Table"),gt_output("goodness_counts")),
                     titlePanel("Goodness of Fit test results"),
                     verbatimTextOutput("chi_goodness"),
                     plotly:: plotlyOutput("chiPlot1")
                 )
             )
        ),
        tabPanel("Test for Independence",
             sidebarLayout(
                 
                 sidebarPanel(
                     selectizeInput("ind_hom1", "Choose Category 1:", categories
                     ),
                     selectizeInput("ind_hom2", "Choose Category 2:", rev(categories)
                     ),
                     checkboxInput('obs_table', "Show observed counts?", value = FALSE, width = NULL),
                     checkboxInput('expected2', "Show expected counts under the null hypothesis?", value = FALSE, width = NULL)
                 ),
                 
                 mainPanel(
                     conditionalPanel("input.obs_table == 1",titlePanel("Observed Counts Table"),gt_output("obs_counts2")),
                     titlePanel("Are expected cell count assumptions met?"),
                     verbatimTextOutput("a_met2"),
                     conditionalPanel("input.expected2 == 1",titlePanel("Expected Counts Table"),gt_output("exp_counts_ind")),
                     titlePanel("Chi-squared Test for Independence results "),
                     verbatimTextOutput("independence"),
                     plotly:: plotlyOutput("chiPlot2")
                 )
             )
        ),
        tabPanel("T-tests",
                 sidebarLayout(
                     
                     sidebarPanel(
                         selectizeInput("test_choice","Choose test", c("One-sample t test", "Two-sample t test")
                             
                         ),
                         conditionalPanel("input.test_choice == 'One-sample t test'",
                             selectizeInput("one_samp", "Choose Quantitative Variable:", quant[quant != "Timestamps"]
                             ),
                             selectizeInput("assumption", "Choose Normality Test (assumption testing):", c("Shapiro-Wilkes","Normal Q-Q plot")
                             ),
                             selectizeInput("alternative", "Choose alternative:", c("greater","less", "two.sided")
                             ),
                             textInput("mu", "Select value of mu under the null hypothesis", value = 0, width = NULL, placeholder = NULL
                             )
                         ),
                         conditionalPanel("input.test_choice == 'Two-sample t test'",
                                          selectizeInput("two_samp", "Select category to dictate the 'two-samples':", categories
                                          ),
                                          selectizeInput("samp1", "Select 1st sample:", c()
                                          ),
                                          selectizeInput("samp2", "Select 2nd sample:", c()
                                          ),
                                          selectizeInput("samp_variable", "Choose Quantitative Variable:", quant[quant != "Timestamps"]
                                          ),
                                          selectizeInput("assumption2", "Choose Normality Test (assumption testing):", c("Shapiro-Wilkes","Normal Q-Q plot")
                                          ),
                                          selectizeInput("alternative2", "Choose alternative:", c("greater","less", "two.sided")
                                          ),
                                          checkboxInput('var', "Assume equal variance between samples?", value = FALSE, width = NULL),
                         )          
                     ),
                     
                     mainPanel(
                             conditionalPanel("input.test_choice == 'One-sample t test'",
                                 tabsetPanel(
                                     tabPanel("One Sample t-test",
                                         titlePanel("Shapiro-Wilkes Test for Normality"),
                                         verbatimTextOutput("ttest_assumption_print1"),
                                         conditionalPanel("input.assumption == 'Shapiro-Wilkes'",titlePanel("Shapiro-Wilkes Test"),
                                                         verbatimTextOutput("shapiro")
                                         ),
                                         conditionalPanel("input.assumption == 'Normal Q-Q plot'", titlePanel("Normal Q-Q plot"),
                                                          plotly:: plotlyOutput("qqPlot")
                                         ),
                                      titlePanel("One sample t-test results"),
                                      verbatimTextOutput("test_stat"),
                                      plotly:: plotlyOutput("tPlot")
                                         
                                     )
                                )
                             ),
                             conditionalPanel("input.test_choice == 'Two-sample t test'",
                                              tabsetPanel(
                                                  tabPanel("Two Sample t-test",
                                                           titlePanel("Shapiro-Wilkes Test for Normality)?"),
                                                           verbatimTextOutput("ttest_assumption_print2"),
                                                           conditionalPanel("input.assumption2 == 'Shapiro-Wilkes'", textOutput("shap2_1") ,
                                                                            verbatimTextOutput("shapiro2_1"), textOutput("shap2_2"), verbatimTextOutput("shapiro2_2") 
                                                           ),
                                                           conditionalPanel("input.assumption2 == 'Normal Q-Q plot'",titlePanel("Normal Q-Q plots"),
                                                                            plotly:: plotlyOutput("qqPlot2")
                                                           ),
                                                           titlePanel("Two sample t-test results"),
                                                           verbatimTextOutput("test_stat2"),
                                                           plotly:: plotlyOutput("tPlot2")
                                                  )
                                              )
                             )
                             
                        )
                 )
        )
        
    ),
    
    theme = shinytheme("flatly")
)



server <- function(input, output, session) {
    
#tab 1
    
    output$datatable <- renderDT({DT::datatable(df, options = list(
        pageLength=10, scrollX='400px'), filter = 'top')})
    
    
    output$trialPlot <- renderPlotly({
                                    if(input$plot == "barplot"){
                                    g = df %>% 
                                        ggplot() + geom_bar() + aes(x = .data[[input$c1]], fill =.data[[input$c1]] )  +
                                        labs(x = input$c1, fill='') + ggtitle(paste(input$c1, input$plot ) %>% str_to_title()) + theme_minimal()+ 
                                        theme(axis.text.x = element_text(angle = 45), legend.position = 'bottom')
                                    if (input$c1 == "Postcode"){
                                        ggplotly(g,width = 1600) %>%
                                            layout(legend=list(title=list(text= input$c1)))
                                    }else{
                                        ggplotly(g) %>%
                                            layout(legend=list(title=list(text= input$c1)))
                                    }
                                
                                        
                                    }else if(input$plot  == "histogram"){

                                    g = df %>% 
                                        ggplot() + geom_histogram() + aes(x = .data[[input$q1]], fill =.data[[input$q1]] )  +
                                        labs(x = input$q1, fill='') + ggtitle(paste(input$q1, input$plot ) %>% str_to_title())+ theme_minimal()+ 
                                        theme(axis.text.x = element_text(angle = 45), legend.position = 'bottom')
                                    
                                    ggplotly(g) %>%
                                        layout(legend=list(title=list(text= input$q1)))       
                                    
                                    }else if (input$plot == "boxplot(one variable)"){
                                       
                                    g = df %>% 
                                        ggplot() + geom_boxplot() + aes( y = .data[[input$q1]]) + aes(fill ="#FFDB6D") + labs(x = input$q1, y = ' ')+ ggtitle(paste(input$q1, "boxplot" ) %>% str_to_title()) + theme(legend.position = "none") + theme_minimal() + theme(axis.text.x = element_text(angle = 45), legend.position = 'none')
                                    ggplotly(g) 
                                    
                                    }else if (input$plot == "comparative boxplot (two variables)"){
                                    g = df %>% 
                                        ggplot() + geom_boxplot() + aes(x = .data[[input$c2]], y = .data[[input$q2]], 
                                                                               fill = .data[[input$c2]]) + ggtitle(paste(input$q2, "boxplot by", input$c2) %>% str_to_title()) + theme_minimal()+
                                        theme(axis.text.x = element_text(angle = 45), legend.position = 'bottom')
                                    if (input$c2 == "Postcode"){
                                        ggplotly(g,width = 1600)
                                    }else{
                                    ggplotly(g)
                                    }

                                    
                                    }else {
                                        g= df %>% ggplot() + geom_point() + aes(x = .data[[input$qx]], y = .data[[input$qy]], color = .data[[input$qy]]) + ggtitle(paste(input$qx,"vs", input$qy, input$plot ) %>% str_to_title())+ theme_minimal()+theme(axis.text.x = element_text(angle = 45), legend.position = 'bottom')
                                        ggplotly(g)
                                    }

        
                                             })
    
    

    
# Tab 2    
    
    goodness_hist = reactive({
        if(input$distribution == "uniform"){
            input$uniform
        }else if(input$distribution == "poisson"){
            input$poisson 
        }
    })
    
    
    # output$goodness_bar <- renderPlotly({
    #         g = df %>% 
    #             ggplot() + geom_bar() + aes(x = .data[[goodness_hist()]], fill =.data[[goodness_hist()]] ) + 
    #             theme(axis.text.x = element_text(angle = 45), legend.position = 'bottom') +
    #             labs(x = goodness_hist(), fill='') + ggtitle(paste(goodness_hist(), "barplot" ) %>% str_to_title()) + theme_minimal()
    #         
    #         if (goodness_hist() == "Postcode"){
    #             ggplotly(g,width = 1600) %>%
    #                 layout(legend=list(title=list(text= goodness_hist())))
    #         }else{
    #             ggplotly(g) %>%
    #                 layout(legend=list(title=list(text= goodness_hist())))
    #         }
    #     })
    
    output$goodness_obs = render_gt({
        if(input$distribution == "uniform"){
            dummy = df %>% pull(goodness_hist()) %>% table()  %>% rbind() %>% as.data.frame()
            rownames(dummy)[1] = "Frequency"
            data.table::setDT(dummy, keep.rownames = TRUE)[]
            dummy %>%  gt(rowname_col = "rn") %>%  tab_stubhead(" ") %>% tab_spanner(goodness_hist(), colnames(dummy))
        }else{
            ob = new_observed %>% rbind() %>% as.data.frame()
            colnames(ob)[3] = ">=2" 
            rownames(ob)[1] = "Frequency"
            data.table::setDT(ob, keep.rownames = TRUE)[]
            ob %>%  gt(rowname_col = "rn") %>%  tab_stubhead(" ") %>% tab_spanner(goodness_hist(), colnames(ob)) 
        }
        
    })
    
    
    goodness = reactive({
        if(input$distribution == "uniform"){
            df %>% select(input$uniform) %>% pull() %>% table() %>%  chisq.test()
        }else if(input$distribution == "poisson"){
            test_cov
        }
    })

    output$goodness_counts = render_gt({
        if (input$distribution == "uniform"){
            dummy2 = goodness()$expected %>% t() %>% as.data.frame() %>% mutate_if(is.numeric, round,1)
            rownames(dummy2)[1] = "Frequency"
            data.table::setDT(dummy2, keep.rownames = TRUE)[]
            dummy2 %>% gt(rowname_col = "rn") %>%  tab_stubhead(" ") %>% tab_spanner(goodness_hist(), colnames(dummy2)) %>%  tab_source_note(md("Expected counts under the null hypothesis in the goodness of fit test")) 
        }else {
            cov_tab
        }

        })
    output$a_met = renderPrint({
        if (all(goodness()$expected >= 5)){
            print("Congrats, assumptions are met!")
        }else{
            print("Assumptions are not met")
        }
    })
    

    output$chi_goodness = renderPrint({goodness() %>% pander::pander()})
    
    output$chiPlot1 = renderPlotly({
        chi= goodness()  
        chi_p2 = dist_chisq(deg.f = chi$parameter, p = 0.05) %>% remove_geom("GeomText")
        chi_p2 = chi_p2 + annotate("point", x = chi$statistic, y = dchisq(chi$statistic,df =chi$parameter), colour = "blue") +
            annotate("text", x = qchisq(0.95, df =chi$parameter), y = dchisq(qchisq(0.95, df =chi$parameter),df =1), label = "p < 0.05", colour = "red") +
            annotate("text", x = chi$statistic- 0.01*chi$statistic, y = dchisq(chi$statistic,df =chi$parameter) + 0.01, label = "Observed", colour = "blue")+theme_minimal()+ theme(legend.position="none")+
            ggtitle(paste("Chi-squared density with", as.character(chi$parameter), "degree(s) of freedom"))
        ggplotly(chi_p2, width = 7)
    })
    
    
    # Tab 3
    observe({
        updateSelectizeInput(session, 'ind_hom2', choices = setdiff(rev(categories), c(input$ind_hom1)), server = TRUE)
    })

    
    ind = reactive({
        df %>% select(input$ind_hom1,input$ind_hom2) %>% na.omit() %>% table()
    })
    
    output$independence = renderPrint({ind() %>% chisq.test(correct = FALSE)} %>% pander::pander())
    
    output$a_met2 = renderPrint({
        ind_assump = ind() %>% chisq.test(correct = FALSE)
        if (all(ind_assump$expected >= 5)){
            print("Congrats, assumptions are met!")
        }else{
            print("Assumptions are not met")
        }
 
    })
    

    output$obs_counts2 = render_gt({
        exp = ind() %>% chisq.test(correct = FALSE) 
        exp = exp$observed %>% as.data.frame.matrix()
        data.table::setDT(exp, keep.rownames = TRUE)[]
        colnames(exp)[1] = input$ind_hom1
        exp %>% as.data.frame() %>% gt(rowname_col = colnames(exp)[1]) %>% tab_stubhead(label = colnames(exp)[1]) %>% 
            tab_spanner(input$ind_hom2, columns = colnames(exp)[2:length(colnames(exp))])
        
    })
    output$exp_counts_ind = render_gt({
        exp = ind() %>% chisq.test(correct = FALSE)
        exp = exp$expected %>% as.data.frame() %>% mutate_if(is.numeric, round,1)
        data.table::setDT(exp, keep.rownames = TRUE)[]
        colnames(exp)[1] = input$ind_hom1
        exp %>% as.data.frame() %>% gt(rowname_col = colnames(exp)[1]) %>% tab_stubhead(label = colnames(exp)[1]) %>% 
            tab_spanner(input$ind_hom2, columns = colnames(exp)[2:length(colnames(exp))])
    })
    
    output$chiPlot2 = renderPlotly({
                            chi= ind() %>% chisq.test(correct = FALSE) 
                            chi_p2 = dist_chisq(deg.f = chi$parameter, p = 0.05) %>% remove_geom("GeomText")
                            chi_p2 = chi_p2 + annotate("point", x = chi$statistic, y = dchisq(chi$statistic,df =chi$parameter), colour = "blue") +
                                annotate("text", x = chi$statistic - 0.01*chi$statistic, y = dchisq(chi$statistic,df =chi$parameter)+ 0.01, label = "Observed", colour = "blue") +
                                annotate("text", x = qchisq(0.95, df =chi$parameter), y = dchisq(qchisq(0.95, df =chi$parameter),df =1), label = "p < 0.05", colour = "red") + theme_minimal()+ theme(legend.position="none") +  ggtitle(paste("Chi-squared density with", as.character(chi$parameter), "degree(s) of freedom"))
                            ggplotly(chi_p2)
                      })
    
    
    # Tab 4 - one samp
    output$shapiro = renderPrint({
        if (input$assumption=="Shapiro-Wilkes"){
                shap = df %>% pull(input$one_samp) %>% shapiro.test() %>% pander::pander()
                shap
        }
    })
    
    output$ttest_assumption_print1 = renderPrint({

        shap = df %>% pull(input$one_samp) %>% shapiro.test()
        if (shap$p.value < 0.05){
            print("Normality assumption is violated")
        }else{
            print("Congrats, normality assumption satisfied") 
        }
    
    })
    
     
    output$qqPlot = renderPlotly({
        if (input$assumption=="Normal Q-Q plot"){
            g=df %>% ggplot() + aes(sample = .data[[input$one_samp]]) + stat_qq() + stat_qq_line() + theme_minimal()
            ggplotly(g)
        }
    })
    
    output$test_stat = renderPrint({
        df %>% pull(input$one_samp) %>% t.test(mu = as.numeric(input$mu), alternative =input$alternative ) %>% pander::pander()
    })
    
    
    
    output$tPlot = renderPlotly({
        test = df %>% pull(input$one_samp) %>% t.test(mu = as.numeric(input$mu), alternative = input$alternative)
        tp = df %>% pull(input$one_samp) %>% t.test(mu = as.numeric(input$mu), alternative = input$alternative) %>% plot() 
        tp = tp+ annotate("text", x = test$statistic, y = dt(test$statistic, df =test$parameter)+0.01, label = "Observed", colour = "blue")
        ggplotly(tp)
    })
    
    
    
    # Tab 4 - two samp
    
    
    
    cats = reactive({
        df %>% filter(!input$samp_variable %>% is.na()) %>%  select(input$two_samp) %>%  unique() %>% pull(input$two_samp) %>% sort() 
    })
    
    
        
    observe({
        # Updating selectize input
        updateSelectizeInput(session, 'samp1', choices = cats() %>% str_to_title(), server = TRUE)
    })

    
    observe({
        # Updating selectize input
        updateSelectizeInput(session, 'samp2', choices = rev(cats()) %>% str_to_title(), server = TRUE)
    })
    
    
    
    
    
    d1 = reactive({
        df %>% filter(!!as.symbol(input$two_samp) == cats()[cats() == input$samp1 %>% str_to_lower()]) %>% select(input$samp_variable) 
    })
    
    d2 = reactive({
        df %>% filter(!!as.symbol(input$two_samp) == cats()[cats() == input$samp2 %>% str_to_lower()]) %>% select(input$samp_variable) 
    })
    
    
    logical = reactive({
        l11 = length(d1() %>% pull(input$samp_variable))
        l22 = length(d2() %>% pull(input$samp_variable))
        
        if((l11>1) & (l22>1)){
            TRUE
        }else{
            FALSE
        }
    })
    
    output$test_stat2 = renderPrint({
        if (logical() == TRUE){
            print(t.test(d1(),d2(),var.equal = !input$var, alternative = input$alternative2) %>% pander::pander())
        }else{
            print("At least one of the samples is too small to conduct the test.")
        }
    })
    
    output$shap2_1 = renderText({print(paste("Shapiro Test for: ", input$samp1 %>%  str_to_title(), "sample"))})
    output$shap2_2 = renderText({print(paste("Shapiro Test for: ", input$samp2  %>%  str_to_title(), "sample"))})
    output$shapiro2_1 = renderPrint({
          d1() %>% pull(input$samp_variable) %>% na.omit() %>%  shapiro.test() %>%  pander:: pander()
        
    })
    output$shapiro2_2 = renderPrint({
        d2() %>% pull(input$samp_variable) %>%  shapiro.test() %>% pander:: pander()
        
    })
    

    
    output$ttest_assumption_print2 = renderPrint({
        l1 = length(d1() %>% pull(input$samp_variable))
        l2 = length(d2() %>% pull(input$samp_variable))


        if (l1<3){
            print(paste("Size of", input$samp1,"sample is",as.character(l1)," too small(< 3) to check normality assumption"))
        }
        if (l2<3){
            print(paste("Size of", input$samp2,"sample is", as.character(l2)," too small(< 3) to check normality assumption"))
        }
        if((length(d1() %>% pull(input$samp_variable))>=3) & (length(d2() %>% pull(input$samp_variable)) >= 3)){
            x = d1() %>% pull(input$samp_variable) %>%  shapiro.test()
            y = d2() %>% pull(input$samp_variable) %>%  shapiro.test()
            
            if(x$p.value < 0.05 | y$p.value <0.05){
                
                print("Normality assumptions are not met")
            }else{
                print("Congrats, assumptions are met!")
            }
        }
        
    
    })
    
    
    output$tPlot2 = renderPlotly({
        if(logical() == TRUE){
            test2 = t.test(d1(),d2(),var.equal = !input$var, alternative = input$alternative2)
            tp2 = test2 %>% plot() 
            tp2 = tp2+ annotate("text", x = test2$statistic, y = dt(test2$statistic, df =test2$parameter)+0.01, label = "Observed", colour = "blue")
            ggplotly(tp2)
        }
    })
    
    
    
    
    output$qqPlot2 = renderPlotly({

        a <- list(
            text = paste(input$samp1  %>%  str_to_title(), input$samp_variable,'Q-Q plot'),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )
        
        b <- list(
            text = paste(input$samp2  %>%  str_to_title(), input$samp_variable,'Q-Q plot'),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )
        
        p1 = d1() %>% ggplot() + aes(sample = .data[[input$samp_variable]]) + stat_qq() + stat_qq_line() + theme_minimal()
        p1 = p1 %>% ggplotly() %>% layout(annotations = a)
        
        p2 = d2() %>% ggplot() + aes(sample = .data[[input$samp_variable]]) + stat_qq() + stat_qq_line() + theme_minimal()
        p2 = p2 %>% ggplotly %>% layout(annotations = b)
        
        plotly:: subplot(p1, p2, nrows = 1, margin = 0.04)
        
        
        
    })
    
str1 =     " Hey! This Shiny app was created to dynamically produce visualisations and conduct various hypothesis tests on survey data obtained from a sample of 174 DATA2X02 students from the University of Sydney. The survey was conducted over a relatively short period of 4-5 days and there were 572 students enrolled in the cohort DATA2X02 at the time. The dataset has been cleaned with the exception of the shoe size variable due to inconsistency issues that could not be resolved."

str2 = "The survey collects information on a wide array of topics ranging from dental practices, seasonal preferences to one's appearence. However, user identification has been stripped such that respondents remain annonymous.  With the variety in the dataset, I'm sure that you'll be able to find a variable that interests you!"
    
    output$about = renderUI({HTML(paste(str1, str2, sep = '<br/> <br/>'))})
    
    
}








# Run the application 
shinyApp(ui = ui, server = server)
