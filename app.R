#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(modeldata)
library(shinythemes)

#
# Earthquake Data Exploration Shiny Application
#
# This application is an interactive data analysis application made using the built-in
# 'quakes' dataset in the modeldata library in R. It is designed to help users
# explore and understand earthquake characteristics through multiple statistical 
# and visualization tools.
#
# The app allows users to:
# - View descriptive statistics for earthquake variables
# - Explore correlations between variables using a heatmap and scatterplots
# - Visualize distributions with histograms and density curves
# - Analyze percentiles and values within distributions interactively
# - Perform statistical inference including one-sample t-tests and correlation tests
# - Build and evaluate multiple linear regression models with interaction terms
#
# The purpose of this application is to demonstrate how exploratory data analysis, 
# statistical testing, and regression modeling can be combined into a single application
# using shiny. It's intended to be a "proof-of-concept" project to demonstrate 
# knowledge rather than be used in actual real-world seismic prediction.
#
# Would I use it?
# Yes, I made the project with the goal that it would be intriguing to me if it was a 
# tool that I randomly came across online. The tools would allow me to gain a decent
# understanding of the different variables and their distributions in this specific 
# earthquake dataset, thought it's not designed for actual scientific earthquake forecasting 
# or production-level analysis, obviously.

data("quakes")

vars <- names(quakes)[sapply(quakes, is.numeric)]

ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  
  titlePanel(tags$b("Earthquakes Data Application")),
  
  tabsetPanel(
    tabPanel("Exploratory Data Analysis",
             h3("Descriptive Statistics"),
             fluidRow(
               column(12,
                      selectInput("Variable", "Choose variable", vars, selected = vars[1]),
                      verbatimTextOutput("summary")
                      )
               ),
             
             hr(),
             
             h3("Correlation Heatmap"),
             fluidRow(
               column(2),
               column(8, plotOutput("heatmap", height = "500px")),
               column(2)
               ),
             
             hr(),
             
             h3("Exploratory Data Visualization"),
             fluidRow(
               column(6,
                      selectInput("x1", "Scatterplot: X Variable", vars, selected = vars[1], width = "50%"),
                      uiOutput("y1_ui"),
                      checkboxInput("show_lm", "Show regression line", FALSE),
                      
                      hr(),
                      
                      plotOutput("scatter"),
                      verbatimTextOutput("scatter_stats")
                      ),
               column(6,
                      selectInput("x2", "Histogram Variable", vars, selected = vars[3], width = "50%"),
                      sliderInput("bins", "Number of Bins", min = 20, max = 40, value = 30, width = "90%"),
                      checkboxInput("show_density", "Show density curve", TRUE),
                      
                      hr(),
                      
                      plotOutput("hist"),
                      )
               )
             ),
    tabPanel("Distributions",
             h3("Percentile/Distribution Explorer"),
             fluidRow(
               column(4,
                      selectInput("var", "Select Variable", vars, selected = vars[1], width = "75%"),
                      
                      hr(),
                      
                      h4("Value to Percentile (Red)"),
                      numericInput("xval", "Enter a value", value = NA, width = "75%"),
                      
                      hr(),
                      
                      h4("Percentile to Value (Blue)"),
                      radioButtons("qmode", "Quantile Mode", c("Single Quantile", "Quantile Range")),
                      conditionalPanel(condition = "input.qmode == 'Single Quantile'",
                                       sliderInput("pval", "Percentile", min = 0, max = 1, value = 0.5, width = "100%")),
                      conditionalPanel(condition = "input.qmode == 'Quantile Range'",
                                       sliderInput("pval_range", "Percentile Range", min = 0, max = 1, value = c(0.25, 0.75), width = "100%"))
                      ),
               column(8,
                      plotOutput("distPlot"),
                      verbatimTextOutput("distText")
                      )
               )
             ),
    tabPanel("Statistical Tests",
             h3("One Sample T-Test"),
             fluidRow(
               column(4,
                      selectInput("ttest_var", "Select Variable", vars),
                      numericInput("mu", "Hypothesized Mean", value = 0),
                      radioButtons("ttest_alt", "Alternative Hypothesis", c("Two-Sided" = "two.sided", "Less Than" = "less", "Greater Than" = "greater"))
                      ),
               column(8,
                      verbatimTextOutput("ttestText")
                      )
             ),
             
             hr(),
             
             h3("Correlation Analysis"),
             fluidRow(
               column(4,
                      selectInput("xvar", "X Variable", vars, selected = vars[1], width = "100%"),
                      uiOutput("yvar_ui"),
                      radioButtons("corr_method", "Correlation Method", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"))
                      ),
               column(8,
                      plotOutput("corrPlot"),
                      verbatimTextOutput("corrText")
                      )
               )
             ),
    tabPanel("Regression",
             h3("Multiple Regression"),
             fluidRow(
               column(4,
                      selectInput("yvar", "Response Variable", vars, selected = vars[1]),
                      checkboxGroupInput("xvars", "Predictors", choices = NULL),                      
                      uiOutput("interaction_ui")
                      ),
               column(8,
                      plotOutput("lmPlot")
                      )
             ),
             
             hr(),
             
             fluidRow(
               column(2),
               column(8, verbatimTextOutput("lmText")),
               column(2)
             )
    )
  )
)

server <- function(input, output, session) {

  ### ------------------------EXPLORATORY DATA ANALYSIS------------------------
  
  # Stat summary for the variable of choosing
  output$summary <- renderPrint({
    
    var <- quakes[[input$Variable]]
    cat("Descriptive summary for variable:", input$Variable, "\n")
    cat("Number of observations:", length(var), "\n\n")
    print(summary(var))
    
  })
  
  # Correlation heatmap with a custom gradient, specific to earthquake dataset
  # coord_fixed() so dimensions are clean
  output$heatmap <- renderPlot({
      
    corr <- cor(quakes)
    
    df <- as.data.frame(as.table(corr))
    colnames(df) <- c("var1", "var2", "value")
    
    ggplot(df, aes(var1, var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "royalblue", mid = "black", high = "firebrick",
                           midpoint = 0, limits = c(-1, 1), name = "Correlation") +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14, angle = -90, hjust = 0.5),
        legend.title.position = "right",
        legend.text = element_text(size = 14, hjust = 0.5),
        legend.key.height = unit(2.5, "cm")
      ) + coord_fixed()
  })
  
  # Enable dynamic change in y variable options depending on x variable chosen for scatterplot
  output$y1_ui <- renderUI({
    
    req(input$x1)
    y_choices <- setdiff(vars, input$x1)
    selectInput("y1", "Scatterplot: Y Variable", y_choices, 
                selected = y_choices[1], width = "50%")
    
  })
  
  # Scatterplot with option to chose x/y variables + toggle linear regression line
  output$scatter <- renderPlot({
    
    req(input$x1, input$y1)
    validate(need(input$x1 != input$y1, ""))
    
    plot <- ggplot(quakes, aes(x = .data[[input$x1]], y = .data[[input$y1]])) + 
      geom_point(size = 2) + 
      labs(title = paste("Scatterplot of", input$x1, "vs", input$y1)) +
      theme_minimal() + 
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
    if (input$show_lm) {
      plot <- plot + geom_smooth(method = "lm", color = "red", linewidth = 1.5)
    }
    
    plot
    
  })
  
  # Histogram with the option to chose variable and bins + toggle density curve
  output$hist <- renderPlot({
    
    plot <- ggplot(quakes, aes(x = .data[[input$x2]])) +
      labs(title = paste("Histogram of", input$x2)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
    if (input$show_density) {
      plot <- plot + 
        geom_histogram(aes(y = after_stat(density)), bins = input$bins, 
                       fill = "black", color = "white") +
        geom_density(color = "red", linewidth = 1.5)
    } else {
      plot <- plot + 
        geom_histogram(bins = input$bins, fill = "black", color = "white")
    }
    
    plot
    
  })
  
  # Correlation and r-squared for scatterplot
  output$scatter_stats <- renderPrint({
    
    req(input$x1, input$y1)
    validate(need(input$x1 != input$y1, ""))
    
    x <- quakes[[input$x1]]
    y <- quakes[[input$y1]]
    r <- cor(x, y)
    
    cat("Correlation (r):", round(r, 3), "\n")
    cat("R-squared:", round(r^2, 3), "\n")
    
  })
  
  ### ------------------------------DISTRIBUTIONS------------------------------
  
  # Reset entered value when changing variable, freeze to prevent flickering incorrect plot
  observeEvent(input$var, {
    freezeReactiveValue(input, "xval")
    updateNumericInput(session, "xval", value = NA)
  })
  
  # Show histogram of chosen variable by default
  # Lines on histogram for quantile, quantile ranges, and any entered value
  output$distPlot <- renderPlot({
    
    x <- quakes[[input$var]]
    df <- data.frame(value = x)
    
    plot <- ggplot(df, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), bins = input$bins, 
                     fill = "black", color = "white") +
      labs(title = paste("Distribution of", input$var), x = input$var, 
           y = "Density") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
    if (input$qmode == "Single Quantile") {
      qval <- quantile(x, probs = input$pval)
      plot <- plot + 
        geom_vline(xintercept = qval, color = "royalblue", linewidth = 2)
    } else {
      qlow  <- quantile(x, probs = input$pval_range[1])
      qhigh <- quantile(x, probs = input$pval_range[2])
      plot <- plot +
        geom_vline(xintercept = qlow, color = "royalblue", linewidth = 2) +
        geom_vline(xintercept = qhigh, color = "royalblue", linewidth = 2)
    }
    
    if (!is.na(input$xval)) {
      plot <- plot +
      geom_vline(xintercept = input$xval, color = "red", linewidth = 2)
    }
    
    plot
    
  })

  # Get percentile of entered value from variable distribution
  # Get values of specified percentile/percentile range from variable distribution
  output$distText <- renderPrint({
    
    x <- quakes[[input$var]]
    qval <- quantile(x, probs = input$pval)
    qlow  <- quantile(x, probs = input$pval_range[1])
    qhigh <- quantile(x, probs = input$pval_range[2])
    
    if (!is.na(input$xval)) {
      percentile <- ecdf(x)(input$xval)
      cat(paste0("Percentile of value (Red): ", round(percentile * 100, 2), "%\n"))
    }
    
    if (input$qmode == "Single Quantile") {
      cat(paste0(input$pval * 100, "th percentile value (Blue): ", round(qval, 3)))
    } else {
      cat(paste0(input$pval_range[1] * 100, "th percentile value (Blue): ", round(qlow, 3), "\n"))
      cat(paste0(input$pval_range[2] * 100, "th percentile value (Blue): ", round(qhigh, 3)))
    }
    
  })
  
  ### ----------------------------STATISTICAL TESTS----------------------------
  
  # Reset hypothesis mean when changing variable/distribution
  observeEvent(input$ttest_var, {
    updateNumericInput(session, "mu", value = 0)
  })
  
  # Summary about variable/distribution + test statistic/p-value/95% CI of chosen mean
  # Additional short line about significance, kinda pointless tbh
  output$ttestText <- renderPrint({
    
    x <- quakes[[input$ttest_var]]
    
    test <- t.test(x, mu = input$mu, alternative = input$ttest_alt)
    
    cat("Sample Mean:", round(mean(x), 3), "\n")
    cat("Sample Size:", length(x), "\n")
    cat("Degrees of Freedom:", round(test$parameter, 3), "\n\n")
    
    cat("Null Hypothesis: Mean =", input$mu, "\n")
    if (input$ttest_alt == "two.sided") {
      cat("Alternative Hypothesis: Mean ≠", input$mu, "\n\n")
    } else if (input$ttest_alt == "less") {
      cat("Alternative Hypothesis: Mean <", input$mu, "\n\n")
    } else {
      cat("Alternative Hypothesis: Mean >", input$mu, "\n\n")
    }
    
    cat("t-statistic:", round(test$statistic, 3), "\n")
    cat("p-value:", format.pval(test$p.value, digits = 3), "\n")
    cat("95% Confidence Interval:",  round(test$conf.int[1], 3), "to", round(test$conf.int[2], 3), "\n\n")
    
    cat("Using alpha level of 0.05:\n")
    if (test$p.value < 0.05) {
      cat("Statistically significant difference detected")
    } else {
      cat("No statistically significant difference detected")
    }
    
  })
  
  # Enable dynamic change in y variable options depending on x variable chosen for correlation test
  output$yvar_ui <- renderUI({
    
    req(input$xvar)
    y_choices <- setdiff(vars, input$xvar)
    selectInput("yvar", "Y Variable", y_choices, selected = y_choices[1], 
                width = "100%")
    
  })
  
  # Like earlier scatterplot, looks different by correlation method
  # Pearson same as earlier, Spearman uses loess just as visualization choice
  # Kendall uses ranks of variables, not sure what to look for but added as a visualization anyways
  output$corrPlot <- renderPlot({
    
    req(input$xvar, input$yvar)
    validate(need(input$xvar != input$yvar, ""))
    
    plot <- ggplot(quakes, aes(
      x = if (input$corr_method == "kendall") rank(.data[[input$xvar]]) else .data[[input$xvar]], 
      y = if (input$corr_method == "kendall") rank(.data[[input$yvar]]) else .data[[input$yvar]])) +
      geom_point(size = 2) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
    if (input$corr_method == "pearson") {
      plot <- plot + 
        geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.5) +
        labs(title = paste("Pearson Correlation of", input$xvar, "vs", input$yvar),
             x = input$xvar,
             y = input$yvar)
    } else if (input$corr_method == "spearman") {
      plot <- plot + 
        geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1.5) +
        labs(title = paste("Spearman Correlation of", input$xvar, "vs", input$yvar),
             x = input$xvar,
             y = input$yvar)
    } else {
      plot <- plot +
        labs(title = paste("Kendall Correlation of", input$xvar, "vs", input$yvar),
             x = paste0("rank(", input$xvar, ")"),
             y = paste0("rank(", input$yvar, ")"))
    }
    
    plot
    
  })
  
  # Correlation test checks for statistically significant association and correlation coefficient
  # 95% CI for Pearson only, additional significance line again
  output$corrText <- renderPrint({
    
    test <- cor.test(quakes[[input$xvar]], quakes[[input$yvar]], 
                     method = input$corr_method)
    
    if (input$corr_method == "pearson") {
      cat("Pearson Correlation Analysis \n\n")
    } else if (input$corr_method == "spearman") {
      cat("Spearman Correlation Analysis \n\n")
    } else {
      cat("Kendall Correlation Analysis \n\n")
    }
    
    cat("Correlation Coefficient:", round(test$estimate, 3), "\n")
    cat("p-value:", format.pval(test$p.value, digits = 3), "\n\n")
    if (input$corr_method == "pearson") {
      cat("95% Confidence Interval:", round(test$conf.int[1], 3), "to", round(test$conf.int[2], 3), "\n\n")
    }
    
    cat("Using alpha level of 0.05:\n")
    if (test$p.value < 0.05) {
      cat("Statistically significant difference detected")
    } else {
      cat("No statistically significant difference detected")
    }
    
  })
  
  ### -------------------------------REGRESSION--------------------------------
  
  # Enable dynamic change in predictor options based on chosen response, freeze for cleaner transition
  # choices has to be directly specified here, not too sure why
  observeEvent(input$yvar, {
    freezeReactiveValue(input, "xvars")
    predictors <- setdiff(vars, input$yvar)
    updateCheckboxGroupInput(session, "xvars", choices = predictors, selected = predictors)
  })
  
  # Enable dynamic change in interaction term options based on chosen predictors
  output$interaction_ui <- renderUI({
    
    req(input$xvars)
    if (length(input$xvars) < 2) return(NULL)
    
    combos <- combn(input$xvars, 2, simplify = FALSE)
    
    interaction_labels <- sapply(combos, function(pair) {
      paste(pair, collapse = " × ")
    })
    
    interaction_values <- sapply(combos, function(pair) {
      paste(pair, collapse = ":")
    })
    
    checkboxGroupInput("interactions", "Interaction Terms", 
                       choices = setNames(interaction_values, interaction_labels))
  })
  
  # Model of chosen terms used to predict response, line for perfect prediction overlayed
  output$lmPlot <- renderPlot({
    
    req(input$yvar, input$xvars)
    
    main_effects <- paste(input$xvars, collapse = " + ")
    interaction_terms <- if (!is.null(input$interactions)) {
      paste(input$interactions, collapse = " + ")
    } else {NULL}
    
    rhs <- if (!is.null(interaction_terms)) {
      paste(main_effects, "+", interaction_terms)
    } else {main_effects}
    
    formula <- as.formula(paste(input$yvar, "~", rhs))
    model <- lm(formula, data = quakes)
    
    df <- data.frame(observed = quakes[[input$yvar]], predicted = predict(model))
    
    ggplot(df, aes(x = predicted, y = observed)) +
      geom_point(size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(title = paste("Observed vs Predicted", input$yvar),
           x = "predicted values",
           y = "observed values") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
  })
  
  # Summary of model of chosen terms (term significance, F-test results)
  output$lmText <- renderPrint({
    
    req(input$yvar, input$xvars)
    
    main_effects <- paste(input$xvars, collapse = " + ")
    
    interaction_terms <- if (!is.null(input$interactions)) {
      paste(input$interactions, collapse = " + ")
    } else {NULL}
    
    rhs <- if (!is.null(interaction_terms)) {
      paste(main_effects, "+", interaction_terms)
    } else {main_effects}
    
    formula <- as.formula(paste(input$yvar, "~", rhs))
    model <- lm(formula, data = quakes)
    
    summary(model)
    
  })
  
}

shinyApp(ui = ui, server = server)
