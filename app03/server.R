# adds data table

# SERVER ----
server <- function(input, output) {
  
  # generate random data for app
  source("random_data_generator_dynamic.R")
  
  # create model
  model_2 <- lm(af18 ~ af14 + af15 + af17 + cap14 + cap16 + cap17 + cap18, df)
  
  # predict fy18 annual gifts
  df$predicted_af18 <- predict(model_2, df) 
  
  # change negative predicted gifts to $0
  df$predicted_af18 <- ifelse(df$predicted_af18 < 0, 0, df$predicted_af18)
  
  # create data table based on data filtered by inputs and map
  output$dt <- DT::renderDataTable({
    
    df %>%
      select(name, 
             officer, 
             status, 
             city, 
             state, 
             af18, 
             predicted_af18
      ) %>%
      rename(predicted = predicted_af18) %>% 
      arrange(desc(af18)) %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    extensions = c('Responsive', 
                                   'Buttons')
      ) %>% 
      formatCurrency(c('af18', 
                       'predicted'), 
                     digits = 0)  
    
  }) # end of data table
  
}
