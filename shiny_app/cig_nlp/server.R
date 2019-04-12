shinyServer(function(input, output) {

    output$output_plot <- renderPlotly({

        # draw the plot
        ggplotly(p_list[[input$input_sw]], tooltip = c("text","label")) %>% 
            layout(legend = list(x = 0, y = 130, orientation = 'h')) %>% 
            config(collaborate = FALSE,
            modeBarButtonsToRemove = buttons_to_remove)

    })

})
