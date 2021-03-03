##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["overview_selected_cells_plot_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Plot of selected cells"),
        cerebroInfoButton("overview_details_selected_cells_plot_info")
      ),
      tagList(
        selectInput(
          "overview_selected_cells_plot_select_variable",
          label = "Variable to compare:",
          choices = colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
        ),
        plotly::plotlyOutput("overview_details_selected_cells_plot")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_details_selected_cells_plot_info"]], {
  showModal(
    modalDialog(
      overview_details_selected_cells_plot_info$text,
      title = overview_details_selected_cells_plot_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_details_selected_cells_plot_info <- list(
  title = "Plot of selected cells",
  text = p("Depending on the variable selected to color cells in the dimensional reduction, this plot will show different things. If you select a categorical variable, e.g. 'sample' or 'cluster', you will get a bar plot showing which groups the cells selected with the box or lasso tool come from. Instead, if you select a continuous variable, e.g. the number of transcripts (nUMI), you will see a violin/box plot showing the distribution of that variable in the selected vs. non-selected cells.")
)
