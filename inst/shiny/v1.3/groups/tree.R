##----------------------------------------------------------------------------##
## Tab: Groups
##
## Relationship tree.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["groups_tree_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Relationship tree"),
        cerebroInfoButton("groups_tree_info")
        # shinyWidgets::dropdownButton(
        #   tags$div(
        #     style = "color: black !important;",
        #     class = "pull-right",
        #     selectInput("sel", "Select:", LETTERS)
        #   ),
        #   circle = FALSE,
        #   icon = icon("cog"),
        #   inline = TRUE,
        #   size = "xs"
        # )
      ),
      uiOutput("groups_tree_plot_or_message")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["groups_tree_plot_or_message"]] <- renderUI({

  ##
  req(
    input[["groups_selected_group"]]
  )

  ##
  if ( !is.null(getTree( input[["groups_selected_group"]] )) ) {
    tagList(
      shinyWidgets::radioGroupButtons(
         inputId = "groups_tree_plot_type",
         label = NULL,
         choices = c("Unrooted", "Phylogram"),
         status = "primary",
         justified = TRUE,
         width = "100%",
         size = "sm"
      ),
      plotOutput("groups_tree_plot")
    )
  } else {
    textOutput("groups_tree_text")
  }
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

output[["groups_tree_plot"]] <- renderPlot({

  ## only proceed if tree is present (this check is necessary because it can
  ## otherwise result in an error when switching between groups)
  if ( !is.null(getTree( input[["groups_selected_group"]] )) ) {

    ## retrieve tree from Cerebro object
    tree <- getTree( input[["groups_selected_group"]] )

    ## get colors for tips
    colors_tree <- reactive_colors()[[ input[["groups_selected_group"]] ]]

    ##
    if ( input[["groups_tree_plot_type"]] == "Unrooted" ) {
      ape::plot.phylo(
        cerebro_seurat$trees$seurat_clusters,
        type = 'unrooted',
        lab4ut = 'axial',
        align.tip.label = TRUE,
        edge.width = 2,
        label.offset = 1,
        tip.color = colors_tree,
        font = 1,
        cex = 2,
        no.margin = TRUE
      )

    ##
    } else if ( input[["groups_tree_plot_type"]] == "Phylogram" ) {
      ape::plot.phylo(
        cerebro_seurat$trees$seurat_clusters,
        type = 'phylogram',
        direction = "downwards",
        align.tip.label = TRUE,
        edge.width = 2,
        label.offset = 1,
        tip.color = colors_tree,
        font = 1,
        cex = 2,
        no.margin = TRUE,
        srt = 90,
        adj = 0.5
      )
    }
  }
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["groups_tree_text"]] <- renderText({ "Data not available." })

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["groups_tree_info"]], {
  showModal(
    modalDialog(
      groups_tree_info[["text"]],
      title = groups_tree_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

groups_tree_info <- list(
  title = "Relationship tree",
  text = p("The relationship tree reflects the similarity of groups based on their expression profiles. Instead of using the expression values, the correlation is calculated using the user-specified number of principal components (see 'Analysis info' tab on the left).")
)
