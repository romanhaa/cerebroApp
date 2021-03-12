##----------------------------------------------------------------------------##
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
        cerebroInfoButton(
          "groups_tree_info",
          style = "margin-right: 3px"
        ),
        shinyWidgets::dropdownButton(
          tags$div(
            style = "color: black !important;",
            class = "pull-right",
            tagList(
              sliderInput(
                "groups_tree_edge_width",
                label = "Edge width:",
                min = 1,
                max = 5,
                step = 1,
                value = 2
              ),
              sliderInput(
                "groups_tree_label_size",
                label = "Label size:",
                min = 1,
                max = 5,
                step = 1,
                value = 2
              ),
              sliderInput(
                "groups_tree_label_offset",
                label = "Label offset:",
                min = 0,
                max = 5,
                step = 0.5,
                value = 0.5
              ),
              checkboxInput(
                inputId = "groups_tree_margin",
                label = "Extend margin",
                value = TRUE
              )
            )
          ),
          circle = FALSE,
          icon = icon("cog"),
          inline = TRUE,
          size = "xs"
        )
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
  req(input[["groups_selected_group"]] %in% getGroups())
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
  req(
    input[["groups_selected_group"]] %in% getGroups(),
    input[["groups_tree_edge_width"]],
    input[["groups_tree_label_size"]],
    input[["groups_tree_label_offset"]],
    !is.null(input[["groups_tree_margin"]])
  )
  ## only proceed if tree is present (this check is necessary because it can
  ## otherwise result in an error when switching between groups)
  if (
    !is.null(getTree( input[["groups_selected_group"]] )) &&
    class(getTree( input[["groups_selected_group"]] )) == 'phylo'
  ) {
    ## retrieve tree from Cerebro object
    tree <- getTree( input[["groups_selected_group"]] )
    ## get color assignments for groups
    group_colors <- reactive_colors()[[ input[["groups_selected_group"]] ]]
    ## get put colors in correct order
    tip_colors <- group_colors[match(tree$tip.label, names(group_colors))]
    ##
    if ( input[["groups_tree_plot_type"]] == "Unrooted" ) {
      ape::plot.phylo(
        tree,
        type = 'unrooted',
        lab4ut = 'axial',
        align.tip.label = TRUE,
        edge.width = input[["groups_tree_edge_width"]],
        label.offset = input[["groups_tree_label_offset"]],
        tip.color = tip_colors,
        font = 1,
        cex = input[["groups_tree_label_size"]],
        no.margin = input[["groups_tree_margin"]]
      )
    ##
    } else if ( input[["groups_tree_plot_type"]] == "Phylogram" ) {
      ape::plot.phylo(
        tree,
        type = 'phylogram',
        direction = "downwards",
        align.tip.label = TRUE,
        edge.width = input[["groups_tree_edge_width"]],
        label.offset = input[["groups_tree_label_offset"]],
        tip.color = tip_colors,
        font = 1,
        cex = input[["groups_tree_label_size"]],
        no.margin = input[["groups_tree_margin"]],
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
      footer = NULL,
      size = "l"
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
