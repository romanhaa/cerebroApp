##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##

tab_groups <- tabItem(
  tabName = "groups",
  shinyjs::inlineCSS("
    #groups_by_other_group_table .table th {
      text-align: center;
    }
    #groups_by_cell_cycle_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("groups_selected_group_UI"),
  cerebroBox(
    title = tagList(
      boxTitle("Relationship tree"),
      cerebroInfoButton("groups_tree_info")
    ),
    uiOutput("groups_tree_UI")
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Composition by other group"),
      cerebroInfoButton("groups_by_other_group_info")
    ),
    tagList(
      uiOutput("groups_by_other_group_UI_buttons"),
      uiOutput("groups_by_other_group_UI_rest")
    )
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Number of transcripts"),
      cerebroInfoButton("groups_nUMI_info")
    ),
    uiOutput("groups_nUMI_UI")
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Number of expressed genes"),
      cerebroInfoButton("groups_nGene_info")
    ),
    uiOutput("groups_nGene_UI")
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Mitochondrial gene expression"),
      cerebroInfoButton("groups_percent_mt_info")
    ),
    uiOutput("groups_percent_mt_UI")
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Ribosomal gene expression"),
      cerebroInfoButton("groups_percent_ribo_info")
    ),
    uiOutput("groups_percent_ribo_UI")
  ),
  cerebroBox(
    title = tagList(
      boxTitle("Cell cycle assignments"),
      cerebroInfoButton("groups_by_cell_cycle_info")
    ),
    tagList(
      uiOutput("groups_by_cell_cycle_UI_buttons"),
      uiOutput("groups_by_cell_cycle_UI_rest")
    )
  )
)
