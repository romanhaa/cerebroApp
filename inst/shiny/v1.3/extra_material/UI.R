##----------------------------------------------------------------------------##
## Tab: Extra material
##----------------------------------------------------------------------------##

tab_extra_material <- tabItem(
  tabName = "extra_material",
  shinyjs::inlineCSS("
    #extra_material_table .table th {
      text-align: center;
    }
    #extra_material_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  uiOutput("extra_material_select_category_and_content_UI"),
  uiOutput("extra_material_content_UI")
)
