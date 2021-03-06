##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

##
output[["about"]] <- renderText({
  paste0(
    '<b>Version of cerebroApp</b><br>
    v1.3.0<br>
    <br>
    <b>Author</b><br>
    Roman Hillje<br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://romanhaa.github.io/cerebroApp/ title="Official cerebroApp website" target="_blank"><b>Official cerebroApp website</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro title="Official Cerebro repository on GitHub" target="_blank"><b>Official Cerebro repository on GitHub</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro/releases title="Cerebro releases" target="_blank"><b>Cerebro releases</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro/tree/master/examples title="Cerebro example data sets" target="_blank"><b>Cerebro example data sets</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used Cerebro for your research, please cite the following publication:
    <br>
    Roman Hillje, Pier Giuseppe Pelicci, Lucilla Luzi, Cerebro: Interactive visualization of scRNA-seq data, Bioinformatics, btz877, <a href=https://doi.org/10.1093/bioinformatics/btz877 title="DOI" target="_blank">https://doi.org/10.1093/bioinformatics/btz877</a><br>
    <br>
    <b>License</b><br>
    Cerebro is distributed under the terms of the <a href=https://github.com/romanhaa/Cerebro/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>Credit where credit is due</b><br>
    <ul>
      <li>Color palettes were built using colors from <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">https://flatuicolors.com/</a></li>
    </ul>
    <br>
    <b>Preferences</b>'
  )
})

##
output[["preferences"]] <- renderUI({
  tagList(
    tags$div(
      title = "Using WebGL is best for performance but might not be compatible with every browser.",
      checkboxInput(
        "webgl_checkbox",
        label = "Use WebGL",
        value = TRUE
      )
    ),
    tags$div(
      title = "Switching off hover info in projections improves performance.",
      checkboxInput(
        "hover_info_in_projections_checkbox",
        label = "Show hover info in projections",
        value = Cerebro.options[['projections_show_hover_info']]
      )
    )
  )
})

##
observeEvent(input[["webgl_checkbox"]], {
  preferences[["use_webgl"]] <- input[["webgl_checkbox"]]
  print(glue::glue("[{Sys.time()}] WebGL status: {preferences[['use_webgl']]}"))
})

##
observeEvent(input[["hover_info_in_projections_checkbox"]], {
  preferences[["show_hover_info_in_projections"]] <- input[["hover_info_in_projections_checkbox"]]
  print(glue::glue("[{Sys.time()}] Show hover info status: {preferences[['show_hover_info_in_projections']]}"))
})

##
outputOptions(
  output,
  "preferences",
  suspendWhenHidden = FALSE
)

##
output[["logo_Cerebro"]] <- renderImage({
  list(
    src = paste0(Cerebro.options$cerebro_root, '/extdata/logo_Cerebro.png'),
    contentType = 'image/png',
    width = 350,
    height = 405,
    alt = "Cerebro logo",
    align = "right"
  )},
  deleteFile = FALSE
)

##
output[["about_footer"]] <- renderText({
  paste0(
    '<br>
    <div class="text-center">
      <a target="_blank" href="https://www.twitter.com/fakechek1"><i class="fab fa-twitter" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://github.com/romanhaa"><i class="fab fa-github" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://gitlab.com/romanhaa"><i class="fab fa-gitlab" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://hub.docker.com/u/romanhaa"><i class="fab fa-docker" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://linkedin.com/in/roman.hillje"><i class="fab fa-linkedin" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
    </div>'
  )
})
