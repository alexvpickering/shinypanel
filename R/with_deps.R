# attach dependencies
with_deps <- function(markup) {
  addResourcePath(
    prefix = 'css',
    directoryPath = system.file('css', package='shinypanel'))

  addResourcePath("sbs", system.file("www", package="shinyBS"))

  selectizeDep <- htmltools::htmlDependency(
    "selectize", "0.11.2", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap3.css",
    head = format(tagList(
      HTML('<!--[if lt IE 9]>'),
      tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
      HTML('<![endif]-->'),
      tags$script(src = 'shared/selectize/js/selectize.min.js')
    ))
  )

  shinyBSDep <- htmltools::htmlDependency(
    "shinyBS",
    utils::packageVersion("shinyBS"),
    src = c("href" = "sbs"),
    script = "shinyBS.js",
    stylesheet = "shinyBS.css")

  tagList(
    singleton(tags$head(
      tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'css/shinypanel.css'),
      selectizeDep,
      shinyBSDep
    )),
    markup
  )
}
