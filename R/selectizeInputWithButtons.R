#' @import shiny
NULL

#' selectizeInput with buttons and validation utilities
#'
#' @inheritParams shiny::selectizeInput
#' @param ... One or more \code{actionButton}'s to appear to right of input element.
#' @param container_id id for outer \code{div} (optional). Can use e.g. to add \code{'has-error'} class to style inputs
#'   with \code{shinyjs::addClass}.
#' @param help_id id for span with class \code{'help-block'} below input (optional). Can be used for add
#'   error message with \code{shinyjs::html}.
#' @param label_title Optional text to appear in info icon tooltip. Used to provide information to user about input.
#' @param btn_titletips if \code{TRUE} (default), converts title attributes of \code{actionButtons} into tooltips.
#' @param btn_placement Where the \code{actionButton} tooltips should appear (\code{top}, \code{bottom}, \code{left}, \code{right}).
#'   Defaults to \code{'right'} for dropdown buttons and \code{'bottom'} for all others.
#' @param hide_btns If \code{TRUE} styling is adjusted so that all buttons are initially hidden. Can unhide buttons
#'   using \code{toggleSelectizeButtons}.
#' @seealso \code{\link[shiny]{selectizeInput}}, \code{\link[shinyjs]{html}},
#'  \code{\link[shinyjs]{addClass}}, \code{\link{toggleSelectizeButtons}}.
#'
#' @export
#'
#' @examples
#'
#' library(shiny)
#' library(shinypanel)
#'
#' ui <- fluidPage(
#'   div(class = 'row',
#'       div(class = 'col-sm-12 col-lg-6',
#'           div(class = 'well-form',
#'               textAreaInputWithButtons(
#'                 inputId = 'text',
#'                 label = 'Type something:',
#'                 actionButton(
#'                   'btn3',
#'                   '',
#'                   icon('plus', 'fa-fw'),
#'                   title = 'Click to add something'
#'                 )
#'               ),
#'               selectizeInputWithButtons(
#'                 inputId = 'selection',
#'                 label = 'Select something:',
#'                 label_title = 'Information about input',
#'                 actionButton(
#'                   'btn1',
#'                   '',
#'                   icon('tag', 'fa-fw'),
#'                   title = 'this does something'
#'                 ),
#'                 actionButton(
#'                   'btn2',
#'                   '',
#'                   icon('chevron-right', 'fa-fw'),
#'                   title = 'this does something else'
#'                 ),
#'                 options = list(multiple = TRUE)
#'               )
#'           )
#'       )
#'   )
#' )
#'
#'
#' server <- function(input, output, session) {
#'
#'   choices <- reactive({
#'     paste('Long Option', 1:5)
#'   })
#'
#'   observe({
#'     updateSelectizeInput(session, 'selection', choices = choices())
#'   })
#' }
#'
#' # shinyApp(ui, server)
#'
selectizeInputWithButtons <- function(inputId,
                                      label,
                                      ...,
                                      options = NULL,
                                      container_id = NULL,
                                      help_id = NULL,
                                      label_title = NULL,
                                      btn_titletips = TRUE,
                                      btn_placement = NULL,
                                      hide_btns = FALSE) {


  mult <- isTRUE(options$multiple)
  if(mult) {
    select_tag <- tags$select(id = inputId, style = 'display: none;', multiple = TRUE)
  } else {
    select_tag <- tags$select(id = inputId, style = 'display: none;')
  }

  buttons <- list(...)
  buttons <- buttons[!sapply(buttons, is.null)]

  if (length(buttons) == 0)
    return(selectizeInputWithValidation(inputId, label, options, container_id, help_id, label_title))

  # generate tooltips for buttons from titles
  button_tooltips <- NULL
  if (btn_titletips) {
    button_tooltips <- tags$div(
      lapply(buttons, function(btn) {
        btn_placement <- ifelse(is.null(btn_placement),
                                ifelse(any(grepl('dropdown', unlist(btn$attribs))), 'right', 'bottom'),
                                btn_placement)
        shinyBS::bsTooltip(id = btn$attribs$id, title = btn$attribs$title, placement = btn_placement, options = list(container = 'body'))
      })
    )
  }


  # add info icon to label with tooltip
  label_tooltip <- NULL
  if (!is.null(label_title)) {
    label_id <- paste0(inputId, '-label-info')
    label_tooltip <- shinyBS::bsTooltip(label_id, title = label_title, placement = 'top', options = list(container = 'body'))
    label <- tags$span(label, span(class='hover-info', span(id = label_id, icon('info', 'fa-fw'))))
  }

  # if hide_btns allows full-width selectize
  ig_class  <- ifelse(hide_btns, '', 'input-group full-height-btn')
  fhs_class <- ifelse(hide_btns, '', 'full-height-selectize')

  options <- ifelse(is.null(options), '{}', jsonlite::toJSON(options, auto_unbox = TRUE))


  markup <- tags$div(class = 'form-group selectize-fh validate-wrapper', id = container_id,
                     tags$label(class = 'control-label', `for` = inputId, label),
                     tags$div(class = ig_class, id = paste0(inputId, '-input-group'),
                              tags$div(class = fhs_class, id = paste0(inputId, '-full-height-selectize'),
                                       select_tag,
                                       tags$script(type = 'application/json', `data-for` = inputId, HTML(options))
                              ),
                              lapply(buttons, function(btn) {
                                is_dropdown <- any(grepl('dropdown', unlist(btn$attribs)))


                                if (hide_btns & is_dropdown)
                                  btn$children[[1]]$attribs$style <- paste0('display: none;', btn$children[[1]]$attribs$style)

                                if (hide_btns & !is_dropdown)
                                  btn$attribs$style <- paste0('display: none;', btn$attribs$style)

                                if (!is_dropdown)
                                  btn <- div(class = 'input-group-btn', id = paste0(btn$attribs$id, '-parent'), style = btn$attribs$`parent-style`, btn)

                                # remove title since using tooltips
                                if (btn_titletips) btn$attribs$title <- NULL

                                return(btn)
                              })
                     ),
                     tags$span(class = 'help-block', id = help_id),
                     button_tooltips,
                     label_tooltip
  )

  with_deps(markup)
}



#' selectizeInput with validation utilities
#'
#' @inheritParams selectizeInputWithButtons
#' @seealso \code{\link{selectizeInputWithButtons}}, \code{\link[shinyjs]{html}},
#'  \code{\link[shinyjs]{addClass}}, \code{\link{toggleSelectizeButtons}}.
#'
#' @export
#'
selectizeInputWithValidation <- function(inputId, label, options = NULL, container_id = NULL, help_id = NULL, label_title = NULL) {
  options <- ifelse(is.null(options), '{}', jsonlite::toJSON(options, auto_unbox = TRUE))

  label_tooltip <- NULL
  if (!is.null(label_title)) {
    label_id <- paste0(inputId, '-label-info')
    label_tooltip <- shinyBS::bsTooltip(label_id, title = label_title, placement = 'top', options = list(container = 'body'))
    label <- tags$span(label, span(class='hover-info', span(id = label_id, icon('info', 'fa-fw'))))
  }

  markup <- div(class = 'form-group selectize-fh validate-wrapper', id = container_id,
                tags$label(class = 'control-label', `for` = inputId, label, title = label_title),
                div(
                  tags$select(id = inputId, style = 'display: none'),
                  tags$script(type = 'application/json', `data-for` = inputId, HTML(options))
                ),
                tags$span(class = 'help-block', id = help_id),
                label_tooltip
  )

  with_deps(markup)
}



#' Show/hide all buttons in selectizeInputWithButtons
#'
#' When hiding all buttons, some CSS changes are required to allow return of full-width
#' and rounded corners of selectizeInput.
#'
#' @param selectize_id id of \code{selectizeInputWithButtons} element.
#' @param button_ids character vector of \code{actionButton} ids associated with \code{selectize_id}.
#' @inheritParams shinyjs::toggleClass
#' @export
#' @seealso \code{\link{selectizeInputWithButtons}}
#'
toggleSelectizeButtons <- function(selectize_id, button_ids, condition) {
  # allows to take full-width/rounded corners
  shinyjs::toggleClass(paste0(selectize_id, '-input-group'),
                       class = 'input-group full-height-btn',
                       condition = condition)

  shinyjs::toggleClass(paste0(selectize_id, '-full-height-selectize'),
                       class = 'full-height-selectize',
                       condition = condition)

  #hide buttons
  for (button_id in button_ids)
    shinyjs::toggle(button_id, condition = condition)
}
