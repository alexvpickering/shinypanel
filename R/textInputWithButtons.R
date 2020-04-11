#' textInput with validation
#'
#' @inheritParams shiny::textInput
#' @inheritParams selectizeInputWithButtons
#' @export
#'
textInputWithValidation <- function(id, label, container_id = NULL, help_id = NULL) {
  tags$div(class = 'form-group selectize-fh', id = container_id,
           tags$label(class = 'control-label', `for` = id, label),
           tags$input(id = id, type = 'text', class = 'form-control shiny-bound-input', value = '', placeholder = ''),
           tags$span(class='help-block', id = help_id)
  )
}


#' textInput with buttons and validation
#'
#' @inheritParams shiny::textInput
#' @inheritParams selectizeInputWithButtons
#' @export
#' @keywords internal
textInputWithButtons <- function(id, label, ..., container_id = NULL, help_id = NULL, btn_titletips = TRUE, placeholder = '') {

  buttons <- list(...)
  buttons <- buttons[!sapply(buttons, is.null)]

  # generate tooltips
  button_tooltips <- NULL
  if (btn_titletips) {
    button_tooltips <- tags$div(
      lapply(buttons, function(btn)
        shinyBS::bsTooltip(id = btn$attribs$id, title = btn$attribs$title, options = list(container = 'body')))
    )
  }

  tags$div(class = 'form-group selectize-fh', id = container_id, class = 'validate-wrapper',
           tags$label(class = 'control-label', `for` = id, label),
           tags$div(class = 'input-group',
                    tags$input(id = id, type = 'text', class = 'form-control shiny-bound-input', value = '', placeholder = placeholder),
                    tags$span(class = 'input-group-btn',
                              lapply(buttons, function(btn) {
                                if (btn_titletips) btn$attribs$title <- NULL
                                return(btn)
                              }))
           ),
           tags$span(class = 'help-block', id = help_id),
           button_tooltips
  )
}

#' textAreaInput with buttons and validation
#'
#' @inheritParams shiny::textAreaInput
#' @inheritParams selectizeInputWithButtons
#'
#' @export
#'
textAreaInputWithButtons <- function(id, label, ..., container_id = NULL, help_id = NULL, btn_titletips = TRUE, placeholder = '') {

  buttons <- list(...)
  buttons <- buttons[!sapply(buttons, is.null)]

  # generate tooltips
  button_tooltips <- NULL
  if (btn_titletips) {
    button_tooltips <- tags$div(
      lapply(buttons, function(btn)
        shinyBS::bsTooltip(id = btn$attribs$id, title = btn$attribs$title, options = list(container = 'body')))
    )
  }

  tags$div(class = 'form-group selectize-fh', id = container_id, class = 'validate-wrapper',
           tags$label(class = 'control-label', `for` = id, label),
           tags$div(class = 'input-group full-height-btn',
                    tags$textarea(id = id,
                                  class = 'form-control shiny-bound-input',
                                  value = '',
                                  style = 'resize: vertical; min-height: 34px;',
                                  placeholder = placeholder),
                    tags$span(class = 'input-group-btn',
                              lapply(buttons, function(btn) {
                                if (btn_titletips) btn$attribs$title <- NULL
                                return(btn)
                              }))
           ),
           tags$span(class = 'help-block', id = help_id),
           button_tooltips
  )
}
