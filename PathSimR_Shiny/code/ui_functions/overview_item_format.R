
#' Format overview UI items
#'  @description 
#' layout and text formatting of items in the 
#' wizard and outputs sections of the 
#' Overview and Glossary tab/page
#' sourced in the PathSimR_Shiny_v1.R script
#' used in overview_wizard.R, and overview_output.R
#' 
#' @details 
#' The function is used to create the individual items of a text list with bespoke formatting. It creates an HTML string (stored in a list), which divides the page into three columns of specified widths, and places text in each, with specified alignment and paddding.
#' 
#' @param width1 Integer, width of first colum in CSS units (12ths of a page)
#' @param width2 Integer, width of first colum in CSS units (12ths of a page)
#' @param width3 Integer, width of first colum in CSS units (12ths of a page)
#' @param offset1 Integer
#' @param style1 Text
#' @param align2 Text
#' @param text_name Character string, title of UI list element
#' @param text_description Character string, single paragraph of descriptive text in UI list element
#' @param include_col_2 Logical
#' 
#' @return An HTML string stored as a list, which can be read by the Shiny UI script.


ov_format <- function(width1=1,
                      width2=2,
                      width3=4,
                      offset1=0,
                      style1 = 'padding:0px',
                      align2 = "center",
                      text_name = "",
                      text_description = "",
                      include_col_2=TRUE
){
  list(fluidRow(
    column(
      width=width1,
      offset=offset1,
      style=style1
    ),
    if(include_col_2==TRUE){
    column(
      width=width2,
      strong(p(text_name)),
      align=align2
    )},
    column(
      width=width3,
      p(text_description)
    )
  ),
  br()
  )
}
