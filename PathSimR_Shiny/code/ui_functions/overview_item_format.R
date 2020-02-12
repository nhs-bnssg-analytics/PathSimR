#layout and text formatting of items in the 
  #wizard and outputs sections of the 
  #Overview and Glossary tab/page
  #sourced in the PathSimR_Shiny_v1.R script
  #used in overview_wizard.R, and overview_output.R

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
