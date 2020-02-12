#layout and text formatting of items in the 
  #wizard and outputs sections of the 
  #Overview and Glossary tab/page

ov_format <- function(width1=1,
                      width2=2,
                      width3=4,
                      offset1=0,
                      style1 = 'padding:0px',
                      align2 = "center",
                      text_name = "",
                      text_description = ""
){
  fluidRow(
    column(
      width=width1,
      offset=offset1,
      style=style1
    ),
    column(
      width=width2,
      strong(p(text_name)),
      align=align2
    ),
    column(
      width=width3,
      p(text_description)
    )
  )
}
