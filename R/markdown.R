# mdapply <- function(X, FUN, depth, options=""){
#   FUN       <- as.character(substitute(FUN))
#   list_name <- as.character(substitute(X))
#   if(options != "")
#     options <- paste(",",names(options),"=",options,collapse="")
#   build_chunk <- function(nm)
#   {
#     paste0(
#       paste0(rep("#",depth), collapse=""),
#       " ",
#       nm,
#       "\n\n```{r", options, "}\n",
#       FUN,
#       "(", list_name, "[['", nm, "']])\n```")
#   }      
#   parts <- sapply(names(X), build_chunk)
#   whole <- paste(parts, collapse="\n\n")
#   knit(text=whole)
# }



create_section <- function(data, title, level, data_type = "plot") {
  
  # Inserts "## Title (auto)"
  pander::pandoc.header(paste0(title, " {.unnumbered}"), level = level)
  
  # Section contents
  # e.g. a random plot
  
  if (data_type == "plot") {
    print(data) 
  } else if (data_type == "tibble") {
    data %>% 
      kable()
  } else {
    warning("Provide a valid string for the argument 'data_type'")
  }
  
  # a list, formatted as Markdown
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p('')
  # pander::pandoc.list(letters[1:3])
  # pander::pandoc.p('')
}


create_section_results <- function(plot1, plot2, title, level) {
  pander::pandoc.header(paste0(title, " {.unnumbered}"), level = level)
  # Section contents
    # print(plot1)
    # pander::pandoc.p("")
    plot1_log <- 
      plot1 +
            scale_x_log10() +
            scale_y_log10() +
            theme(legend.position = "none",
                  legend.justification='left',
                  legend.direction='horizontal',
                  plot.subtitle = element_markdown(),
                  plot.title = element_markdown()) +
            labs(title = "**... with logarithmic axes**",
                 subtitle = "")
    
    print(plot1 | plot1_log)
    print(plot2)
  # a list, formatted as Markdown
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p('')
  # pander::pandoc.list(letters[1:3])
  # pander::pandoc.p('')
}


affiche_multi_df <- function(b){
  for ( i in seq_along(b)){
    cat(glue::glue("### {names(b)[i]} \n\n"))
    b[[i]] %>%
      kable()
  }}