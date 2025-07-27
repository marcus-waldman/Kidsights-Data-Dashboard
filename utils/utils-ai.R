init_system_msg <- function(){
  # system prompt: tell the model how to write the plotting function
  system_msg <- paste(
    "You are an expert in data visualization in R.",
    "Write a self-contained R function called ai_plot_function.",
    "The function should be written so that it:",
    "- takes a data.frame as its only argument ",
    "- returns a ggplot2 plot object matching the user's request,",
    "- uses ggplot2 and follows best practices in data visualization,",
    "- ensures all required packages are installed and loaded,",
    "- uses explicit package::function calls", 
    "- uses good judgment in matching variables in the data.frame with those required to meet the User's plot request", 
    sep = "\n"
  )
}

anthropic_dynamic_plot <- function(prompt_text, metadata, model = NULL) {
  
  options(timeout = 120)                        # for base R
  
  # ensure ellmer client package is installed and loaded
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    install.packages("ellmer")
  }
  library(ellmer)
  
  # ensure jsonlite package is installed and loaded for JSON serialization
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
  }
  library(jsonlite)
  
  # validate that metadata is provided
  if (is.null(metadata) || length(metadata) == 0) {
    stop("Metadata must be provided as a non-empty list")
  }
  
  # format metadata information for the prompt
  var_descriptions <- c()
  
  # Extract variables from all categories
  if (!is.null(metadata$variable_categories)) {
    for (category_name in names(metadata$variable_categories)) {
      category <- metadata$variable_categories[[category_name]]
      
      if (!is.null(category$variables)) {
        for (var_name in names(category$variables)) {
          var_info <- category$variables[[var_name]]
          
          # Build description string
          type_info <- if (!is.null(var_info$data_type)) paste0(" (", var_info$data_type, ")") else ""
          label_info <- if (!is.null(var_info$label) && var_info$label != "") paste0(" - ", var_info$label) else ""
          category_info <- paste0(" [Category: ", category_name, "]")
          
          # Add specific info based on data type
          extra_info <- ""
          if (!is.null(var_info$factor_info)) {
            extra_info <- paste0(" - Levels: ", paste(var_info$factor_info$levels, collapse = ", "))
          } else if (!is.null(var_info$numeric_info)) {
            extra_info <- paste0(" - Range: ", var_info$numeric_info$min, " to ", var_info$numeric_info$max)
          } else if (!is.null(var_info$character_info)) {
            extra_info <- paste0(" - Unique values: ", var_info$character_info$n_unique)
          }
          
          var_desc <- paste0(var_name, type_info, label_info, category_info, extra_info)
          var_descriptions <- c(var_descriptions, var_desc)
        }
      }
    }
    var_desc <- paste(var_descriptions, collapse = "\n")
  } else {
    # fallback: convert entire metadata to JSON for the model to interpret
    var_desc <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)
  }
  
  # system prompt: tell the model how to write the plotting function
  system_msg <- init_system_msg()
  
  # user prompt: include the user's plot description and the metadata
  user_msg <- paste(
    system_msg, 
    "User's plot request:",
    prompt_text,
    "",
    "Dataset metadata (variables and their information):",
    var_desc,
    "",
    "Please analyze this dataset metadata to understand the available variables, their types, and relationships.",
    "Then generate the ai_plot_function accordingly, using the variable information provided in the metadata.",
    "The function should work with a data.frame that contains these variables.",
    sep = "\n"
  )
  
  # call LLM via the ellmer R client using Anthropic
  chat <- ellmer::chat_anthropic(
    model = model,
    system_prompt = user_msg,
    params = list(temperature = 0, max_tokens = 64000), 
    echo = "all"
  )
  
  content <- chat$chat(user_msg)
  
  # helper to strip markdown fences and extract the function code by brace‐matching
  extract_function_code <- function(text, fname) {
    # remove code fences if present
    text <- gsub("^```[rR]?\\s*|```$", "", text)
    # locate start of the function definition
    pat   <- sprintf("%s\\s*<-\\s*function", fname)
    start <- regexpr(pat, text)
    if (start == -1) stop("Could not find function definition in the model output.")
    snippet <- substring(text, start)
    
    depth  <- 0
    end_pos <- NULL
    for (i in seq_len(nchar(snippet))) {
      ch <- substring(snippet, i, i)
      if (ch == "{") depth <- depth + 1
      if (ch == "}") depth <- depth - 1
      if (!is.null(end_pos)) next
      if (depth == 0 && ch == "}") {
        end_pos <- i
      }
    }
    if (is.null(end_pos)) stop("Unmatched braces in the function definition.")
    substring(snippet, 1, end_pos)
  }
  
  # extract the code for ai_plot_function
  func_code <- extract_function_code(content, "ai_plot_function")
  
  # write it to a temp file and source into a fresh environment
  temp_file <- tempfile(fileext = ".R")
  writeLines(func_code, temp_file)
  func_env <- new.env()
  sys.source(temp_file, envir = func_env)
  
  # retrieve and call the generated plotting function
  ai_plot_function <- func_env$ai_plot_function
  
  # return the function and metadata (plot object would be created when function is called with actual data)
  list(
    ai_plot_function = ai_plot_function, 
    content = content,
    metadata = metadata
  )
}


# NOT RUN
# if(FALSE){
#   
#   prompt_txt = "Make a plot that would give me a sense of whether survey attrition is different by race and ethnicity."
#   out_list = anthropic_dynamic_plot(prompt_txt,metadata)
#   
# }