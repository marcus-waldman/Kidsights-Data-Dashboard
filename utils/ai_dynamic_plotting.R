chatgpt_dynamic_plot <- function(prompt_text, data, api_key, model = "gpt-4.1-mini") {
    options(timeout = 120)                        # for base R
  
   # ensure openai client package is installed and loaded
  if (!requireNamespace("openai", quietly = TRUE)) {
    install.packages("openai")
  }
  library(openai)
  
  # inspect data.frame to list variable names and their classes
  var_names   <- names(data)
  var_classes <- vapply(data, function(x) paste(class(x), collapse = "/"), character(1))
  var_desc    <- paste(sprintf("%s (%s)", var_names, var_classes), collapse = ", ")
  
  # system prompt: tell the model how to write the plotting function
  system_msg <- paste(
    "You are an expert in data visualization in R.",
    "Write an self-contained R function called openai_plot_function.",
    "The function should be written so that it:",
    "- takes a data.frame as its only argument ",
    "- returns a ggplot2 plot object matching the user's request,",
    "- uses ggplot2 and follows best practices in data visualization,",
    "- ensures all required packages are installed and loaded,",
    "- uses explicit package::function calls", 
    "- uses good judgment in matching variables in the data.frame with those required to meet the User's plot request", 
    sep = "\n"
  )
  
  # user prompt: include the user's plot description and the data schema
  user_msg <- paste(
    "User's plot request:",
    prompt_text,
    "",
    "Input data.frame has these variables:",
    var_desc,
    "",
    "Generate the openai_plot_function accordingly.",
    sep = "\n"
  )
  
  # call ChatGPT via the openai R client
  resp <- openai::create_chat_completion(
    model    = "gpt-4o-mini",
    messages = list(
      list(role = "system", content = system_msg),
      list(role = "user",   content = user_msg)
    ),
    temperature = .7, 
    openai_api_key  = api_key
  )
  
  content <- resp$choices$message.content
  
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
  
  # extract the code for openai_plot_function
  func_code <- extract_function_code(content, "openai_plot_function")
  
  # write it to a temp file and source into a fresh environment
  temp_file <- tempfile(fileext = ".R")
  writeLines(func_code, temp_file)
  func_env <- new.env()
  sys.source(temp_file, envir = func_env)
  
  # retrieve and call the generated plotting function
  openai_plot_function <- func_env$openai_plot_function
  plot_obj <- openai_plot_function(data)
  
  # return both the ggplot object and the function itself
  list(
    plot                   = plot_obj,
    openai_plot_function   = openai_plot_function, 
    content                = content
  )
}


hi = chatgpt_dynamic_plot(prompt_text = "Plot sepal length by sepal width color coded by specieis", data = iris)
