#Created: 2019-10-14
#Updated: 2020-02-12
#Author: Alex Zajichek
#Package: cheese
#Description: Function definitions for the package

#Name: actual_depth
#Description: Compute the explicit mapping depth based on what is implied by the input
actual_depth <-
  function(
    depth, #An abstract depth
    ref #A reference value for adjusting depth
  ) {
    
    #Check that inputs are numeric
    if(!is.numeric(depth) | !is.numeric(ref))
      stop("Inputs must be numeric.")
    
    #If depth is negative, subtract from ref
    if(depth < 0) {
      
      #Use zero if it goes beyond
      depth <- max(ref + depth, 0)
      
    } else {
      
      #Use maximum depth as the limit
      depth <- min(depth, ref)
      
    }
    
    depth
    
  }

#Name: divide
#Description: Stratify a data frame into a list
divide <-
  function(
    data, #A data frame
    ..., #Variables to split by
    depth = Inf, #Depth to split to; default to max depth
    remove = TRUE, #Should stratification variables be removed?
    drop = TRUE, #Should unused combinations be dropped?
    sep = "|" #Character to separate levels when necessary
  ) {
    
    #Splice variable names
    selected_vars <-
      tidyselect::eval_select(
        rlang::expr(c(...)),
        data
      ) 
    
    #Return error if no split variables entered
    if(length(selected_vars) == 0)
      stop("No columns registered.")
      
    #Set values as names
    selected_vars <- as.list(names(selected_vars))

    #Get the depth
    depth <-
      actual_depth(
        depth = depth,
        ref = length(selected_vars)
      )
    
    #Return data if depth is 0
    if(depth == 0) {
      
      #Give warning
      warning("Split depth is 0; returning original data.")
      
      #Return original data as a tibble
      return(data %>% tibble::as_tibble())
      
    }
    
    #Pull elements past position 'depth' into position 'depth'
    selected_vars[[depth]] <- 
      selected_vars[seq.int(depth, length(selected_vars))] %>% 
      purrr::flatten_chr()
    
    #Only keep up to desired depth
    selected_vars <- selected_vars[seq_len(depth)]
    
    #Set initial list to start loop
    data <-
      data %>%
      
      #Convert to tibble
      tibble::as_tibble() %>%
      
      #Split by first variable(s)
      split(
        dplyr::select(., tidyselect::all_of(selected_vars[[1]])),
        drop = drop,
        sep = sep
      )
    
    #If needed, loop to split at desired depth
    if(depth > 1) {
      
      for(i in seq_len(depth - 1)) {
        
        data <-
          data %>%
          
          #Split at leaf data frames
          purrr::modify_depth(
            .depth = i,
            ~
              .x %>%
              split(
                dplyr::select(., tidyselect::all_of(selected_vars[[i + 1]])),
                drop = drop,
                sep = sep
              )
          )
        
      }
      
    }
    
    #Remove all split variables if necessary
    if(remove) {
      
      data <-
        data %>%
        
        #Alter at the leaves
        purrr::modify_depth(
          .depth = depth,
          ~
            .x %>%
            dplyr::select(
              -tidyselect::all_of(
                selected_vars %>% 
                  purrr::flatten_chr()
              )
            )
        )
      
    }
    
    #Return result
    data
    
  }

#Name: depths_string
#Description: Create representation of elements satisfying a predicate in a list structure
depths_string <-
  function(
    list, #A list, data frame or atomic vector
    predicate, #A binary function
    bare = TRUE, #Only continue on bare lists
    ... #Additional arguments for 'predicate'
  ) {
    
    #Check if function is supplied
    if(missing(predicate))
      stop("No predicate supplied.")
    
    #Get continuation function
    continue <- rlang::is_list
    if(bare)
      continue <- rlang::is_bare_list
    
    #Check if elements satisfy the predicate
    result <-
      list %>%
      purrr::map_lgl(
        .f = predicate,
        ...
      )
    
    #Make vector of indices
    index <- seq_along(result)
    
    #Merge indices with indicator
    result <- ifelse(result, -1*index, index)
    
    #Check if next elements are lists
    are_lists <- 
      list %>% 
      purrr::map_lgl(
        .f = continue
      )
    
    #Continue traversal for list elements only
    if(any(are_lists)) {
      
      result <-
        
        c(
          
          #Recursive function call for list elements
          result[are_lists] %>%
            stringr::str_c(
              list[are_lists] %>% 
                purrr::map_chr(
                  .f = depths_string, 
                  predicate = predicate,
                  bare = bare,
                  ...
                )
            ),
          
          #Concatenate with indices of non-list elements
          result[!are_lists]
          
        )[order(order(are_lists, decreasing = TRUE))]
      
    } 
    
    #Return current result
    result %>%
      
      #Collapse indices
      stringr::str_c(
        collapse = ","
      ) %>%
      
      #Add boundaries
      stringr::str_c(
        "{",
        .,
        "}"
      )
    
  }

#Name: depths
#Description: Find the depths in a list structure that satisfy a predicate
depths <-
  function(
    list, #A list, data frame or atomic vector
    predicate, #A binary function
    bare = TRUE, #Only continue on bare lists
    ... #Additional arguments for 'predicate'
  ) {
    
    #Get string representation of structure
    string <-
      depths_string(
        list = list,
        predicate = predicate,
        bare = bare,
        ...
      )
    
    #Split input into vectors of characters
    string <- strsplit(string, split = "")[[1]]
    
    #Get the depth of each character
    result <- cumsum(string == "{") - cumsum(string == "}")
    
    #Return depths that satisfied the predicate
    unique(result[string == "-"])
    
  }

#Name: fasten
#Description: Collapse a list of data frames of arbitrary depth to any depth
fasten <-
  function(
    list, #A list with data frames at the leaves
    into = NULL, #Character vector of variable names (use "" for NULL)
    depth = 0 #Depth to collapse to
  ) {
    
    #Check for a bare list
    if(!rlang::is_bare_list(list))
      stop("A bare list must be supplied.")
    
    #Find depth of leaves
    bind_depth <- 
      list %>%
      
      #Extract the depth
      depths(
        predicate = is.data.frame,
        bare = TRUE
      ) 
    
    #Check for a data frame
    if(length(bind_depth) == 0)
      stop("There must be data frames at the leaves.")
    
    #Check for single depth
    if(length(bind_depth) > 1)
      stop("All data frames must be at the same depth.")
    
    #Get implied depth
    depth <-
      actual_depth(
        depth = depth,
        ref = bind_depth
      )
    
    #Return original list if no binding is needed
    if(depth == bind_depth) {
      
      #Give warning
      warning("Desired depth equal to current depth. Returning original list.")
      
      #Return original data as a tibble
      return(list)
      
    }
    
    #Check if column names were provided
    if(is.null(into)) {
      
      #Make vector of empty strings
      into <- rep("", bind_depth - depth)
      
    } else {
      
      #Check if input is a character vector
      if(!is.character(into)) {
        
        #Coerce to character
        into <- as.character(into)
        
        #Give warning
        warning("Coercing 'into' to character vector.")
        
      }
      
      #Check if enough column names were provided
      if(length(into) < bind_depth - depth) {
        
        #Add on empty strings for sufficient length
        into <- c(into, rep("", bind_depth - depth - length(into)))
        
      } else if(length(into) > bind_depth - depth) {
        
        #Give warning
        warning(paste0(length(into), " column names given but only ", bind_depth - depth, " needed."))
        
        #Only take elements needed
        into <- into[seq_len(bind_depth - depth)]
        
      }
      
    }
    
    #Loop to bind rows
    while(bind_depth > depth) {
      
      #Get inner column name
      .id <- into[length(into)]
      
      #Set to NULL if empty string
      if(.id == "")
        .id <- NULL
      
      if(bind_depth > 1) {
        
        list <-
          list %>%
          
          #Bind in the interior of list
          purrr::modify_depth(
            .depth = bind_depth - 1,
            dplyr::bind_rows,
            .id = .id
          )
        
      } else {
        
        list <-
          list %>%
          
          #Bind at top level of list
          dplyr::bind_rows(
            .id = .id
          )
        
      }
      
      #Move to next depth
      bind_depth <- bind_depth - 1
      
      #Remove used column header
      into <- into[-length(into)]
      
    }
    
    list
    
  }

#Name: muddle
#Description: Randomly permute some or all columns in a data frame
muddle <-
  function(
    data, #A data frame
    at, #Columns to permute
    ... #Arguments to pass to 'sample'
  ) {
    
    #Set to all columns if missing input
    if(missing(at))
      at <- names(data)

    #Splice variable names
    selected_vars <-
      tidyselect::eval_select(
        rlang::enquo(at),
        data
      ) 
    
    #Return error if no split variables entered
    if(length(selected_vars) == 0)
      stop("No columns registered.")
    
    #Shuffle desired variables
    data %>%
      purrr::map_at(
        selected_vars,
        sample,
        ...
      ) %>%
      
      #Combine columns back together
      dplyr::bind_cols()
    
  }

#Name: stratiply
#Description: Stratify a data frame and apply a function
stratiply <-
  function(
    data, #Any data frame
    f, #Function to apply to each strata
    by, #Stratification variables
    ... #Additional arguments to pass to f
  ) { 
    
    #Check for a function
    if(missing(f))
      stop("No function supplied.")
    
    #Check for inputs
    if(missing(by))
      stop("No columns supplied.")
    
    #Splice variable names
    selected_vars <-
      tidyselect::eval_select(
        rlang::enquo(by),
        data
      ) 
    
    #Return error if no split variables entered
    if(length(selected_vars) == 0)
      stop("No columns registered.")
    
    data <-
      data %>%
      
      #Divide frame into a list
      divide(tidyselect::all_of(selected_vars))
    
    #Find mapping depth
    mapping_depth <-
      data %>%
      depths(
        predicate = is.data.frame,
        bare = TRUE
      )
    
    #Evaluate function at desired depth
    data %>%
      purrr::map_depth(
        .depth = mapping_depth,
        .f = f,
        ...
      )
    
  }

#Name: dish
#Description: Evaluate a function on parts of a data frame
dish <-
  function(
    data, #A data frame
    f, #A two-argument function
    left, #Columns to be entered in arg1
    right, #Columns to be entered in arg2
    each_left = TRUE, #Should the left variables be entered individually as vectors?
    each_right = TRUE, #Should the right variables be entered individually as vectors?
    ... 
  ) {
    
    #Check for function
    if(missing(f))
      stop("No function supplied.")
    
    #Check for left inputs
    if(missing(left)) {
      
      #Check for right inputs
      if(missing(right)) {
        
        #Set both sides to all variables
        left <-
          tidyselect::eval_select(
            rlang::expr(names(data)),
            data
          )
        
        right <- left
        
      } else {
        
        #Splice right variables
        right <-
          tidyselect::eval_select(
            rlang::enquo(right),
            data
          )
        
        #Check for registration
        if(length(right) == 0)
          stop("No right variables registered.")
        
        #Left variables are everything else
        left <-
          tidyselect::eval_select(
            rlang::expr(names(data)[-right]),
            data
          )
        
      }
      
    } else {
      
      #Splice left variables
      left <-
        tidyselect::eval_select(
          rlang::enquo(left),
          data
        )
      
      #Check for registration
      if(length(left) == 0)
        stop("No left variables registered.")
      
      #Check for right inputs
      if(missing(right)) {
        
        #Right variables are everything else
        right <-
          tidyselect::eval_select(
            rlang::expr(names(data)[-left]),
            data
          )
        
      } else {
        
        #Splice right variables
        right <-
          tidyselect::eval_select(
            rlang::enquo(right),
            data
          )
        
        #Check for registration
        if(length(right) == 0)
          stop("No right variables registered.")
        
      }
      
    }
    
    #Select data subsets
    left <- data %>% dplyr::select(tidyselect::all_of(left))
    right <- data %>% dplyr::select(tidyselect::all_of(right))
    
    ###Evaluate function depending on 'each_' arguments
    #Check for left evaluation
    if(each_left) {
      
      #Check for right evaluation
      if(each_right) {
        
        #Evaluate each left argument as a vector with each right argument as a vector
        left %>%
          
          #For every left variable
          purrr::map(
            
            #Evaluate the function for every right variable
            function(.x)
              right %>% purrr::map(function(.y) f(.x, .y, ...))
            
          )
        
      } else {
        
        #Evaluate each left argument as a vector with the right columns in a single data frame
        left %>%
          
          #For every left variable
          purrr::map(
            
            #Evaluate the function with the right data frame
            function(.x) f(.x, right, ...)
            
          )
        
      }
      
    } else {
      
      #Check for right evaluation
      if(each_right) {
        
        #Evaluate each right argument as a vector with the left columns in a single data frame
        right %>%
          
          #For every right variable
          purrr::map(
            
            #Evaluate the function with the left data frame
            function(.x) f(left, .x,...)
            
          )
        
      } else {
        
        #Evaluate with a data frame of left columns and data frame of right columns
        f(left, right, ...)
        
      }
      
    }
    
  }

#Name: absorb
#Description: Populate a custom text template with the values in a set of key-value pairs
absorb <-
  function(
    key,
    value,
    text,
    sep = "_",
    trace = FALSE,
    evaluate = FALSE
  ) {
    
    #Check that lengths match
    if(length(key) != length(value))
      stop("Keys and values must hold the same number of elements.")
    
    #Check text is a character vector
    if(!is.character(text))
      stop("Text input must be a vector of character strings.")
    
    #Check for names
    names <- names(text)
    
    #Put into data frame
    lookup <-
      tibble::tibble(
        key = key,
        value = value
      ) %>%
      
      #Convert keys/values to character vectors
      dplyr::mutate_all(
        as.character
      ) %>%
      
      #Concatenate values for each key
      dplyr::group_by(
        key
      ) %>%
      dplyr::summarise(
        value = stringr::str_c(value, collapse = sep)
      )
    
    #Repeat for each unique key
    for(i in seq_len(nrow(lookup))) {
      
      #Replace all substrings in text with values that match the key
      text <- text %>% stringr::str_replace_all(pattern = lookup$key[i], replacement = lookup$value[i])
      
      #Trace if desired
      if(trace)
        cat(text, "\n")
      
    }
    
    #Restore names if provided
    if(!is.null(names))
      names(text) <- names
    
    #Evaluate expressions if necessary
    if(evaluate)
      text <- text %>% purrr::map(~eval(parse(text = .x)))
    
    #Return result
    text
    
  }

#Name: some_type
#Description: Does an object conform to one or more types?
some_type <-
  function(
    object,
    types
  ) {
    
    #Check for inputs
    if(missing(object))
      stop("No object supplied")
    
    #Check for types
    if(missing(types))
      stop("No types supplied")
    
    #Loop through input
    match_found <- FALSE
    for(i in seq_along(types)) {
      
      #Check if object type conforms
      if(methods::is(object, types[i])) {
        
        #Change status; exit loop
        match_found <- TRUE
        break
        
      }
      
    }
    
    #Return indicator
    match_found
    
  }

#Name: typly
#Description: Apply a function to columns conforming (or not) to the specified types
typly <-
  function(
    data,
    f,
    types,
    negated = FALSE,
    ...
  ) {
    
    #Check for a function
    if(missing(f))
      stop("No function supplied.")
    
    #Check for types
    if(missing(types))
      stop("No types supplied")
    
    #Test type matching
    type_test <- 
      data %>% 
      
      #Get a logical vector
      purrr::map_lgl(
        .f = some_type,
        types = types
      )
    
    #Negate if needed
    if(negated)
      type_test <- !type_test
    
    #If any conform, apply function
    if(any(type_test)) {
      
      data %>%
        
        #Select columns with conforming types
        dplyr::select_if(
          type_test
        ) %>%
        
        #Apply the function
        purrr::map(
          f,
          ...
        )
      
    } else {
      
      NULL
      
    }
    
    
  }

#Name: default_univariate_functions
#Description: Provides list of functions for different types of data
default_univariate_functions <-
  function(useNA) {
    
    list(
      
      #Functions for all data
      f_all =
        list(
          
          length = length,
          missing = ~sum(is.na(.)),
          available = ~sum(!is.na(.)),
          class = class,
          unique = ~length(unique(.))
          
        ),
      
      #Functions for numeric data
      f_numeric =
        list(
          
          evaluated = ~"numeric",
          mean = ~mean(., na.rm = TRUE),
          sd = ~sd(., na.rm = TRUE),
          min = ~min(., na.rm = TRUE),
          q1 = ~quantile(., .25, na.rm = TRUE),
          median = ~median(., na.rm = TRUE),
          q3 = ~quantile(., .75, na.rm = TRUE),
          max = ~max(., na.rm = TRUE),
          iqr = ~IQR(., na.rm = TRUE),
          range = ~diff(range(., na.rm = TRUE))
          
        ),
      
      #Functions for categorical data
      f_categorical =
        list(
          
          evaluated = ~"categorical",
          count = table,
          proportion = ~prop.table(table(., useNA = useNA)),
          percent = ~prop.table(table(., useNA = useNA))*100
          
        ),
      
      #Functions for other data
      f_other =
        list(
          
          evaluated = ~"other"
          
        )
      
    ) %>%
      
      #Convert everything to a function
      purrr::map_depth(
        .depth = 2,
        rlang::as_function
      )
    
  }

#######COMPLETE UP TO THIS POINT
#Name: descriptives
#Description: Compute descriptive statistics on columns of a data frame
descriptives <-
  function(
    data,
    f_all = NULL,
    f_numeric = NULL,
    numeric_types = "numeric",
    f_categorical = NULL,
    categorical_types = "factor",
    f_other = NULL,
    na.rm = TRUE,
    useNA = c("ifany", "no", "always"),
    round = 2
  ) {
    
    ###Concantenate provided functions with default functions
    #Functions to apply to all data
    f_all <-
      c(
        list(
          length = function(x, ...) length(x),
          missing = function(x, ...) sum(is.na(x)),
          available = function(x, ...) sum(!is.na(x)),
          class = function(x, ...) class(x),
          unique = dplyr::n_distinct
        ),
        f_all
      )
    
    #Functions to apply to numeric data
    f_numeric <-
      c(
        f_all,
        list(
          evaluated = function(x, ...) "continuous",
          mean = mean,
          sd = sd,
          min = min,
          median = median,
          iqr = IQR,
          max = max,
          q1 = function(x, ...) quantile(x, .25, na.rm = na.rm),
          q3 = function(x, ...) quantile(x, .75, na.rm = na.rm)
        ),
        f_numeric
      )
    
    #Functions to apply to categorical data
    f_categorical <-
      c(
        f_all,
        list(
          evaluated = function(x, ...) "categorical",
          count = table,
          percent = function(x, ...) prop.table(table(x, ...))*100
        ),
        f_categorical
      )
    
    #Functions to apply to other data
    f_other <-
      c(
        f_all,
        list(
          evaluated = function(x) "other"
        ),
        f_other
      )
    
    #Get NA argument
    useNA <- match.arg(useNA)
    
    #Get summary
    data %>%
      
      #Get numeric summaries
      typly(
        types = numeric_types,
        f = f_numeric,
        keep = TRUE,
        na.rm = na.rm
      ) %>%
      
      #Get categorical summaries
      typly(
        types = categorical_types,
        f = f_categorical,
        keep = TRUE,
        useNA = useNA
      ) %>%
      
      #Get remaining summaries
      typly(
        types = 
          setdiff(
            data %>%
              purrr::map(class) %>%
              purrr::flatten_chr(),
            c(numeric_types, categorical_types)
          ),
        f_other,
        keep = TRUE
      ) %>%
      
      #Bind together
      purrr::map_df(
        ~.x %>%
          purrr::map_df(
            function(.y) {
              
              if(is.null(names(.y))) {
                
                if(any(c(numeric_types, "numeric") %>% purrr::map_lgl(function(.z) methods::is(.y, .z)))) {
                  
                  if(methods::is(.y, "numeric")) {
                    
                    tibble::tibble(
                      .value = .y
                    )
                    
                  } else {
                    
                    tibble::tibble(
                      .label = as.character(.y)
                    )
                    
                  }
                  
                } else {
                  
                  tibble::tibble(
                    .label = .y
                  )
                  
                }
                
              } else {
                
                tibble::tibble(
                  .level = names(.y),
                  .order = 1:length(.y),
                  .value = .y
                )
                
              }
              
            },
            .id = ".key"
          ),
        .id = ".variable"
      ) %>%
      
      #Make a new column with all labels/values
      dplyr::mutate(
        .combo =
          dplyr::case_when(
            !is.na(.value) ~ as.character(round(.value, round)),
            TRUE ~ .label
          )
      )
  }

#Name: grable
#Description: Make a kable with stacked header
grable <-
  function(
    data
  ) {
    
  }

#Name: absorb_descriptive_variable
#Description: Absorbs values into a custom text string for a specific variable produced by descriptives
absorb_descriptive_variable <-
  function(
    variable,
    numeric_summary,
    categorical_summary,
    other_summary,
    all_summary,
    evaluate
  ) {
    
    #Extract evaluation type
    eval_type <-
      variable %>%
      dplyr::filter(.data$.key == "evaluated") %>%
      dplyr::pull(.data$.combo)
    
    #Summarise depending on type
    if(eval_type %in% c("continuous", "other")) {
      
      #Choose the correct summary templates
      use_summary <- numeric_summary
      if(eval_type == "other")
        use_summary <- other_summary
      
      #Absorb the computed functions into strings
      results <-
        absorb(
          key = variable$.key,
          value = variable$.combo,
          text = use_summary,
          evaluate = evaluate
        ) %>%
        
        #Make a bindable tibble
        as.list() %>%
        dplyr::bind_cols()
      
    } else {
      
      results <-
        variable %>%
        
        #Only keep the spots where there is a factor level
        dplyr::filter(!is.na(.data$.order)) %>%
        
        #Convert the NA level to a string so it splits
        dplyr::mutate(
          .level =
            dplyr::case_when(
              is.na(.data$.level) ~ "NA",
              TRUE ~ .data$.level
            )
        ) %>%
        
        #Split by the level
        stratiply(
          strata = ".level",
          f =
            function(.level) {
              
              #Absorb the summary on each level
              absorb(
                key = .level$.key,
                value = .level$.combo,
                text = categorical_summary,
                evaluate = evaluate
              ) %>%
                
                #Convert to binable tibble
                as.list() %>%
                dplyr::bind_cols() %>%
                
                #Maintain the order
                dplyr::bind_cols(
                  .level %>%
                    dplyr::select(.data$.order) %>%
                    dplyr::distinct()
                )
              
            },
          
          #Bind the summaries together over the levels
          bind = TRUE,
          separate = TRUE
        ) %>%
        dplyr::mutate(
          .level = as.character(.data$.level)
        )
    } 
    
    #Add the summaries that should be applied to all variables
    if(!is.null(all_summary)) {
      
      #Allow absorption of all keys
      results_all <-
        absorb(
          key = variable$.key,
          value = variable$.combo,
          text = all_summary,
          evaluate = evaluate
        ) %>%
        
        #Make bindable tibble
        as.list() %>%
        dplyr::bind_cols()
      
      if(eval_type == "categorical") {
        
        results <-
          results %>%
          
          #Create an extra row for categorical variables so summaries are not lined up with levels
          dplyr::bind_rows(
            results_all %>%
              
              #Add index to maintain order
              dplyr::mutate(
                .order = 0
              )
          )
        
        
      } else {
        
        results <-
          results %>%
          
          #For other variables, just add the summary to new columns
          dplyr::bind_cols(
            results_all
          )
        
      }
      
    }
    
    results
    
  }

#Name: absorb_descriptives
#Description: Calls absorb_descriptive_variable for each variable and arranges columns
absorb_descriptives <-
  function(
    variables,
    numeric_summary,
    categorical_summary,
    other_summary,
    all_summary,
    evaluate
  ) {
    
    temp_results <-
      variables %>%
      
      #Separate computed statistics by each variable and absorb values into string templates
      stratiply(
        strata = ".variable",
        absorb_descriptive_variable,
        bind = TRUE,
        separate = TRUE,
        numeric_summary = numeric_summary,
        categorical_summary = categorical_summary,
        other_summary = other_summary,
        all_summary = all_summary,
        evaluate = evaluate
      ) %>%
      
      #Rearrange the variables into a logical order
      dplyr::select(
        .data$.variable,
        tidyselect::matches("^[.](level|order)$"),
        tidyselect::everything()
      )
    
    #Reorder factor levels
    if(".order" %in% names(temp_results)) {
      
      temp_results <-
        temp_results %>%
        
        #Group by the variable
        dplyr::group_by(.data$.variable) %>%
        
        #Arrange by order
        dplyr::arrange(.data$.order, .by_group = TRUE) %>%
        dplyr::ungroup()
      
    }
    
    temp_results
    
  }

#Name: univariate_associations
#Description: Apply a list of functions to a variable for each response
univariate_associations <-
  function(
    data,
    f,
    responses = NULL,
    predictors = NULL
  ) {
    
    #Make f a list if its a single function
    if(methods::is(f, "function")) {
      f <- list(f)
    }
    
    f %>%
      
      #Map each function
      purrr::imap(
        ~
          data %>% 
          dish(
            .x,
            left = responses,
            right = predictors,
            bind = TRUE
          ) %>%
          
          #Remove .right
          dplyr::select(-.data$.right) %>%
          
          #Gather results
          tidyr::gather(
            key = .variable,
            value = .y,
            -.data$.left
          ) %>%
          
          #Rename result as function name
          dplyr::rename_at(
            ".y",
            function(.z) .y
          )
      ) %>%
      
      #Recursively join on common columns
      purrr::reduce(
        dplyr::inner_join,
        by = c(".left", ".variable")
      ) %>%
      
      #Convert to factor to maintain order
      dplyr::mutate(
        .left = forcats::as_factor(.data$.left)
      ) %>%
      
      #Arrange by outcome
      dplyr::arrange(
        .data$.left
      )
  }

#Name: univariate_table
#Description: Compute a custom univariate summary table for a dataset
univariate_table <-
  function(
    data, 
    strata = NULL,
    associations = NULL,
    numeric_summary = c(Summary = "median (q1, q3)"),
    categorical_summary = c(Summary = "count (percent%)"),
    other_summary = c(Summary = "unique"),
    all_summary = NULL,
    evaluate = FALSE,
    add_n = FALSE,
    order = NULL,
    labels = NULL,
    levels = NULL,
    format = c("html", "latex", "markdown", "pandoc", "none"),
    variableName = "Variable",
    levelName = "Level",
    na_string = "(missing)",
    strata_sep = "/",
    summary_strata_sep = "_",
    fill_blanks = "",
    caption = NULL,
    ...
  ) {
    
    #Function that retrieves raw summary
    base_summary <-
      function(.data) {
        
        .data %>%
          
          #Compute descriptive statistics passing arguments
          descriptives(...) %>%
          
          #Absorb the values into the string templates
          absorb_descriptives(
            numeric_summary = numeric_summary,
            categorical_summary = categorical_summary,
            other_summary = other_summary,
            all_summary = all_summary,
            evaluate = evaluate
          )
        
      }
    
    #Create some default values for later use
    col_strata <- NULL
    row_strata <- NULL
    header <- NULL
    association_names <- NULL
    
    #Retrieve base summary
    if(is.null(strata)) {
      
      results <-
        data %>%
        base_summary
      
    } else {
      
      results <-
        data %>%
        
        #Obtain summary for each strata combination
        stratiply(
          
          #Extract all variables
          strata = all.vars(strata),
          
          #Send delimiter
          delimiter = strata_sep,
          
          #Base summary on each strata
          f = base_summary,
          
          #Bind results back together
          bind = TRUE,
          
          #Separate strata to original columns
          separate = TRUE
          
        )
      
      ###Tease out row vs. column strata
      #All row strata
      if(length(attr(terms(strata), "factors")) == 0) {
        
        #Assign non-null value
        row_strata <- all.vars(strata)
        
        #Combine all strata to a single column
        results <-
          results %>%
          tidyr::unite(
            col = ".row_strata",
            row_strata,
            sep = strata_sep
          )
        
        #All column strata    
      } else if(attr(terms(strata), "response") == 0) {
        
        #Assign non-null value
        col_strata <- all.vars(strata)
        
        #Unite all strata to single variable
        results <-
          results %>%
          tidyr::unite(
            col = ".col_strata",
            col_strata,
            sep = strata_sep
          )
        
        #Row and column strata
      } else {
        
        #Assign non-null values
        col_strata <-
          colnames(
            attr(terms(strata), "factors")
          )
        row_strata <-
          setdiff(
            all.vars(strata),
            col_strata
          )
        
        #Unite columns to respective strata
        results <-
          results %>%
          tidyr::unite(
            col = ".row_strata",
            row_strata,
            sep = strata_sep
          ) %>%
          tidyr::unite(
            col = ".col_strata",
            col_strata,
            sep = strata_sep
          )
        
      }
      
      #Convert strata variables to factors
      results <-
        results %>%
        dplyr::mutate_at(
          dplyr::vars(
            tidyselect::matches("^[.](row_strata|col_strata)$")
          ),
          forcats::as_factor
        )
      
      #Add a sample size if requested
      if(add_n) {
        
        get_n <-
          function(.strat, .colname) {
            
            data %>%
              
              #Group by stratification variables
              dplyr::group_by_at(.strat) %>%
              
              #Compute sample size
              dplyr::summarise(.n = dplyr::n()) %>%
              dplyr::ungroup() %>%
              
              #Remove NA levels
              na.omit() %>%
              
              #Combine columns to match with results
              tidyr::unite_(
                col = .colname,
                .strat,
                sep = strata_sep
              ) %>%
              
              #Make factor for type matching
              dplyr::mutate_at(
                .colname,
                function(.z) {
                  
                  forcats::fct_relevel(
                    factor(.z),
                    
                    #Get current levels
                    results %>% 
                      dplyr::pull(.colname) %>%
                      levels()
                  )
                  
                }
              )
            
          }
        
        #Retrieve for row strata
        if(!is.null(row_strata)) {
          
          results <-
            results %>%
            
            #Call temporary function and join
            dplyr::inner_join(
              y =
                get_n(
                  .strat = row_strata,
                  .colname = ".row_strata"
                ),
              by = ".row_strata"
            ) %>%
            
            #Concatenate sample size and remove
            dplyr::mutate(
              .row_strata =
                stringr::str_c(
                  .data$.row_strata, " (N=", .data$.n, ")"
                ) %>%
                forcats::as_factor()
            ) %>%
            dplyr::select(-.data$.n)
          
        }
        
        #Retrieve for column strata
        if(!is.null(col_strata)) {
          
          results <-
            results %>%
            dplyr::inner_join(
              y =
                get_n(
                  .strat = col_strata,
                  .colname = ".col_strata"
                ),
              by = ".col_strata"
            ) %>%
            
            #Concatenate sample size and remove
            dplyr::mutate(
              .col_strata =
                stringr::str_c(
                  .data$.col_strata, " (N=", .data$.n, ")"
                ) %>%
                forcats::as_factor()
            ) %>%
            dplyr::select(-.data$.n)
          
        }
        
      }
      
      #Span summaries across the columns
      if(!is.null(col_strata)) {
        
        results <-
          results %>%
          stretch(
            keys = ".col_strata",
            keep = tidyselect::matches("^[.](row_strata|variable|level|order)"),
            .sep = summary_strata_sep,
            extract_keys_as_header = TRUE
          )
        
        #Check if multiple columns were spanned
        if(methods::is(results, "list")) {
          
          #Get base header
          header <- results$.header
          
          #Set results
          results <- results$.result
          
        }
        
        #Compute association statistics if requested
        if(!is.null(associations)) {
          
          #Temporary function to extract association statistics
          get_associations <-
            function(.d) {
              
              .d %>%
                
                #Combine columns
                tidyr::unite(
                  col = ".col_strata",
                  col_strata,
                  sep = strata_sep
                ) %>%
                
                #Get metrics
                univariate_associations(
                  f = associations,
                  responses = ".col_strata"
                ) %>%
                
                #Remove response column
                dplyr::select(-.data$.left)
              
            }
          
          #Check for row strata
          if(is.null(row_strata)) {
            
            #Obtain associations on entire data
            association_results <-
              data %>%
              get_associations
            
            #Join with results
            results <-
              results %>%
              dplyr::inner_join(
                y = association_results,
                by = ".variable"
              )
            
          } else {
            
            #Add temporary column to join by in case of sample size
            if(add_n) {
              
              results <-
                results %>%
                
                #Parse out the sample size addition
                dplyr::mutate(
                  .row_strata2 =
                    stringr::str_remove(
                      .data$.row_strata,
                      "\\s[(]N=.+[)]$"
                    ) %>%
                    forcats::as_factor()
                )
              
            } else {
              
              results <-
                results %>%
                
                #Copy original
                dplyr::mutate(
                  .row_strata2 = .data$.row_strata
                )
              
            }
            
            #Now compute associations
            association_results <-
              data %>%
              
              #Compute for each row strata
              stratiply(
                strata = row_strata,
                f = get_associations,
                delimiter = strata_sep,
                bind = TRUE,
                separate = FALSE
              ) %>%
              dplyr::mutate_at(
                ".strata",
                forcats::as_factor
              )
            
            #Join with results
            results <-
              results %>%
              dplyr::inner_join(
                y = association_results,
                by =
                  c(
                    ".row_strata2" = ".strata",
                    ".variable"
                  )
              ) %>%
              
              #Remove temporary column
              dplyr::select(-.data$.row_strata2)
          }
          
          #Extract names of association metric columns
          association_names <-
            tidyselect::vars_select(
              names(association_results),
              -tidyselect::matches("^[.](variable|strata)")
            )
          
          #Append the header if needed
          if(!is.null(header)) {
            
            header <-
              c(
                header,
                rep(" ", length(association_names))
              )
            
          }
        }
        
      }
      
    }
    
    #Consolidate ordering of variables
    if(is.null(order)) {
      
      #Use data order by default
      order <- setdiff(names(data), all.vars(strata))
      
    } else {
      
      #Use provided order then finish with maintaining original column order
      order <-
        c(
          order,
          setdiff(
            names(data),
            c(order, all.vars(strata))
          )
        )
      
    }
    
    #Extract summary variable names
    summary_vars <-
      tidyselect::vars_select(
        names(results),
        -tidyselect::matches("^[.](row_strata|variable|level|order)$")
      )
    
    #Remove more columns if needed
    if(!is.null(association_names)) {
      
      summary_vars <-
        tidyselect::vars_select(
          summary_vars,
          -tidyselect::one_of(association_names)
        )
      
    }
    
    #Reorder the variables
    results <-
      results %>%
      
      #Create a factor
      dplyr::mutate(
        .variable = 
          forcats::fct_relevel(
            factor(.data$.variable),
            order
          )
      )
    
    #Check for groupings
    if(!is.null(row_strata)) {
      
      results <-
        results %>%
        
        #Group by row strata
        dplyr::group_by(.data$.row_strata) %>%
        
        #Arrange within row strata
        dplyr::arrange(
          .data$.variable,
          .by_group = TRUE
        ) 
      
    } else {
      
      results <-
        results %>%
        
        #Arrange over entire frame
        dplyr::arrange(.data$.variable)
      
    }
    
    #Coerce back to character
    results <-
      results %>%
      dplyr::mutate(
        .variable = as.character(.data$.variable)
      )
    
    #Tidy up factor levels
    cat_vars <- FALSE
    if(".order" %in% names(results)) {
      
      #Set to try
      cat_vars <- TRUE
      
      results <-
        results %>%
        
        #Replace explicit NA levels with string
        dplyr::mutate(
          .level = 
            dplyr::case_when(
              !is.na(.data$.order) & is.na(.data$.level) & .data$.order > 0 ~ na_string,
              TRUE ~ .data$.level
            )
        )
      
      #Relabel the levels if needed
      if(!is.null(levels)) {
        
        #Make relational data frame
        levels <-
          levels %>%
          purrr::map_df(
            ~
              tibble::tibble(
                .level = names(.x),
                .newLevel = unname(.x)
              ),
            .id = ".variable"
          )
        
        #Join with results
        results <-
          results %>%
          dplyr::left_join(
            y = levels,
            by = c(".variable", ".level")
          ) %>%
          
          #Get new levels
          dplyr::mutate(
            .level =
              dplyr::case_when(
                !is.na(.data$.newLevel) ~ .data$.newLevel,
                TRUE ~ .data$.level
              )
          ) %>%
          
          #Remove new levels
          dplyr::select(
            -.data$.newLevel
          )
        
      }
      
    }
    
    #Fill in remaining blank spots in table
    results <-
      results %>%
      
      #Apply to columns with at least 1 NA
      dplyr::mutate_if(
        function(.x) any(is.na(.x)),
        dplyr::funs(
          dplyr::case_when(
            is.na(.) ~ fill_blanks,
            TRUE ~ as.character(.)
          )
        )
      )
    
    #Relabel variables if needed
    if(!is.null(labels)) {
      
      #Make a relational frame
      labels <-
        tibble::tibble(
          .variable = names(labels),
          .newVariable = unname(labels)
        )
      
      #Join with results
      results <-
        results %>%
        dplyr::left_join(
          y = labels,
          by = ".variable"
        ) %>%
        dplyr::mutate(
          .variable =
            dplyr::case_when(
              !is.na(.data$.newVariable) ~ .data$.newVariable,
              TRUE ~ as.character(.data$.variable)
            )
        ) %>%
        dplyr::select(
          -.data$.newVariable
        )
      
    }
    
    #Add a group for the variable
    results <-
      results %>%
      
      #Apply within variables
      dplyr::group_by(
        .data$.variable,
        add = TRUE
      ) 
    
    #Remove duplicate function results
    if(!is.null(association_names)) {
      
      results <-
        results %>%
        
        #Apply function to association columns
        dplyr::mutate_at(
          dplyr::vars(
            tidyselect::one_of(association_names)
          ),
          dplyr::funs(
            dplyr::case_when(
              duplicated(.) ~ fill_blanks,
              TRUE ~ as.character(.)
            )
          )
        ) 
      
    }
    
    #Ungroup data
    results <-
      results %>%
      dplyr::ungroup()
    
    row_strata_vec <- NULL
    #Only group by row_strata if in data
    if(!is.null(row_strata)) {
      
      #Keep a vector of the row_strata
      row_strata_vec <- 
        results %>%
        dplyr::pull(.data$.row_strata)
      
      #Group by strata
      results <-
        results %>%
        dplyr::group_by(.data$.row_strata)
      
    }
    
    #Get desired format
    format <- match.arg(format)
    
    #Remove duplicate variable names if needed
    if(format %in% c("pandoc", "markdown", "none")) {
      
      #Remove duplicate variable names
      results <-
        results %>%
        dplyr::mutate(
          .variable =
            dplyr::case_when(
              duplicated(.data$.variable) ~ fill_blanks,
              TRUE ~ .data$.variable
            )
        )
      
    }
    
    #Remove ordering variable and rename columns
    results <-
      results %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -tidyselect::matches("^[.]order$")
      ) %>%
      
      #Provide specified column names
      dplyr::rename_all(
        dplyr::recode,
        .variable = variableName,
        .level = levelName
      ) %>%
      dplyr::rename_at(
        dplyr::vars(
          tidyselect::matches("^[.]row_strata$")
        ),
        function(.z) stringr::str_c(row_strata, collapse = strata_sep)
      )
    
    #Final formatting depending on type of rendering
    if(format %in% c("pandoc", "markdown", "none")) {
      
      #Remove duplicated row strata
      if(!is.null(row_strata)) {
        
        results <-
          results %>%
          dplyr::mutate_at(
            stringr::str_c(row_strata, collapse = strata_sep),
            dplyr::funs(
              dplyr::case_when(
                duplicated(.) ~ fill_blanks,
                TRUE ~ as.character(.)
              )
            )
          )
      }
      
      #Return result
      if(format == "none") {
        
        results
        
      } else {
        
        #Make a kable
        results %>%
          knitr::kable(
            format = format,
            caption = caption
          )
        
      }
      
    } else {
      
      #Remove row_strata from frame if it's there
      results <-
        results %>%
        dplyr::select(
          -tidyselect::matches(stringr::str_c("^", stringr::str_c(row_strata, collapse = strata_sep), "$"))
        )
      
      #If there is a header then remove strata
      if(!is.null(header)) {
        
        results <-
          results %>%
          dplyr::rename_at(
            summary_vars,
            stringr::str_remove,
            pattern = stringr::str_c(summary_strata_sep, ".+")
          )
        
      }
      
      #Make base result
      results <-
        results %>%
        knitr::kable(
          format = format,
          caption = caption
        ) %>%
        
        #Style the output
        kableExtra::kable_styling(
          full_width = FALSE,
          bootstrap_options = c("striped", "responsive"),
          latex_options = c("striped", "responsive")
        )
      
      #Group by row strata if needed
      if(!is.null(row_strata)) {
        
        results <-
          results %>%
          kableExtra::group_rows(
            index = 
              kableExtra::auto_index(
                as.character(row_strata_vec)
              )
          )
        
        #Remove an entry from the header
        header <- header[-1]
        
      }
      
      #Collapse variable column
      results <-
        results %>%
        kableExtra::collapse_rows(1, valign = "top")
      
      #Check for header
      if(!is.null(header)) {
        
        #Remove the entry for ".order"
        if(cat_vars) 
          header <- header[-1]
        
        #Add header to table and return
        results %>%
          kableExtra::add_header_above(
            kableExtra::auto_index(
              header
            )
          )
        
      } else {
        
        results
        
      }
    }
    
  }

###FUNCTIONS TO BE WRITTEN

#Name: explore
#Description: Explore non-linear relationships and interactions with flexible models

#Name: regression_table
#Description: Make a custom table for regression models

#Name: wander
#Description: Reduce a set of columns from a data frame by user-specified functions, stopping criteria, comparator values, etc.
