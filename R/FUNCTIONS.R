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
        dplyr::select(data, tidyselect::all_of(selected_vars[[1]])),
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
                dplyr::select(.x, tidyselect::all_of(selected_vars[[i + 1]])),
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
    
    #Collapse indices and add boundaries
    stringr::str_c(
      "{",
      stringr::str_c(
        result,
        collapse = ","
      ),
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
#Description: Evaluate a function on combinations of columns
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
      
      #Trace if desired
      if(trace)
        cat(text, "\n")
      
      #Replace all substrings in text with values that match the key
      text <- text %>% stringr::str_replace_all(pattern = lookup$key[i], replacement = lookup$value[i])
      
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
          
          count = table,
          proportion = ~prop.table(table(., useNA = useNA)),
          percent = ~prop.table(table(., useNA = useNA))*100
          
        )
      
    ) %>%
      
      #Convert everything to a function
      purrr::map_depth(
        .depth = 2,
        rlang::as_function
      )
    
  }
