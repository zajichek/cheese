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

#Name: order_levels
#Description: Returns order of combinations of levels
order_levels <-
  function(
    data,
    sep = "_"
  ) {
    
    #Find set of unique values for each variable
    sets <- 
      data %>%
      purrr::map(unique)
    
    #Get rank order of levels within each column
    ranks <-
      sets %>%
      purrr::map_df(
        ~
          tibble::tibble(
            name = as.character(.x),
            value = order(.x)
          ),
        .id = "column"
      )
    
    sets %>%
      
      #Convert to character
      purrr::map(as.character) %>%
      
      #Get cross-product for levels
      purrr::cross_df() %>%
      
      #Add group identifier
      dplyr::mutate(
        .id = seq_len(nrow(.))
      ) %>%
      
      #Send down the rows
      tidyr::gather(
        key = "column",
        value = "name",
        -tidyselect::all_of(".id")
      ) %>%
      
      #Get rank of each level within each column
      dplyr::inner_join(
        y = ranks,
        by = c("column", "name")
      ) %>%
      
      #Within each combination
      dplyr::group_by_at(
        tidyselect::all_of(".id")
      ) %>%
      
      #Concatenate the levels
      dplyr::summarise(
        index = paste(value, collapse = ""),
        name = paste(name, collapse = sep)
      ) %>%
      dplyr::ungroup() %>%
      
      #Keep rank and level
      dplyr::transmute(
        index = order(order(index)),
        name = name
      )
    
  }

#Name: stretch
#Description: Span keys and values across the columns
stretch <-
  function(
    data,
    key,
    value,
    sep = "_"
  ) {
    
    #Check for keys
    if(missing(key))
      stop("No key(s) supplied.")
    
    #Splice the keys
    key <-
      tidyselect::eval_select(
        rlang::enquo(key),
        data
      ) %>%
      names
    
    #Gather column order for keys
    column_order <-
      data %>%
      dplyr::select(
        tidyselect::all_of(key)
      ) %>%
      order_levels(
        sep = sep
      )
    
    #Check for registration
    if(length(key) == 0)
      stop("No key(s) registered")
    
    #Check for values
    if(missing(value))
      stop("No value(s) supplied.")
    
    #Splice the keys
    value <-
      tidyselect::eval_select(
        rlang::enquo(value),
        data
      ) %>% 
      names
    
    #Check for registration
    if(length(value) == 0)
      stop("No value(s) registered")
    
    ##Create a single column for the keys
    data <-
      data %>%
      
      #Combine columns together
      tidyr::unite(
        col = ".key",
        key,
        sep = sep,
        remove = TRUE
      ) 
    
    #Filter column ordering to those existing in the data
    column_order <-
      column_order %>%
      dplyr::filter(
        name %in% purrr::pluck(data, ".key")
      )
    
    #Gather id variables
    id_vars <- setdiff(names(data), c(".key", value))
    
    #Collect column ordering
    temp_column_order <- tibble::tibble()
    
    #For each value column...
    result <- list()
    for(i in seq_along(value)) {
      
      #Extract the current value
      value_i <- value[i]
      
      #Make a temporary data frame
      temp_dat <-
        data %>%
        
        #Remove all value columns except the current one
        dplyr::select(
          -tidyselect::all_of(
            setdiff(value, value_i)
          )
        )
      
      #Append keys if needed
      if(length(value) > 1) {
        
        temp_dat <-
          temp_dat %>%
          
          #Append key column
          dplyr::mutate_at(
            ".key",
            ~paste0(.x, sep, value_i)
          )
        
        #Add to column ordering
        temp_column_order <-
          temp_column_order %>%
          dplyr::bind_rows(
            
            column_order %>%
              
              #Append value name and add index
              dplyr::mutate(
                name = paste0(name, sep, value_i),
                val_index = i
              )
            
          )
        
      }
      
      result[[i]] <-
        temp_dat %>% 
        
        #Send this value across the columns
        tidyr::spread(
          key = ".key",
          value = value[i]
        ) 
      
    }
    
    #Combine based on presence of id columns
    if(length(id_vars) > 0) {
      
      result <-
        result %>%
        
        #Iteratively join
        purrr::reduce(
          .f = dplyr::inner_join,
          by = id_vars
        )
      
    } else {
      
      result <-
        result %>%
        dplyr::bind_cols()
      
    }
    
    #Find final sorting order
    if(nrow(temp_column_order) == 0)
      temp_column_order <- column_order
    
    column_order <-
      temp_column_order %>%
      
      #Arrange by indices
      dplyr::arrange_at(
        dplyr::vars(
          tidyselect::any_of(
            c(
              "index",
              "val_index"
            )
          )
        )
      ) %>%
      
      #Extract vector
      dplyr::pull(name)
    
    #Rearrange the columns
    result %>%
      dplyr::select(
        tidyselect::all_of(
          c(
            id_vars,
            column_order
          )
        )
      )
  } 

#Name: grable
#Description: Make a hierarchical kable 
grable <-
  function(
    data, #A dataset
    at, #Columns to include in hierarchy
    sep = "_", #Delimiter for parsing
    reverse = FALSE, #Stack in opposite direction?
    format = c("html", "latex"), #Rendering format
    caption = NULL, #Table caption
    ... #Arguments passed to kableExtra::kable_styling
  ) {
    
    #Set to all columns if missing input
    if(missing(at)) {
      
      #Extract all indices
      at <- seq_along(names(data))
      
      #Set names
      names(at) <- names(data)
      
    }
    
    #Splice variable names
    selected_vars <-
      tidyselect::eval_select(
        rlang::enquo(at),
        data
      ) 
    
    #Return error if no split variables entered
    if(length(selected_vars) == 0)
      stop("No columns registered.")
    
    #Split to create header matrix
    headers <- 
      selected_vars %>% 
      names %>% 
      stringr::str_split(
        pattern = sep,
        simplify = TRUE
      )
    
    #Iterable index vector
    index <- rev(seq_len(ncol(headers)))
    
    #Reverse if needed
    if(reverse)
      index <- rev(index)
    
    #Replace names of data with first level
    names(data)[selected_vars] <- headers[,index[1]]
    
    #Get format
    format <- match.arg(format)
    
    #Make initial kable
    result <-
      data %>%
      knitr::kable(
        format = format,
        caption = caption
      ) %>%
      kableExtra::kable_styling(...)
    
    #Iteratively add layers
    if(length(index) > 1) {
      
      #Make a template
      template <- rep(" ", ncol(data))
      
      for(i in 2:length(index)) {
        
        #Make index
        temp_template <- template
        temp_template[selected_vars] <- headers[,index[i]]
        
        #Add empty spots 
        result <-
          result %>%
          kableExtra::add_header_above(
            kableExtra::auto_index(
              temp_template
            )
          )
        
      }
      
    }
    
    result
    
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
          
          count = ~table(., useNA = useNA),
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

#Name: descriptives_component
#Description: Internal function to build a subset of the descriptives() call for a given result
descriptives_component <-
  function(res, na_string) {
    
    #Check if the input is numeric or character
    if(!some_type(res, c("numeric", "character", "logical", "Date", "table")))
      stop("Function must return a an atomic vector or table.")
    
    #Check if result has names
    names <- NA_character_
    if(!is.null(names(res))) 
      names <- dplyr::coalesce(names(res), na_string)
    
    #Extract values from a table
    if(is.table(res))
      res <- as.numeric(res)
    
    #Add result to a data frame
    temp_result <-
      tibble::tibble(
        val_ind = seq_along(res),
        val_lab = names,
        val_dbl = res
      )
    
    #Change label for non-numeric output
    if(!some_type(res, "numeric"))
      names(temp_result)[3] <- "val_chr"
    
    #Return results
    temp_result
    
  }

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
    useNA = c("ifany", "no", "always"),
    round = 2,
    na_string = "(missing)"
  ) {
    
    #Get default functions
    useNA <- match.arg(useNA)
    default_functions <- default_univariate_functions(useNA = useNA)
    
    #Add user-specified functions
    f_all <- c(f_all, default_functions$f_all)
    f_numeric <- c(f_numeric, default_functions$f_numeric)
    f_categorical <- c(f_categorical, default_functions$f_categorical)
    
    ###Evaluate functions for...
    #All columns
    all_results <-
      f_all %>%
      
      #Apply each function to each column
      purrr::map(
        ~
          data %>%
          purrr::map(
            .f = .x
          )
      )
    
    #Numeric columns
    numeric_results <-
      f_numeric %>%
      
      #Apply function for each type
      purrr::map(
        typly,
        data = data,
        types = numeric_types
      ) 
    
    #Categorical columns
    categorical_results <-
      f_categorical %>%
      
      #Apply function for each type
      purrr::map(
        typly,
        data = data,
        types = categorical_types
      )
    
    #Other columns
    other_results <- NULL
    if(!is.null(f_other)) {
      
      other_results <-
        f_other %>%
        
        #Apply function excluding each type
        purrr::map(
          typly,
          data = data,
          types = c(numeric_types, categorical_types),
          negated = TRUE
        )
      
    }
    
    #Get column indices
    columns <- names(data)
    
    #Build a data frame with the results
    list(
      all = all_results,
      numeric = numeric_results,
      categorical = categorical_results,
      other = other_results
    ) %>%
      purrr::map_depth(
        .depth = 3,
        descriptives_component,
        na_string = na_string
      ) %>%
      
      #Bind columns
      fasten(
        into = c("fun_eval", "fun_key", "col_lab")
      ) %>%
      
      #Join to get column order
      dplyr::inner_join(
        y =
          tibble::tibble(
            col_lab = columns,
            col_ind = seq_along(columns)
          ),
        by = "col_lab"
      ) %>%
      
      #Rearrange columns
      dplyr::select(
        tidyselect::all_of(
          c(
            "fun_eval",
            "fun_key",
            "col_ind",
            "col_lab"
          )
        ),
        tidyselect::everything()
      ) %>%
      
      #Add consolidated summary column
      dplyr::mutate(
        val_cbn = dplyr::coalesce(as.character(round(val_dbl, round)), val_chr)
      ) %>%
      
      #Rearrange rows
      dplyr::arrange_at(
        dplyr::vars(
          tidyselect::all_of(
            c(
              "fun_eval",
              "fun_key",
              "col_ind",
              "val_ind"
            )
          )
        )
      )
  }

#Name: univariate_associations
#Description: Evaluate a list of functions to a variable for each response
univariate_associations <-
  function(
    data,
    f,
    responses,
    predictors
  ) {
    
    #Check for function
    if(missing(f))
      stop("No function(s) supplied.")
    
    #Check for response inputs
    if(missing(responses)) {
      
      #Check for predictor inputs
      if(missing(predictors)) {
        
        #Set both sides to all variables
        responses <-
          tidyselect::eval_select(
            rlang::expr(names(data)),
            data
          )
        
        predictors <- responses
        
      } else {
        
        #Splice predictor variables
        predictors <-
          tidyselect::eval_select(
            rlang::enquo(predictors),
            data
          )
        
        #Check for registration
        if(length(predictors) == 0)
          stop("No predictor variables registered.")
        
        #Response variables are everything else
        responses <-
          tidyselect::eval_select(
            rlang::expr(names(data)[-predictors]),
            data
          )
        
      }
      
    } else {
      
      #Splice response variables
      responses <-
        tidyselect::eval_select(
          rlang::enquo(responses),
          data
        )
      
      #Check for registration
      if(length(responses) == 0)
        stop("No response variables registered.")
      
      #Check for predictor inputs
      if(missing(predictors)) {
        
        #Predictor variables are everything else
        predictors <-
          tidyselect::eval_select(
            rlang::expr(names(data)[-responses]),
            data
          )
        
      } else {
        
        #Splice predictor variables
        predictors <-
          tidyselect::eval_select(
            rlang::enquo(predictors),
            data
          )
        
        #Check for registration
        if(length(predictors) == 0)
          stop("No predictor variables registered.")
        
      }
      
    }
    
    #Make f a list if its a single function
    if(methods::is(f, "function"))
      f <- list(f)
    
    f %>%
      
      #Map each function
      purrr::imap(
        ~
          data %>% 
          
          #Get list of association statistics
          dish(
            f = .x,
            left = tidyselect::all_of(responses),
            right = tidyselect::all_of(predictors)
          ) %>%
          
          #Convert result to tibble
          purrr::map_depth(
            .depth = 2,
            ~
              .x %>%
              
              #Make tibble
              tibble::enframe(
                value = "value"
              ) %>%
              
              #Remove key
              dplyr::select(
                -tidyselect::all_of("name")
              )
          ) %>%
          
          #Zip up result
          fasten(
            into = c("response", "predictor")
          ) %>%
          
          #Rename value to match function
          dplyr::rename_at(
            "value",
            function(x) .y
          )
      ) %>%
      
      #Iteratively join
      purrr::reduce(
        dplyr::inner_join,
        by =
          c(
            "response",
            "predictor"
          )
      )
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
    
    #Vector function evaluation types
    eval_types <- unique(variable$fun_eval)
    
    #Set parameters for summarizing
    use_groups <- c("col_lab", "val_ind")
    if("numeric" %in% eval_types) {
      
      use_summary <- numeric_summary
      use_filter <- "numeric"
      
    } else if("categorical" %in% eval_types) {
      
      use_summary <- categorical_summary
      use_filter <- "categorical"
      use_groups <- c(use_groups, "val_lab")
      
    } else {
      
      use_summary <- other_summary
      use_filter <- "other"
      
    }
    
    #Populate strings templates
    results <-
      variable %>%
      
      #Filter to categorical functions only
      dplyr::filter(
        fun_eval %in% use_filter
      )
    
    #Check if results exist
    if(nrow(results) > 0) {
      
      results <-
        results %>%
        
        #For each level
        dplyr::group_by_at(
          dplyr::vars(
            tidyselect::all_of(use_groups)
          )
        ) %>%
        
        #Absorb the strings
        dplyr::do(
          absorb(
            key = .data$fun_key,
            value = .data$val_cbn,
            text = use_summary,
            evaluate = evaluate
          ) %>%
            
            #Make a bindable tibble
            as.list() %>%
            purrr::map_df(~tibble::tibble(sum_val = as.character(.x)), .id = "sum_lab")
        ) %>%
        dplyr::ungroup()
      
    } else {
      
      results <- NULL
      
    }
    
    #Add all summary if requested
    if(!is.null(all_summary)) {
      
      #Filter to subset with all functions
      variable_all <-
        variable %>%
        dplyr::filter(
          fun_eval == "all"
        )
      
      #Add extra row for categorical variables only
      val_lab_all <- 1
      if("categorical" %in% eval_types)
        val_lab_all <- 0
      
      results <- 
        results %>%
        
        #Bind additional summary to results
        dplyr::bind_rows(
          
          absorb(
            key = variable_all$fun_key,
            value = variable_all$val_cbn,
            text = all_summary,
            evaluate = evaluate
          ) %>%
            
            #Make a bindable tibble
            as.list() %>%
            purrr::map_df(~tibble::tibble(sum_val = as.character(.x)), .id = "sum_lab") %>%
            
            #Put index at zero
            tibble::add_column(
              col_lab = variable_all$col_lab[1],
              val_ind = val_lab_all
            )
          
        )
      
    }
    
    results
    
  }

#Name: absorb_descriptives
#Description: Gathers the absorbed descriptives for all variables
absorb_descriptives <-
  function(
    data,
    numeric_summary,
    categorical_summary,
    other_summary,
    all_summary,
    evaluate,
    ...
  ) {
    
    data %>%
      
      #Get descriptive statistics
      descriptives(...) %>%
      
      #Absorb into text strings for each variable
      stratiply(
        f = absorb_descriptive_variable,
        by = col_ind,
        numeric_summary = numeric_summary,
        categorical_summary = categorical_summary,
        other_summary = other_summary,
        all_summary = all_summary,
        evaluate = evaluate
      ) %>%
      
      #Bind back together
      dplyr::bind_rows(
        .id = "col_ind"
      ) %>%
      
      #Convert to factor to maintain order of summaries
      dplyr::mutate(
        sum_lab = forcats::as_factor(sum_lab)
      ) %>%
      
      #Spread over the columns
      tidyr::spread(
        key = sum_lab,
        value = sum_val
      ) 
    
  }

#Name: univariate_table
#Description: Create a custom table with univariate descriptive statistics
univariate_table <-
  function(
    data,
    strata = NULL,
    associations = NULL,
    numeric_summary = c(Summary = "median (q1, q3)"),
    categorical_summary = c(Summary = "count (percent%)"),
    other_summary = NULL,
    all_summary = NULL,
    evaluate = FALSE,
    add_n = FALSE,
    order = NULL,
    labels = NULL,
    levels = NULL,
    format = c("html", "latex", "markdown", "pandoc", "none"),
    variableName = "Variable",
    levelName = "Level",
    sep = "_",
    fill_blanks = "",
    caption = NULL,
    ...
  ) {
    
    #Set default values
    col_strata <- NULL
    row_strata <- character(0)
    
    #Extract all stratification variables
    strata_vars <- all.vars(strata)
    
    #Summarize entire data set if no stratification columns
    if(length(strata_vars) == 0) {
      
      results <-
        data %>%
        absorb_descriptives(
          numeric_summary = numeric_summary,
          categorical_summary = categorical_summary,
          other_summary = other_summary,
          all_summary = all_summary,
          evaluate = evaluate,
          ...
        )
      
      #Otherwise summarize within each group  
    } else {
      
      results <-
        data %>%
        stratiply(
          f = absorb_descriptives,
          by = tidyselect::all_of(strata_vars),
          numeric_summary = numeric_summary,
          categorical_summary = categorical_summary,
          other_summary = other_summary,
          all_summary = all_summary,
          evaluate = evaluate,
          ...
        ) %>%
        
        #Bind back together
        fasten(
          into = strata_vars
        )
      
      #Differentiate row/column strata
      col_strata <- colnames(attr(terms(strata), "factors"))
      row_strata <- setdiff(strata_vars, col_strata)
      
    }
    
    #Compute stratification group sample size if requested
    if(add_n) {
      
      #Check for column strata
      if(!is.null(col_strata)) {
        
        results <-
          results %>%
          
          #Join to get sample size
          dplyr::inner_join(
            y = 
              data %>%
              
              #Group by column strata
              dplyr::group_by_at(
                dplyr::vars(
                  tidyselect::all_of(col_strata)
                )
              ) %>%
              
              #Compute sample size
              dplyr::summarise(
                col_N = dplyr::n()
              ) %>%
              dplyr::ungroup() %>%
              
              #Convert to character types
              dplyr::mutate_at(
                dplyr::vars(
                  tidyselect::all_of(col_strata)
                ),
                as.character
              ),
            by = col_strata
          ) %>%
          
          #Concatenate sample size to last col_strata 
          dplyr::mutate_at(
            dplyr::vars(
              tidyselect::all_of(col_strata[length(col_strata)])
            ),
            ~paste0(.x, " (N=", col_N, ")")
          ) %>%
          
          #Remove sample size column
          dplyr::select(
            -col_N
          )
        
      }
      
      #Check for row strata
      if(length(row_strata) > 0) {
        
        results <-
          results %>%
          
          #Join to get sample size
          dplyr::inner_join(
            y = 
              data %>%
              
              #Group by column strata
              dplyr::group_by_at(
                dplyr::vars(
                  tidyselect::all_of(row_strata)
                )
              ) %>%
              
              #Compute sample size
              dplyr::summarise(
                row_N = dplyr::n()
              ) %>%
              dplyr::ungroup() %>%
              
              #Convert to character types
              dplyr::mutate_at(
                dplyr::vars(
                  tidyselect::all_of(row_strata)
                ),
                as.character
              ),
            by = row_strata
          ) %>%
          
          #Concatenate sample size to last col_strata 
          dplyr::mutate_at(
            dplyr::vars(
              tidyselect::all_of(row_strata[length(row_strata)])
            ),
            ~paste0(.x, " (N=", row_N, ")")
          ) %>%
          
          #Remove sample size column
          dplyr::select(
            -row_N
          )
        
      }
      
    }
    
    results <-
      results %>%
      
      #Ensure all_summary are the last columns
      dplyr::select(
        -tidyselect::any_of(names(all_summary)),
        tidyselect::any_of(names(all_summary))
      )
    
    #Span results across columns
    if(!is.null(col_strata)) {
      
      results <-
        results %>%
        
        #Convert to factor to maintain order
        dplyr::mutate_at(
          dplyr::vars(
            tidyselect::all_of(col_strata)
          ),
          forcats::as_factor
        ) %>%
        
        #Send all results across by the strata
        stretch(
          key = tidyselect::all_of(col_strata),
          value = -tidyselect::any_of(c(row_strata, col_strata, "col_ind", "col_lab", "val_ind", "val_lab"))
        )
      
    }
    
    #Gather association metrics
    association_results <- NULL
    if(!is.null(associations)) {
      
      #Ensure column strata are available
      if(is.null(col_strata))
        stop("Association metrics can only be computed with column strata.")
      
      #Create secondary dataset
      temp_data <-
        data %>%
        
        #Combine column strata so they are compared across all groups
        tidyr::unite(
          col = "col_strata",
          tidyselect::all_of(col_strata),
          sep = sep
        )
      
      #If there are row strata, run association metrics within
      if(length(row_strata) > 0) {
        
        #Append the sample size to the row strata variable so joining works
        if(add_n) {
          
          temp_data <-
            temp_data %>%
            
            #Join to get sample size
            dplyr::inner_join(
              y = 
                data %>%
                
                #Group by column strata
                dplyr::group_by_at(
                  dplyr::vars(
                    tidyselect::all_of(row_strata)
                  )
                ) %>%
                
                #Compute sample size
                dplyr::summarise(
                  row_N = dplyr::n()
                ) %>%
                dplyr::ungroup(),
              by = row_strata
            ) %>%
            
            #Concatenate sample size to last col_strata 
            dplyr::mutate_at(
              dplyr::vars(
                tidyselect::all_of(row_strata[length(row_strata)])
              ),
              ~paste0(as.character(.x), " (N=", row_N, ")")
            ) %>%
            
            #Remove sample size column
            dplyr::select(
              -row_N
            )
          
        }
        
        association_results <-
          temp_data %>%
          
          #Evaluate functions on each row strata
          stratiply(
            f = univariate_associations,
            by = tidyselect::all_of(row_strata),
            associations,
            predictors = tidyselect::all_of("col_strata")
          ) %>%
          
          #Bind results together
          fasten(
            into = row_strata
          ) 
        
      } else {
        
        association_results <-
          temp_data %>%
          univariate_associations(
            f = associations,
            predictors = tidyselect::all_of("col_strata")
          )
        
      }
      
      #Determine index to join on depending on type (0 for categorical, 1 for everything else)
      association_results$val_ind <- as.numeric(!(association_results$response %in% unique(results$col_lab[results$val_ind != 1])))
      
      #Extract the col_ind to maintain order
      association_results <-
        association_results %>%
        
        #Join with distinct labels/indices
        dplyr::inner_join(
          y = 
            results %>%
            
            #Get distinct col_ind, col_labs
            dplyr::select(
              col_ind,
              col_lab
            ) %>%
            dplyr::distinct(),
          by = c("response" = "col_lab")
        ) %>%
        
        #Remove predictor column
        dplyr::select(
          -predictor
        )
      
      #Join back to results
      results <-
        results %>%
        
        #Join on required columns
        dplyr::full_join(
          y = association_results,
          by = c(row_strata, "col_ind", "col_lab" = "response", "val_ind")
        )
      
    }
    
    #Reassign col_ind if needed
    if(!is.null(order)) {
      
      results <-
        results %>%
        dplyr::mutate(
          
          #Convert to factor and apply releveling
          col_lab = forcats::fct_relevel(factor(col_lab), order),
          
          #Get numeric order
          col_ind = as.numeric(col_lab),
          
          #Convert back to character
          col_lab = as.character(col_lab)
          
        )
      
    }
    
    #Relevel if requested
    if(!is.null(levels)) {
      
      results <-
        results %>%
        
        #Keep all result rows
        dplyr::left_join(
          y =
            levels %>%
            
            #Convert each vector to a data frame
            purrr::map_df(
              tibble::enframe,
              name = "val_lab",
              value = "val_lab_new",
              .id = "col_lab"
            ),
          by = c("col_lab", "val_lab")
        ) %>%
        
        #Replace levels with new levels if present
        dplyr::mutate(
          val_lab = dplyr::coalesce(val_lab_new, val_lab)
        ) %>%
        
        #Remove column of new labels
        dplyr::select(
          -val_lab_new
        )
      
    }
    
    #Relabel if requested
    if(!is.null(labels)) {
      
      results <-
        results %>%
        
        #Keep all result rows
        dplyr::left_join(
          y = 
            labels %>%
            tibble::enframe(
              name = "col_lab",
              value = "col_lab_new"
            ),
          by = "col_lab"
        ) %>%
        
        #Replace labels with new labels if present
        dplyr::mutate(
          col_lab = dplyr::coalesce(col_lab_new, col_lab)
        ) %>%
        
        #Remove new labels
        dplyr::select(
          -col_lab_new
        )
      
    }
    
    #Arrange results by indices
    results <-
      results %>%
      dplyr::arrange_at(
        dplyr::vars(
          tidyselect::all_of(c(row_strata, "col_ind", "val_ind"))
        )
      ) %>%
      
      #Remove index columns
      dplyr::select(
        -col_ind,
        -val_ind
      ) %>%
      
      #Provide specified column names
      dplyr::rename_all(
        dplyr::recode,
        col_lab = variableName,
        val_lab = levelName
      ) %>%
      
      #Fill in empty values
      dplyr::mutate_if(
        is.character,
        dplyr::coalesce,
        fill_blanks
      )
    
    #Proceed depending on render format
    format <- match.arg(format)
    if(format %in% c("html", "latex")) {
      
      #Make sequences up to collapse
      collapse_set1 <- seq_len(which(names(results) == variableName))
      collapse_set2 <- NULL
      if(!is.null(associations))
        collapse_set2 <- which(names(results) %in% setdiff(names(association_results), c(row_strata, "col_lab", "response")))
      
      #If there are no col strata, make kable
      if(is.null(col_strata)) {
        
        results <-
          results %>%
          knitr::kable(
            format = format,
            caption = caption
          ) %>%
          kableExtra::kable_styling(
            full_width = FALSE
          )
        
      } else {
        
        #Make a grable
        results <-
          results %>%
          grable(
            at = -tidyselect::any_of(c(row_strata, variableName, levelName, names(association_results))),
            sep = sep,
            format = format,
            caption = caption,
            full_width = FALSE
          ) 
        
      }
      
      #Remove duplicate rows
      results %>%
        kableExtra::collapse_rows(c(collapse_set1, collapse_set2), valign = "top")
      
    } else {
      
      #Remove duplicate association metrics
      if(!is.null(associations)) {
        
        results <-
          results %>%
          
          #Remove duplicate values
          dplyr::group_by_at(
            dplyr::vars(
              tidyselect::all_of(c(row_strata, variableName))
            )
          ) %>%
          dplyr::mutate_at(
            dplyr::vars(
              tidyselect::all_of(setdiff(names(association_results), c(row_strata, "col_lab", "response", "val_ind", "col_ind")))
            ),
            ~
              dplyr::case_when(
                duplicated(.x) ~ fill_blanks,
                TRUE ~ as.character(.x)
              )
          ) %>%
          dplyr::ungroup()
        
      }
      
      #Apply within row strata
      if(length(row_strata) > 0) {
        
        results <-
          results %>%
          
          #Group by row strata
          dplyr::group_by_at(
            dplyr::vars(
              tidyselect::all_of(row_strata)
            )
          )
        
      }
      
      #Remove duplicate variable names
      results <-
        results %>%
        dplyr::mutate_at(
          dplyr::vars(
            tidyselect::all_of(variableName)
          ),
          ~
            dplyr::case_when(
              duplicated(.x) ~ fill_blanks,
              TRUE ~ as.character(.x)
            )
        ) %>%
        dplyr::ungroup()
      
      #Remove duplicate row strata if needed
      if(length(row_strata) > 0) {
        
        #Iteratively group and remove if needed
        if(length(row_strata) > 1) {
          
          for(i in rev(seq_len(length(row_strata) - 1))) {
            
            results <-
              results %>%
              
              #Group by everything up to this variable
              dplyr::group_by_at(
                dplyr::vars(
                  tidyselect::all_of(row_strata[seq_len(i)])
                )
              ) %>%
              
              #Remove duplicates for this variable
              dplyr::mutate_at(
                dplyr::vars(
                  tidyselect::all_of(row_strata[i + 1])
                ),
                ~
                  dplyr::case_when(
                    duplicated(.x) ~ fill_blanks,
                    TRUE ~ as.character(.x)
                  )
              ) %>%
              dplyr::ungroup()
            
          }
          
        }
        
        #Remove duplicate for remaining row strata
        results <-
          results %>%
          dplyr::mutate_at(
            dplyr::vars(
              tidyselect::all_of(row_strata[1])
            ),
            ~
              dplyr::case_when(
                duplicated(.x) ~ fill_blanks,
                TRUE ~ as.character(.x)
              )
          ) %>%
          dplyr::ungroup()
        
      }
      
      #Return frame if no format requested
      if(format == "none") {
        
        results
        
      } else {
        
        results %>%
          knitr::kable(
            format = format,
            caption = caption
          )
        
      }
      
    }
    
  }