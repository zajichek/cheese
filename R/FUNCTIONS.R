#Created: 2019-10-14
#Author: Alex Zajichek
#Package: cheese
#Description: Function definitions for the package

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
    
    #Make quosure
    quos <- dplyr::quos(...)
    
    #Return error if no split variables entered
    if(rlang::is_empty(quos)) {
      
      stop("No columns provided.")
      
    } else {
      
      #Splice variable names; convert to list
      selected_vars <-
        tidyselect::vars_select(
          names(data),
          !!!quos
        ) %>%
        as.list() %>%
        unname()
      
      #Return error if no split variables entered
      if(length(selected_vars) == 0)
        stop("No columns registered.")
      
    }
    
    #Check that depth is numeric
    if(!is.numeric(depth))
      stop("Depth must be numeric.")
    
    #If depth is negative, adjust from max depth
    if(depth < 0) {
      
      #Use zero if it goes beyond
      depth <- max(length(selected_vars) + depth, 0)
      
    } else {
      
      #Use maximum depth as the limit
      depth <- min(depth, length(selected_vars))
      
    }
    
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
        dplyr::select(., tidyselect::one_of(selected_vars[[1]])),
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
                dplyr::select(., tidyselect::one_of(selected_vars[[i + 1]])),
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
              -tidyselect::one_of(
                selected_vars %>% 
                  purrr::flatten_chr()
              )
            )
        )
      
    }
    
    #Return result
    data
    
  }

#Name: depths
#Description: Traverse a list of arbitrary depth to find elements that satisfy a predicate
depths <-
  function(
    list, #A list, data frame or atomic vector
    predicate, #A binary function
    bare = TRUE, #Only continue on bare lists
    ... #Additional arguments for 'predicate'
  ) {
    
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
                  .f = depths, 
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
    
    #Check that depth is numeric
    if(!is.numeric(depth))
      stop("Depth must be numeric.")
    
    #Find depth of leaves
    depth_string <- 
      list %>%
      
      #Extract paths
      depths(
        predicate = is.data.frame,
        bare = TRUE
      ) %>%
      
      #Split into long character vector
      stringr::str_split(
        pattern = ""
      ) %>%
      
      #Extract first element
      purrr::pluck(1)
    
    #Check for a data frame
    if(!any(depth_string == "-"))
      stop("There must be data frames at the leaves.")
    
    #Get the depth where data frames are located
    bind_depth <- cumsum(depth_string == "{")[min(which(depth_string == "-"))]
    
    #If depth is negative, adjust from max depth
    if(depth < 0) {
      
      #Use zero if it goes beyond
      depth <- max(bind_depth + depth, 0)
      
    } else {
      
      #Use maximum depth as the limit
      depth <- min(depth, bind_depth)
      
    }
    
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
        warning(stringr::str_c(length(into), " column names given but only ", bind_depth - depth, " needed."))
        
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

#Name: pick
#Description: Select a subset of a list
pick <-
  function(
    list,
    ...
  ) {
    
    #Check if list is named
    named <- !is.null(names(list))
    
    #Set default names as the index
    if(!named) 
      names(list) <- 1:length(list)
    
    #Make quosure
    quos <- dplyr::quos(...)
    
    #Return error if no split variables entered
    if(rlang::is_empty(quos)) {
      
      stop("No names provided.")
      
    } else {
      
      #Splice variable names; convert to list
      selected_elements <-
        tidyselect::vars_select(
          names(list),
          !!!quos
        )
      
      #Return error if no split variables entered
      if(length(selected_elements) == 0)
        stop("No names registered.")
      
    }
    
    #Get subset of elements
    list <- list[names(list) %in% selected_elements]
    
    #Remove added names if needed
    if(!named)
      list <- unname(list)
    
    #Return list
    list
    
  }

#Name: stratiply
#Description: Stratify a data frame, apply a function, and collect results
stratiply <-
  function(
    data, #Any data.frame
    strata, #Stratification variables
    f, #Function to apply to each strata
    delimiter = "|", #Character to separate strata
    bind = FALSE, #Should results be binded?
    separate = TRUE, #Should strata be separated to original columns?
    ... #Additional arguments to pass to f
  ) { 
    
    #Get variables
    strata <-
      tidyselect::vars_select(
        names(data),
        strata
      )
    
    results <-
      data %>%
      
      #Divide frame
      divide(
        strata,
        sep = delimiter,
        depth = 1
      ) %>%
      
      #Apply function to each strata
      purrr::map(f, ...)
    
    #Post-process
    if(bind) {
      
      results <-
        results %>%
        dplyr::bind_rows(
          .id = ".strata"
        )
      
      if(separate | length(strata) == 1) {
        
        results <-
          results %>%
          tidyr::separate(
            col = .data$.strata,
            into = strata,
            sep = stringr::str_c("[", delimiter, "]"),
            convert = TRUE
          )
        
      } 
      
      results
      
    } else {
      
      results
      
    }
    
  }

#Name: stretch
#Description: Span one or more columns over many columns
stretch <-
  function(
    data, #Any data set
    keys, #Keys to spread across columns
    keep = NULL, #Variables to keep as their own column
    send = NULL, #Variables to send to other columns
    join = dplyr::full_join, #How should data be joined across strata?
    .sep = "_", #How should resulting columns separate keys?
    extract_keys_as_header = FALSE, #Should the keys be returned as a list component separate from the data?
    keep_keys_in_header = TRUE, #Should the keys be left in header when extracted?
    ... #Arguments to pass to the joining function
  ) {
    
    #Stratify data according to keys
    results <-
      data %>%
      
      #Divide data
      divide(
        by = keys,
        sep = .sep,
        depth = 1
      )
    
    #Stop if only 1 key
    if(length(results) <= 1)
      stop("Only 1 key found")
    
    #Get other variables
    keep <-
      tidyselect::vars_select(
        names(results[[1]]),
        keep
      )
    send <-
      tidyselect::vars_select(
        names(results[[1]]),
        send
      )
    
    #Remove overlapping send variables
    if(any(send %in% keep)) {
      
      send <- send[!(send %in% keep)]
      
    }
    
    #Find set of variables to select from
    if(length(keep) == 0) {
      
      if(length(send) == 0) {
        
        stop("keep and send cannot both be NULL")
        
      } else {
        
        #Send only matching columns, keep remaining as their own columns
        keep <-
          tidyselect::vars_select(
            names(results[[1]]),
            -send
          )
        
      }
      
      
    } else {
      
      #If both are supplied, remove unwanted columns from all frames
      if(length(send) > 0) {
        
        results <-
          results %>%
          purrr::map(
            ~
              .x %>%
              dplyr::select(
                keep,
                send
              )
          )
        
      } else {
        
        #Making send list for later use
        send <-
          tidyselect::vars_select(
            names(results[[1]]),
            -keep
          )
        
      }
      
    }
    
    results <-
      
      #Recursively join
      purrr::reduce2(
        .x = results[-1],
        .y = names(results)[-1],
        function(.w, .x, .y) {
          
          #Set x and y for iteration
          x <- .w
          y <-
            .x %>%
            dplyr::rename_at(
              dplyr::vars(
                -tidyselect::one_of(keep)
              ),
              function(.z) {
                
                if(length(send) == 1) {
                  
                  .y
                  
                } else {
                  
                  stringr::str_c(.z, .sep, .y)
                  
                }
                
              }
            )
          
          #Join if there are 'keep' variables
          if(length(keep) > 0) {
            
            join(
              x = x,
              y = y,
              by = keep,
              ...
            )
            
          } else {
            
            #Otherwise just bind columns together
            dplyr::bind_cols(
              x,
              y
            )
          }
          
        },
        .init =
          results[[1]] %>%
          dplyr::rename_at(
            dplyr::vars(
              -tidyselect::one_of(keep)
            ),
            function(.z) {
              
              if(length(send) == 1) {
                
                names(results)[1]
                
              } else {
                
                stringr::str_c(.z, .sep, names(results)[1])
                
              }
              
            }
          )
      )
    
    #Ensure that keep variables are left-most
    if(length(keep) > 0) {
      
      results <-
        results %>%
        dplyr::select(
          tidyselect::one_of(
            keep
          ),
          tidyselect::everything()
        )
      
    }
    
    #Extract keys and return list if requested
    if(extract_keys_as_header & length(send) > 1) {
      
      #Remove header if requested
      if(!keep_keys_in_header) {
        
        .result <-
          results %>%
          dplyr::rename_at(
            dplyr::vars(
              -tidyselect::one_of(keep)
            ),
            stringr::str_remove,
            pattern = stringr::str_c("[", .sep, "].+")
          )
        
      } else {
        
        .result <- results
        
      }
      
      list(
        
        #Extract header created from the keys
        .header =
          dplyr::if_else(
            names(results) %in% keep, " ", stringr::str_extract(names(results), stringr::str_c("[", .sep, "].+")) %>% stringr::str_sub(2)
          ),
        
        #Remove the key values from the result
        .result = .result
        
      )
      
    } else {
      
      results
      
    }
  }

#Name: dish
#Description: Dish out a function to parts of a data frame
dish <-
  function(
    data, #Any data frame
    f, #First argument is left, second argument is right
    left = NULL, #Variables to be included in the left side of the function
    right = NULL, #Variables to be included in the right side of the function
    each_left = TRUE, #Should each left variable be evaluated individually?
    each_right = TRUE, #Should each right variable be evaluated individually?
    bind = FALSE, #Should results be binded together?
    ...
  ) {
    
    #Extract variables
    left <-
      tidyselect::vars_select(
        names(data),
        left
      )
    right <-
      tidyselect::vars_select(
        names(data),
        right
      )
    
    #Remove overlapping variables
    if(any(right %in% left)) {
      
      right <- right[!(right %in% left)]
      
    }
    
    #Create data sets
    if(length(left) == 0) {
      
      if(length(right) == 0) {
        
        #Select everything for both sides
        left <-
          data %>%
          dplyr::select(tidyselect::everything())
        right <- left
        
      } else {
        
        #Left is everything not supplied in right
        left <- 
          data %>%
          dplyr::select(-right)
        right <-
          data %>%
          dplyr::select(right)
        
      }
      
    } else {
      
      if(length(right) == 0) {
        
        #Right is everything not in left
        right <-
          data %>%
          dplyr::select(-left)
        left <-
          data %>%
          dplyr::select(left)
        
      } else {
        
        #Select left variables
        left <-
          data %>%
          dplyr::select(left)
        
        #Select right variables
        right <-
          data %>%
          dplyr::select(right)
        
      }
      
    }
    
    if(each_left) {
      
      if(each_right) {
        
        #Apply function to each left hand variable and each right hand variable
        left_eval <-
          left %>%
          purrr::map(
            function(.x) {
              
              right_eval <-
                right %>%
                purrr::map(
                  function(.y) f(.x, .y, ...)
                )
              
              #Check for binding
              if(bind) {
                right_eval %>%
                  dplyr::bind_rows(
                    .id = ".right"
                  )
              } else {
                
                right_eval
                
              }
              
            }
          )
        
        #Check for binding
        if(bind) {
          
          left_eval %>%
            dplyr::bind_rows(
              .id = ".left"
            )
          
        } else {
          
          left_eval
          
        }
        
      } else {
        
        #Apply function to each left hand variable but full right hand set
        left_eval <-
          left %>%
          purrr::map(
            function(.x) f(.x, right,...)
          )
        
        #Check for binding
        if(bind) {
          
          left_eval %>%
            dplyr::bind_rows(
              .id = ".left"
            )
          
        } else {
          
          left_eval
          
        }
        
      }
      
    } else {
      
      if(each_right) {
        
        #Apply function to each right hand variable but full left hand side
        right_eval <- 
          right %>%
          purrr::map(
            function(.x) f(left, .x,...)
          )
        
        #Check for binding
        if(bind) {
          
          right_eval %>%
            dplyr::bind_rows(
              .id = ".right"
            )
          
        } else {
          
          right_eval
          
        }
        
      } else {
        
        #Give warning
        if(bind)
          warning("Binding has no effect when 'each_left' and 'each_right' are FALSE")
        
        #Apply function to full left hand side and full right hand side
        f(left, right, ...)
        
      }
      
    }
    
  }

#Name: absorb
#Description: Populate a custom text template with the values in a set of key-value pairs
absorb <-
  function(
    key, #Variable with the keys
    value, #Variable with the values
    text, #Custom text template with explicit keys where values should be absorbed into
    sep = "|", #Values will be separated by this in the result if there are duplicate keys
    print = FALSE, #Should the recursion process be displayed?
    evaluate = FALSE #Should the resulting string be evaluated as an R expression?
  ) {
    
    #Check that inputs are the same length
    if(length(key) != length(value))
      stop("keys and values must have the same length")
    
    #Coerce the key to character
    key <- as.character(key)
    
    #Map over each element of text
    filled_text <-
      text %>%
      
      purrr::map_chr(
        function(.text_i) {
          
          #Print original text if desired
          if(print) 
            print(.text_i)
          
          #Send key to recursion process
          key %>%
            purrr::reduce(
              function(.x, .y) {
                
                #Fill value for this iteration
                .text_ij <- stringr::str_replace_all(.x, .y, stringr::str_c(value[key == .y], collapse = sep))
                
                #Print intermediate result if desired
                if(print)
                  print(.text_ij)
                
                
                .text_ij
                
              },
              .init = .text_i
            )
          
        }
      )
    
    #Evaluate if requested
    if(evaluate) {
      
      filled_text %>%
        purrr::map(
          ~
            eval(
              parse(
                text = .x
              )
            )
        )
      
    } else {
      
      filled_text
      
    }
  }

#Name: type_match
#Description: Utility function to use in select columns
type_match <-
  function(
    object,
    types,
    negated = FALSE
  ) {
    
    #Apply to columns that DONT match
    if(negated) {
      
      all(
        types %>%
          purrr::map_lgl(
            ~!methods::is(object, .x)
          )
      )
      
      #Apply to columns that do match        
    } else {
      
      any(
        types %>%
          purrr::map_lgl(
            ~methods::is(object, .x)
          )
      )
      
    }
    
  }

#Name: typly
#Description: Apply a function for specified types
typly <-
  function(
    data, #Any data set
    types, #Character vector of data types to apply functions
    f, #Function to apply
    negated = FALSE, #Should the function be applied to variables not matching any types?
    keep = FALSE, #Should non-matching columns be kept in result?
    ... #Additional arguments passed to f
  ) {
    
    #Filter columns if needed
    if(!keep) {
      
      data[data %>% purrr::map_lgl(type_match, types = types, negated = negated)] %>%
        
        purrr::map(
          #Apply function
          function(.x) {
            
            #If argument is a function then evaluate
            if(methods::is(f, "function")) {
              
              f(.x, ...)
              
              #If list of functions are provided, evaluate each        
            } else {
              
              f %>%
                purrr::map(
                  function(.y) .y(.x, ...)
                )
              
            }
            
          }
        )
      
    } else {
      
      data %>%
        
        #Only apply function if condition holds
        purrr::map_if(
          function(.x) type_match(.x, types = types, negated = negated),
          
          #Apply function
          function(.x) {
            
            #If argument is a function then evaluate
            if(methods::is(f, "function")) {
              
              f(.x, ...)
              
              #If list of functions are provided, evaluate each        
            } else {
              
              f %>%
                purrr::map(
                  function(.y) .y(.x, ...)
                )
              
            }
            
          }
        )
    }
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
