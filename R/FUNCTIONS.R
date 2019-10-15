#Created: 2019-10-14
#Author: Alex Zajichek
#Package: cheese
#Description: Function definitions used internally and available externally

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
          dplyr::tbl_vars(data),
          !!!quos
        ) %>%
        as.list() %>%
        unname()
      
      #Return error if no split variables entered
      if(dplyr::near(length(selected_vars), 0))
        stop("No columns registered.")
      
    }
    
    #Check that depth is numeric
    if(!methods::is(depth, "numeric"))
      stop("Depth must be numeric")
    
    #If depth is negative, adjust from max depth
    if(depth < 0) {
      
      #Use zero if it goes beyond
      depth <- max(length(selected_vars) + depth, 0)
      
    } else {
      
      #Use maximum depth as the limit
      depth <- min(depth, length(selected_vars))
      
    }
    
    #Round to nearest integer (in case decimal provided)
    depth <- round(depth)
    
    #Return data if depth is 0
    if(dplyr::near(depth, 0))
      return(data %>% tibble::as_tibble())
    
    #Pull elements past position 'depth' into position 'depth'
    selected_vars[[depth]] <- 
      selected_vars[seq(depth, length(selected_vars))] %>% 
      purrr::flatten_chr()
    
    #Only keep up to desired depth
    selected_vars <- selected_vars[seq_len(depth)]
    
    #Set initial list to start recursion
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
    
    #Recursively split to desired depth if needed
    if(depth > 1) {
      
      data <-
        purrr::reduce2(
          .x = selected_vars[-1],
          .y = 2:depth,
          .f =
            function(
              .result, #Current data frame
              .split_vars, #Variables to split at this depth
              .depth #Depth to split at
            ) {
              
              .result %>%
                
                #Split at leaf data frame
                purrr::modify_depth(
                  .depth = .depth - 1,
                  ~
                    .x %>%
                    split(
                      dplyr::select(., tidyselect::one_of(.split_vars)),
                      drop = drop,
                      sep = sep
                    )
                )
            },
          .init = data
        )
      
    }
    
    #Remove all split variables in necessary
    if(remove) {
      
      data <-
        data %>%
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
    bare = FALSE, #Only continue on bare lists
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
        
        ifelse(
          
          #Check elements that are lists (including data.frames)
          are_lists,
          
          #Concatenate with recursive function call for those elements
          result %>%
            stringr::str_c(
              list[are_lists] %>% 
                purrr::map_chr(
                  .f = depths, 
                  predicate = predicate,
                  bare = bare,
                  ...
                )
            ),
          
          #Otherwise just return index
          result
          
        ) 
      
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
