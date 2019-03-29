#' Add rows of the parent dataframe
#'
#' @param loop a dataframe containing the loops
#' @param parent a dataframe containing the parent informations
#' @param variables.to.keep optional: a string or a vector of string containing the parent columns names that need to be copy in the loop dataset
#' @param uuid.name.loop optional: a string that specify the name of the loop column containing the uuids. If not specify, searches for column containing the uuid string
#' @param uuid.name.parent optional: a string that specify the name of the parent column containing the uuids. If not specify, searches for column containing uuid string
#' @details Searches for each row in loop dataframe if it matches with a row of the parent datarame. Select in the row of the parent dataframe, the information that the user wants to add to the loop dataframe
#' @return the loop dataframe to which for each row has been added the corresponding value of the parent dataframe variable
#' @examples
#' parent <- data.frame(uuid=1:10, age=sample(10,30,60),gender=sample(c("F","M"),10,replace = T) )
#' child <- data.frame (parent_uuid=sample(1:10,20,replace = T), age=sample(20,1,18)  gender=sample(c("F","M"),20,replace = T))
#' family<- add_parent_to_loop(child,parent,c("age"))
#' @export
#'
add_parent_to_loop <- function(loop, parent , variables.to.keep=NULL , uuid.name.loop=NULL , uuid.name.parent=NULL)
{
  # warning on inputs
  if (is.data.frame(loop) == FALSE)stop("loop parameter has to be a dataframe")
  if (is.data.frame(loop) == FALSE)stop("parent parameter has to be a dataframe")
  if (is.vector(variables.to.keep) == FALSE) {
    if (is.null(variables.to.keep) == FALSE)stop("variables.to.keep parameter has to be a vector of string or NULL") }

  #find uuid columns
  if (is.null(uuid.name.parent) == TRUE) {
    uuid.name.parent <- grep("uuid", names(parent), value = T,ignore.case = T)
    if (length(uuid.name.parent) == 0) {
      stop("Could not find the uuid automatically in the parent dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.parent) > 1) { uuid.name.parent <- uuid.name.parent[1] }
  }

  if (is.null(uuid.name.loop) == TRUE) {
    uuid.name.loop <- grep("uuid", names(loop), value = T,ignore.case = T)
    if (length(uuid.name.loop) == 0) {
      stop("Could not find the uuid automatically in the loop dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.loop) > 1) { uuid.name.loop <- uuid.name.parent[1] }
  }

  #if uuid specified but does not exist
  if (is.null(uuid.name.parent) == FALSE) {
    if (!(uuid.name.parent %in% names(parent))) {
      stop("Uuid name given does not exist in the parent dataset. Please provide the name of the uuid column as a parameter")
    }
  }

  if (is.null(uuid.name.loop) == FALSE) {
    if (!(uuid.name.loop %in% names(loop))) {
      stop("Uuid name given does not exist in the loop dataset. Please provide the name of the uuid column as a parameter")
    }
  }

  # find parent row for each row in loop:
  index_of_loop_in_parent<-match(loop[ ,uuid.name.loop],parent[ ,uuid.name.parent])

  if (all(is.na(index_of_loop_in_parent))) {
    stop("Could not find parent row corresponding to a loop row")
  }

  # select those rows:
  parent_reorganised_by_loop_rows <- parent[index_of_loop_in_parent, ,drop=F]

  # Different cases for variables to keep

  # Case 1: Variables to keep are not specified (NULL). Copy all columns of parent except uuid column
  if (is.null(variables.to.keep)) {
    parent_variables <- names(parent)
    variables.to.keep=parent_variables[parent_variables != "uuid"]
  }

  # Case 2: Variables doesn't exist in parent dataframe
  if (!all((variables.to.keep %in% names(parent)))) {

    variables_Not_Exist = variables.to.keep[which((variables.to.keep %in% names(parent)) == FALSE)]
    variables_Not_Exist_Error_Message = paste0(variables_Not_Exist, collapse = ", ")
    variables_Not_Exist_Error_Message = paste("Variables not found in parent dataframe: ", variables_Not_Exist_Error_Message)
    stop(variables_Not_Exist_Error_Message)

  }

  # subset selected columns:
  loop_with_parent_variables <- data.frame(loop,parent_reorganised_by_loop_rows[ ,variables.to.keep,drop=F],stringsAsFactors = F)
  rownames(loop_with_parent_variables) <- rownames(loop)
  return(loop_with_parent_variables)
}


#' Add rows of the parent dataframe
#'
#' @param loop a dataframe containing the loops
#' @param parent a dataframe containing the parent informations
#' @param variable.to.add a name character vector. This vector contains the exact variable(s) name(s) of the loop dataframe that the user wants to aggregate.
#' If there are multiple variables, the variables should be aggregate with the same function. The names of the variables if specified are going to be the name of the parent column.
#' @param aggregate.function function specify by the user to aggregate the variable specified in variable.to.add. This function should take a vector as a parameter and return a single output.
#' @param uuid.name.loop optional: a string that specify the name of the loop column containing the uuids. If not specify, searches for column containing uuid string.
#' @param uuid.name.parent optional: a string that specify the name of the parent column containing the uuids. If not specify, searches for column containing uuid string.
#' @details Add to the parent dataframe, column(s) that is(are) the result of the aggregation made on the loop dataframe. This aggregation is defined by the function aggregate.function.
#' @return the parent dataframe to which column(s) has been added. The column(s) contains for each parent the result of the aggregation on the loop rows that correspond to the same parent.
#' @examples

#' parent <- data.frame(uuid=1:10, age_parent=sample(10,30,60),gender=sample(c("F","M"),10,replace = T) )
#' child <- data.frame (parent_uuid=sample(1:10,20,replace = T), age_child=sample(20,1,18)  gender=sample(c("F","M"),20,replace = T))
#' family <- affect_loop_to_parent(child, parent,aggregate.function = sum , variable.to.add = c(sum_of_child_age="age_child"))

#' @export
#'
affect_loop_to_parent <- function( loop , parent , aggregate.function, variable.to.add, uuid.name.loop=NULL,uuid.name.parent=NULL)
{
  # warning on inputs
  if (is.data.frame(loop) == FALSE)stop("loop parameter has to be a dataframe")
  if (is.data.frame(loop) == FALSE)stop("parent parameter has to be a dataframe")
  if (is.null(variable.to.add)==TRUE)stop("Please provide the name of a the loop column you want to add to the parent dataset.")

  #find uuid columns
  if (is.null(uuid.name.parent) == TRUE) {
    uuid.name.parent <- grep("uuid", names(parent), value = T,ignore.case = T)
    if (length(uuid.name.parent) == 0) {
      stop("Could not find the uuid automatically in the parent dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.parent) > 1) { uuid.name.parent <- uuid.name.parent[1] }
  }

  if (is.null(uuid.name.loop) == TRUE) {
    uuid.name.loop <- grep("uuid", names(loop), value = T,ignore.case = T)
    if (length(uuid.name.loop) == 0) {
      stop("Could not find the uuid automatically in the loop dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.loop) > 1) { uuid.name.loop <- uuid.name.parent[1] }
  }

  #if uuid specified but does not exist
  if (is.null(uuid.name.parent) == FALSE) {
    if (!(uuid.name.parent %in% names(parent))) {
      stop("Uuid name given does not exist in the parent dataset. Please provide the name of the uuid column as a parameter")
    }
  }

  if (is.null(uuid.name.loop) == FALSE) {
    if (!(uuid.name.loop %in% names(loop))) {
      stop("Uuid name given does not exist in the loop dataset. Please provide the name of the uuid column as a parameter")
    }
  }

  #if none loop uuid correspond to a parent uuid
  index_of_loop_in_parent<-match(loop[ ,uuid.name.loop],parent[ ,uuid.name.parent])
  if (all(is.na(index_of_loop_in_parent))) {
    stop("Could not find parent row corresponding to a loop row")
  }

  #if the variable(s) to add don't exist
  if (!all((variable.to.add %in% names(loop)))) {
    variables_Not_Exist = variable.to.add[which((variable.to.add %in% names(loop)) == FALSE)]
    variables_Not_Exist_Error_Message = paste0(variables_Not_Exist, collapse = ", ")
    variables_Not_Exist_Error_Message = paste("Variables not found in loop dataframe: ", variables_Not_Exist_Error_Message)
    stop(variables_Not_Exist_Error_Message)
  }

  #creates a list of dataframe. Each element of the list is a dataframe containing the loop rows corresponding to the same parent uuid
  split_pivot <- loop[[uuid.name.loop]]
  split_loop <- split(loop, split_pivot)

  #For each variable to add, aggregate loop elements and add the result in a new column of the parent dataframe
  new_parent <- parent
  for(i in 1:length(variable.to.add)){

    # For each dataframe of the list, aggregate variable.to.add by using the function choosen by the user
    result_aggregation <- lapply( lapply(split_loop, function(x) x[[variable.to.add[i]]]), aggregate.function)

    #Rename the column containing the results of the aggregation
    # if name is not specified
    if(length(names(variable.to.add[i])) == 0 || names(variable.to.add[i]) == "" ){
      new_variable_name <- paste0("Aggregation_Result_",variable.to.add[i])

      while(new_variable_name %in% names(new_parent)){ #in order to have a unique column name
        new_variable_name <- paste0(new_variable_name,"X")
      }
      warning_msg <- paste("You have not specified a column name for the variable:",variable.to.add[i])
      warning_msg <- paste(warning_msg, "It has been renamed:")
      warning_msg <- paste(warning_msg, new_variable_name )
      warning(warning_msg)
    }
    else{
      if(names(variable.to.add[i]) %in% names(new_parent)){
        stop("Please choose another variable name. This column name already exists in the parent dataframe")
      }
      else{
        new_variable_name <- names(variable.to.add[i])
      }
    }
    new_parent[[variable.to.add[i]]]<-NA #NA for the parent that doesn't correspond to any loop rows
    uuid_into_parent=which((parent$uuid %in% names(result_aggregation))==TRUE)
    new_parent[[variable.to.add[i]]][uuid_into_parent] <- result_aggregation
    names(new_parent)[length(new_parent)] <- new_variable_name
  }

  return(new_parent)
}


