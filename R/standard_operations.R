#' Add rows of the parent dataframe
#'
#' @param loop a dataframe containing the loops
#' @param parent a dataframe containing the parent informations
#' @param variables.to.keep optional: a string or a vector of string containing the parent columns names that need to be copy in the loop dataset
#' @param uuid.name.loop optional: Specify the loop column containing the uuids. If not specify, searches for column containing uuid string
#' @param uuid.name.parent optional: Specify the parent column containing the uuids. If not specify, searches for column containing uuid string
#' @details Searches for parent row for each row in loop, select the rows of the parent dataframe and add them to the loop dataframe
#' @return loop dataframe with the chosen parent variables in addition
#' @examples
#' parent <- data.frame(uuid=1:10, age=sample(10,30,60),gender=sample(c("F","M"),10,replace = T) )
#' child <- data.frame (parent_uuid=sample(1:10,20,replace = T), age=sample(20,1,18)  gender=sample(c("F","M"),20,replace = T))
#' family<- add_parent_to_loop(child,parent,c("age"))
#' equivalente to
#' family<- add_parent_to_loop(child,parent,"parent_uuid","uuid",c("age"))
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
#' @param variables.to.add variable(s) to aggregate
#' @param aggregate.function function specify by the user to aggregate one or several variables
#' @param uuid.name.loop optional: Specify the loop column containing the uuids. If not specify, searches for column containing uuid string
#' @param uuid.name.parent optional: Specify the parent column containing the uuids. If not specify, searches for column containing uuid string
#' @details Add to child information to the corresponding parent by aggregateing with a function specify by the user
#' @return Parent dataframe with the results of the aggregation
#' @examples
#' parent <- data.frame(uuid=1:10, age=sample(10,30,60),gender=sample(c("F","M"),10,replace = T) )
#' child <- data.frame (parent_uuid=sample(1:10,20,replace = T), age=sample(20,1,18), gender=sample(c("F","M"),20,replace = T))
#' aggregate.function <- function(x, variable.to.add){
#'    result_aggregation <- sum(x[[variable.to.add]])
#'    return(result_aggregation)
#' }
#' family <- affect_loop_to_parent(child, parent, "age",aggregate.function)
#' @export
#'
affect_loop_to_parent <- function( loop , parent , aggregate.function, variable.to.add, uuid.name.loop=NULL,uuid.name.parent=NULL)
{
  # warning on inputs
  if (is.data.frame(loop) == FALSE)stop("loop parameter has to be a dataframe")
  if (is.data.frame(loop) == FALSE)stop("parent parameter has to be a dataframe")
  if (is.null(variable.to.add)==TRUE)stop("Please provide the name of a the loop column you want to add to the parent dataset.")
  #if (is.vector(variable.to.add) == FALSE) {
  # if (lenght(variable.to.add) >1)stop("variables.to.add parameter has to be a vector of one element") }

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

  index_of_loop_in_parent<-match(loop[ ,uuid.name.loop],parent[ ,uuid.name.parent])
  if (all(is.na(index_of_loop_in_parent))) {
    stop("Could not find parent row corresponding to a loop row")
  }

  split_pivot <- loop[[uuid.name.loop]] # attention si plusieurs colonne avec m?me mot
  split_loop <- split(loop, split_pivot)

  if (!all((variable.to.add %in% names(loop)))) {

    variables_Not_Exist = variable.to.add[which((variable.to.add %in% names(loop)) == FALSE)]
    variables_Not_Exist_Error_Message = paste0(variables_Not_Exist, collapse = ", ")
    variables_Not_Exist_Error_Message = paste("Variables not found in loop dataframe: ", variables_Not_Exist_Error_Message)
    stop(variables_Not_Exist_Error_Message)

  }

  new_parent <- parent

  for(i in 1:length(variable.to.add)){

    result_aggregation <- lapply(split_loop, aggregate.function, variable.to.add[i])
    new_parent[[variable.to.add[i]]]<-NA
    uuid_into_parent=which((parent$uuid %in% names(result_aggregation))==TRUE)
    new_parent[[variable.to.add[i]]][uuid_into_parent] <- result_aggregation
  }

  return(new_parent)
}



