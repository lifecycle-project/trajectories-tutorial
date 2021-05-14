ds.dataFrame <- function(x=NULL, row.names=NULL, check.rows=FALSE, check.names=TRUE, stringsAsFactors=TRUE, completeCases=FALSE, DataSHIELD.checks=FALSE, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  if(is.null(x)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dataframe.newobj"
  }
  # the input variable might be given as column table (i.e. D$vector)
  # or just as a vector not attached to a table (i.e. vector)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  if(DataSHIELD.checks){
    # check if the input object(s) is(are) defined in all the studies
    for(i in 1:length(varnames)){
      if(is.na(obj2lookfor[i])){
        defined <- isDefined(datasources, varnames[i])
      }else{
        defined <- isDefined(datasources, obj2lookfor[i])
      }
    }
    # call the internal function that checks the input object(s) is(are) of the same legal class in all studies.
    for(i in 1:length(x)){
      typ <- checkClass(datasources, x[i])
      if(!('data.frame' %in% typ) & !('matrix' %in% typ) & !('factor' %in% typ) & !('character' %in% typ) & !('numeric' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ)){
        stop("Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
      }
    }
    # check that there are no duplicated column names in the input components
    for(j in 1:length(datasources)){
      colNames <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(typ %in% c('data.frame', 'matrix')){
          colNames[[i]] <- ds.colnames(x=x[i], datasources=datasources[j])
        }
        if(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical')){
          colNames[[i]] <- as.character(x[i])
        }
        colNames <- unlist(colNames)
        if(anyDuplicated(colNames) != 0){
          cat("\n Warning: Some column names in study", j, "are duplicated and a suffix '.k' will be added to the kth replicate \n")
        }  
      }  
    } 
    # check that the number of rows is the same in all componets to be cbind
    for(j in 1:length(datasources)){
      nrows <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(typ %in% c('data.frame', 'matrix')){
          nrows[[i]] <- ds.dim(x=x[i], type='split', datasources=datasources[j])[[1]][1]
        }
        if(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical')){
          nrows[[i]] <- ds.length(x[i], type='split', datasources=datasources[j])[[1]]
        }
      }
      nrows <- unlist(nrows)
      if(any(nrows != nrows[1])){
        stop("The number of rows is not the same in all of the input components", call.=FALSE)
      }
    }  
  }
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "df_new"
  }
  # CREATE THE VECTOR OF COLUMN NAMES
  colname.list <- list()
  for (std in 1:length(datasources)){  
    colname.vector <- NULL
    class.vector <- NULL
    for(j in 1:length(x)){
      testclass.var <- x[j]
      calltext1 <- call('classDS', testclass.var)
      next.class <- DSI::datashield.aggregate(datasources[std], calltext1)
      class.vector <- c(class.vector, next.class[[1]])
      if (notify.of.progress){
        cat("\n",j," of ", length(x), " elements to combine in step 1 of 2 in study ", std, "\n")
      }  
    }
    for(j in 1:length(x)){
      test.df <- x[j]
      if(class.vector[j]!="data.frame" && class.vector[j]!="matrix"){
        colname.vector <- c(colname.vector, test.df)
        if (notify.of.progress){
          cat("\n",j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }else{
        calltext2 <- call('colnamesDS', test.df)
        df.names <- DSI::datashield.aggregate(datasources[std], calltext2)
        colname.vector <- c(colname.vector, df.names[[1]])
        if (notify.of.progress){
          cat("\n", j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }
    }
    colname.list[[std]] <- colname.vector
  }
  if (notify.of.progress){
    cat("\nBoth steps in all studies completed\n")
  }
  # prepare vectors for transmission
  x.names.transmit <- paste(x, collapse=",")
  colnames.transmit <- list()
  for (std in 1:length(datasources)){
    colnames.transmit[[std]] <- paste(colname.list[[std]], collapse=",")
  }
  if(!is.null(row.names)){
    row.names.transmit <- paste(row.names, collapse=",")
  }
 ###############################
  # call the server side function
  #The serverside function dataFrameDS calls dsBase::dataFrameDS in dsBase repository
  for(std in 1:length(datasources)){
    if(is.null(row.names)){
      cally <- call("dataFrameDS", x.names.transmit, NULL, check.rows, check.names,
                    colnames.transmit[[std]], stringsAsFactors, completeCases)
    }else{
      cally <- call("dataFrameDS", x.names.transmit, row.names.transmit, check.rows, check.names,
                    colnames.transmit[[std]], stringsAsFactors, completeCases)
    }
    DSI::datashield.assign(datasources[std], newobj, cally)
  }
#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
#TRACER																									 	#
#return(test.obj.name)																					 	#
#}                                                                                   					 	#
																											#
																											#
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 	#
																											#
object.info<-DSI::datashield.aggregate(datasources, calltext)												 	#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 	#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS", test.obj.name)															#
    studyside.message<-DSI::datashield.aggregate(datasources, calltext)											#
																											#
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(is.object.created=return.message,validity.check=validity.check))						    #
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################
}
#ds.dataFrame