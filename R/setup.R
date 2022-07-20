# ===============================================================================
# Name      : Rig settings 
# Author    : Filippo Ferrario
# Date      : 20-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : 
# ===============================================================================


#' Filtering a swath matrix
#'
#' This function help visualising which combination of _distance from the bottom_ and _camera spacing_ provide a desired range of swaths
#'
#'
#'
#'
#'
#' @param matrix a matrix produced by [swath_rig].
#' @param cond a string specifying the filtering conditions. Important: the string should contain the name of the object passed to `matrix`
#'
#' @return
#'
#' A named list containing the filtered matrix (i.e., only showing the result meeting the criteria) and a dataframe of the indices (i.e., distance from the bottom and camera spacing) associated to the selected criteria. 	
#'
#' @examples
#'
#' setup_filter(sw, cond='sw>150 & sw<200')
#'
#'
#'
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @seealso
#' 
#' [swath_rig], [overlap_pc_cam], [imaging_strategy]
#' 	
#' 
#' @export



setup_filter<-function(matrix, cond){

	out<-matrix
	ii<-which(eval(parse(text=cond)))
	out[-ii]<-NA
	attr(out,which='between camera overlap')[-ii]<-NA


	pairs<-which(eval(parse(text=cond)), arr.ind=TRUE)

	dtf<-data.frame(dist=dimnames(matrix)$dist[pairs[,1]],cam_spacing=dimnames(matrix)$cam_spacing[pairs[,2]])

	res<-list(out, dtf)
	names(res)<-c('matrix', 'matrix indices')

	return(res)
}









