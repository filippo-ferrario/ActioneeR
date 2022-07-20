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
#' @param matrix a matrix produced by [swath_rig].
#' @param cond a string specifying the filtering conditions. Important: the string should refer to "matrix" AND NOT to the name of the object passed to `matrix`
#' @param filter_on_swath should the matrix be filtered on the swath values (TRUE, the default), or the between camera overlap (FALSE)?
#'
#' @return
#'
#' A named list containing the filtered matrix (i.e., only showing the result meeting the criteria) and a dataframe of the indices (i.e., distance from the bottom and camera spacing) associated to the selected criteria. 	
#'
#' @examples
#'
#' sw<-swath_rig( 1.352524, dist=seq(50,100, by=10), cam_spacing=seq(10,100, by=10), n_cams=3)
#' setup_filter(sw, cond='matrix>150 & matrix<200', filter_on_swath=TRUE)
#' setup_filter(sw, cond='matrix>0.3 ', filter_on_swath=FALSE)
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


setup_filter<-function(matrix, cond, filter_on_swath=TRUE ){

	out<-matrix
	cam_overlap<-attr(out,which='cam_overlap')
	if (filter_on_swath==FALSE){
		cond<-gsub(cond, pattern='matrix', replacement='cam_overlap')
	}
	ii<-which(eval(parse(text=cond)))
	out[-ii]<-NA
	attr(out,which='cam_overlap')[-ii]<-NA


	pairs<-which(eval(parse(text=cond)), arr.ind=TRUE)

	dtf<-data.frame(dist=dimnames(matrix)$dist[pairs[,1]],cam_spacing=dimnames(matrix)$cam_spacing[pairs[,2]])

	res<-list(out, dtf)
	names(res)<-c('matrix', 'matrix indices')

	return(res)
}






 # sw<-swath_rig( 1.352524, dist=seq(50,100, by=10), cam_spacing=seq(10,100, by=10), n_cams=3)
 # setup_filter(sw, cond='matrix>150 & matrix<200', filter_on_swath=TRUE)
 # setup_filter(sw, cond='matrix>0.3 ', filter_on_swath=FALSE)
