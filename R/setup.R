# ===============================================================================
# Name      : Wing settings 
# Author    : Filippo Ferrario
# Date      : 13-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : 
# ===============================================================================


#' 
#'
#' 
#'
#' @param 
#' @param 
#' @param 
#' @param 
#'
#' @return
#'
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' 
#'


setup<-function(matrix, cond_1=NULL, cond_2=NULL){

cond<-expression(paste(matrix, cond_1))

pairs<-which(sw,arr.ind=TRUE)
data.frame(dist=dimnames(sw)$dist[pairs[,1]],cam_spacing=dimnames(sw)$cam_spacing[pairs[,2]])

}

matrix<-sw
cond_1<-'<150'