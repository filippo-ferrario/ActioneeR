# ===============================================================================
# Name      :  
# Author    : Filippo Ferrario
# Date      : 18-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : 
# ===============================================================================


#' Swathe
#'
#' Estimate the swath that will be obtained using a give number of cameras alligned in a rig.
#'
#' @inheritParams ActioneeR::overlap_pc 
#' @param n_cams number of cameras in the setup (all considred having the same properties and being alligned)
#'
#' @details
#'
#' It is supposed that all the cameras have the same FOV, to be specified as `fov_L`.
#' 
#' 
#' @return
#'
#' A matrix storing swath values for each compbination of dimnames "dist" and "cam_spacing". The matrix also has between camera overlap stored as an attribute
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @example
#' swath(fov_L=1.352524, fov_R=1.352524,dist=50, cam_spacing=10, n_cams=3)
#' 
#'
#' sw<-swath( 1.352524, 1.352524,dist=seq(50,100, by=10), cam_spacing=seq(10,100, by=10), n_cams=3)
#' sw
#'
#' @export

swath_rig<-function(fov_L,fov_R=fov_L,dist,cam_spacing, n_cams=NULL)   {   #,name_set

  # check args
  if (!is.numeric(fov_R)) stop ('fov_R must be numeric expressing FOV in RADIANTS')
  if (!is.numeric(fov_L)) stop ('fov_L must be numeric expressing FOV in RADIANTS')
  if (!is.numeric(dist)) stop ('dist must be numeric')
  if (!is.numeric(cam_spacing)) stop ('cam_spacing must be numeric')
  if (!is.numeric(n_cams)) stop ('n_cams must be numeric')

  mat_out<-matrix(nrow=length(dist),ncol=length(cam_spacing), dimnames=list(dist=dist,cam_spacing=cam_spacing))
  swath_mat<-mat_out
   # out_list<-vector('list',length=length(name_set))
   # swath_list<-out_list
   # for (i in 1: length(name_set)){
      L<-  WID(fov=fov_L, d=dist)
      R<-  WID(fov=fov_R, d=dist)

      L05<-L/2
      R05<-R/2
      for (k in 1:length(cam_spacing)){
       inner_space<-cam_spacing[k]*(n_cams-1)
       swath<-L05+R05+inner_space
       swath_mat[,k]<-swath
      }

  overlap<- overlap_pc_cam(fov_L,fov_R,dist,cam_spacing)
  attr(swath_mat,which='between camera overlap')<-overlap      
      # swath_list[[i]]<-swath_mat
      # }
   # names(swath_list)<-name_set
   return(swath_mat)

 }







# # bench
# # ----------   
# swath( 1.352524, 1.352524,dist=50, cam_spacing=10, n_cams=3)

# sw<-swath( 1.352524, 1.352524,dist=seq(50,100, by=10), cam_spacing=seq(10,100, by=10), n_cams=3)

# matrix(rownames(sw)[which(sw>=280,arr.ind=TRUE)],ncol=2)



# pairs<-which(sw>=280,arr.ind=TRUE)
# data.frame(dist=dimnames(sw)$dist[pairs[,1]],cam_spacing=dimnames(sw)$cam_spacing[pairs[,2]])