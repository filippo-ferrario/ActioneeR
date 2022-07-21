# ===============================================================================
# Name   	: 
# Author 	: 
# Date   	:  [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: 
# ===============================================================================



#' Find maximum allowed spacing  
#'
#' Find maximum allowed spacing between cameras given distance to the plane of interest and the desired minimum percentage of overalp in swath between two adjacent cameras. 
#'
#' @inheritParams overlap_pc_cam
#' @param min_camera_overlap desired miminum overalp between two adjacent cameras on the rig
#'
#' @return
#'
#' A matrix with named dimensions: columns are minimum overlap, rows are distances from the plane of interest. Attention: remembere that dimnames are characters, if used to subset.
#'
#' @examples
#' 
#' find_max_spacing(fov_L=1.42 , min_camera_overlap= seq(40, 60, 10),dist=seq(70, 100, 10))
#' 
#' 
#' @seealso
#' 
#' [imaging_strategy], [swath_rig], [FOV], [WID], [overlap_pc_cam]
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export






find_max_spacing<-function(fov_L,fov_R=fov_L, min_camera_overlap, dist)   {
      # check args
      if (!is.numeric(fov_R)) stop ('fov_R must be numeric expressing FOV in RADIANTS')
      if (!is.numeric(fov_L)) stop ('fov_L must be numeric expressing FOV in RADIANTS')
      if (!is.numeric(dist)) stop ('dist must be numeric')
      if (!is.numeric(min_camera_overlap)) stop ('cam_spacing must be numeric')

      over<-matrix(nrow=length(dist),ncol=length(min_camera_overlap), dimnames=list(dist=dist,min_camera_overlap=min_camera_overlap))
      cR<-  WID(fov=fov_R, d=dist)
      cL<-  WID(fov=fov_L, d=dist)
      pair<-cbind(cR,cL)
      dx<-(cR/2)       # half width from right camera
      sx<-(cL/2)       # half width from left camera
      im_sum<-dx+sx
      
      for (i in 1:length(min_camera_overlap)){
      		  min_ov_cm<-apply(pair*min_camera_overlap[i]/100, min, MARGIN=1) 
              cam_spacing<-im_sum-min_ov_cm
              over[,i]<-cam_spacing
      }
      over

   }


# # bench
# # ==========


# fov_L=1.42 
# fov_R=fov_L 
# min_camera_overlap= seq(40, 60, 10)
# dist=seq(70, 100, 10)

find_max_spacing(fov_L=1.42 , min_camera_overlap= seq(40, 60, 10),dist=seq(70, 100, 10))