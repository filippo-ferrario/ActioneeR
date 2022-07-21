# ===============================================================================
# Name      : Overalp estimation Functions 
# Author    : Filippo Ferrario
# Date      : 13-07-2022 [dd-mm-yyyy]
# Version   : 0.1
# URL    : 
# Aim       : Estimate the percentage of overalp in swathe between two adjacent cameras. 
# ===============================================================================


#' Overalp estimation Functions 
#'
#' Estimate the percentage of overalp in swath between two adjacent cameras. 
#'
#'
#' @param fov_L a numeric value/vector expressing FOV in RADIANTS. Either the FOV of all the cameras, or the left camera in a pair if the right one has a different FOV. In this case specify FOV_R.
#' @param fov_R a numeric value/vector expressing FOV in RADIANTS. Optional, if not specified it is assument to be equal to fov_L.
#' @param cam_spacing a numeric value/vector expressing the spacing between two cameras
#' @param dist  a numeric value/vector expressing the distance between the camera and the plane of interest (e.g., the bottom, the ground)
#'
#' @return
#'
#' A matrix of estimate the percentage of overalp (%) with named dimensions: columns are camera spacings, rows are distances from the plane of interest. Attention: remembere that dimnames are characters, if used to subset.
#' 
#' @seealso
#' 
#' [imaging_strategy], [swath_rig], [FOV], [WID]
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export



overlap_pc_cam<-function(fov_L,fov_R=fov_L,dist,cam_spacing)   {
      # check args
      if (!is.numeric(fov_R)) stop ('fov_R must be numeric expressing FOV in RADIANTS')
      if (!is.numeric(fov_L)) stop ('fov_L must be numeric expressing FOV in RADIANTS')
      if (!is.numeric(dist)) stop ('dist must be numeric')
      if (!is.numeric(cam_spacing)) stop ('cam_spacing must be numeric')

      over<-matrix(nrow=length(dist),ncol=length(cam_spacing), dimnames=list(dist=dist,cam_spacing=cam_spacing))
      cR<-  WID(fov=fov_R, d=dist)
      cL<-  WID(fov=fov_L, d=dist)
      pair<-cbind(cR,cL)
      for (k in 1:length(cam_spacing)){
              dx<-(cR/2)       # half width from right camera
              sx<-(cL/2)       # half width from left camera
              im_sum<-dx+sx
              I1<-im_sum<=cam_spacing[k]
              #I2<-im_sum>cam_spacing[i]
              ov<- dx-(cam_spacing[k]-sx)
              OV<-ov
              OV[I1]<-0

              # calculate percent
              max_ov<-apply(OV/pair, max,MARGIN=1)
              over[,k]<-max_ov*100
      }
      over

   }

# bench
# ----------   
# overlap_pc_cam( 1.352524, 1.352524,dist=50, cam_spacing=10)

