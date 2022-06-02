#===============================================================================
# Name   : imagery file rename
# Author : Filippo Ferrario
# Date   : 09/12/2020 
# Version: 0.1
# Aim	 : rename the image or video files acquired in mapping projects.
#          	The files are usually taken with different cameras and saved in a sub folder named as the camera ID (as unique as possible) whitin a folder of the transect.

#===============================================================================



#' @param imagery_path character. Path to the folder containing imagery data. Ideally this folder contains one subfolder for each transect to be mosaiced.
#' @param trs_ignore character vector of names of folders (or files) within the subfolder of a transect which do not contain imagery data and should therefore be ignored. Ideally these elements are constant for each transect.
#' @param img_format character vector specifying in which formats the imagery files could be provided. List all formats that are being used in the project.
#' 


imgRename<-function(imagery_path=NULL, trs_ignore=c('fit'), img_format=c('png','mp4','jpg') )
{
	# check params
	if (!is.character(imagery_path)) stop('enter the path to the folder containing the imagery')

	# retrive names of the transect subfolders
	trs_names<-list.dirs(imagery_path,recursive=F, full.names=F)
	


	for (i in 1: length(trs_names)) {
		# get the name of the transet
		trs_tag<-trs_names[i]
		# get the names of the cameras within a transect excluding the trs_ignore folder/files
		cam_names<-list.dirs(paste0(imagery_path,'/',trs_tag),recursive=F, full.names=F)
		cam_names<-cam_names[!cam_names %in% trs_ignore]
		for (k in 1:length(cam_names)) {
			cam_tag<-cam_names[k]
			tmp_path<-paste0(imagery_path,'/',trs_tag,'/',cam_tag)
			# get names of the files in the subfolder
			img_names<-list.files(tmp_path,recursive=F, full.names=F )
			# select only accepted imagery formats
			form_patt<-paste0('//.',tolower(img_format), collapse='|')
			img_names_indx<- grepl(tolower(img_names), pattern=form_patt)
			img_names<-img_names[img_names_indx]

			name_tag<-paste0(trs_tag,'-',cam_tag,'-',img_names)

			file.rename(from=paste0(tmp_path,'/',img_names), to=paste0(tmp_path,'/',name_tag))
		}

	}



}


# bench
# ---------

# imagery_path<-'F:/PPO_godbout/img_test'
# 
# imgRename(imagery_path='F:/PPO_godbout/imagery')

imgRename(imagery_path='C:/Users/Utilisateur/Desktop/prova',img_format='txt')


