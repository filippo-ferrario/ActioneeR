#===============================================================================
# Name   : Imagery renaming
# Author : Filippo Ferrario
# Date   : 09/12/2020 
# Version: 0.1
# Aim	 : rename files based on the strucure of the folder tree in which they are organized.
# 		   Originally developed to rename imagery files acquired in mapping projects.
#          The files are usually taken with different cameras and saved in subfolders named as the camera ID (as unique as possible) whitin a folder-tree organized to reflect shooting strategy (e.g., transects, DAY-SHOOTING_SESSION, cameras settings).  
#		   Some common structures include:	
# 			- first level is typically grouping different cameras (e.g., a transect, a DAY-SHOOTING_SESSION)
# 			- second level is typically a folder corrsponding to a camera AND contains only imagery files to be renamed.
#		   
#		   For pictures:
# 		   In case the a camera record the same image in multiple formats (e.g., JPG and RAW), the name of the files corresponding to the same shot will only differ by the format.
#		   In this case, if a sequential number is to be given, numbering must reflect the sequence of images in each format in the same way.
#		   While cameras already use a file naming system that is sequential in some way, adding a specific sequential tag can ease processing and avoid cases when the camera numbering system skips some digits. 
#          For examples GoPros time lapse will use only 4 digits sequential ID, so that when reaching 9999 the next picture is 0001. 
#
#		   For videos:
#		   sequential Id is never added since usually video
# 
# IMPROVEMENTS NEEDED: need to add a check to verify that no nested subfolder are ignored when ignore_nested is NULL
# 
#===============================================================================


#' Rename files based on the strucure of the folder tree in which they are organized.
#' 
#' Originally developed to rename imagery files acquired in mapping projects. 
#' The files are usually taken with different cameras and saved in subfolders named as the camera ID (as unique as possible) whitin a folder-tree organized to reflect shooting strategy (e.g., transects, DAY-SHOOTING_SESSION, cameras settings).  
#'		   Some common structures include:	
#' 			- first level is typically grouping different cameras (e.g., a transect, a DAY-SHOOTING_SESSION)
#' 			- second level is typically a folder corrsponding to a camera AND contains only imagery files to be renamed.
#' 
#' @param imagery_path character. Path to the folder containing imagery data. Ideally this folder contains one subfolder for each transect to be mosaiced.
#' @param ignore_lev1 character vector of names of subfolders at level 1 that should not be processed. to be used in particular avoid problems arising in cases when no folders with more levels than needed are present and some of these should be ignored) 
#' @param ignore_nested character vector of names of subfolders that should be ignored INSIDE the desired level to be processed. Ideally these elements are constant throughout the folder tree and not named as other levels. if "ignore" is NULL, any folder of the desired level which contain a subfolder will be skipped since it will be considered of an undesired level.
#' @param img_format character vector (case insensitive) specifying in which formats the image files could be provided. List all formats that are being used in the project. Any format not specified will not be considered. Formats are case insensitive: formats differing only in case will be treated as the same format (i.e., "jpg" == "JPG")
#' @param vid_format character vector (case insensitive) specifying in which formats the video files could be provided. List all formats that are being used in the project. Any format not specified will not be considered. Formats are case insensitive: formats differing only in case will be treated as the same format (i.e., "jpg" == "JPG")
#' @param add_seq logical. If TRUE (default) a sequential number is added at the end of the name, before the format extension, for each format type. 
#' @param n_lev numeric. Number specifying the number of levels (branch) of the path that need to be processed. Only paths with the specified number of levels will be processed.
#' 
#' @details
#' - For pictures:
#' 		   In case the a camera record the same image in multiple formats (e.g., JPG and RAW), the name of the files corresponding to the same shot will only differ by the format.
#'		   In this case, if a sequential number is to be given, numbering must reflect the sequence of images in each format in the same way.
#'		   While cameras already use a file naming system that is sequential in some way, adding a specific sequential tag can ease processing and avoid cases when the camera numbering system skips some digits. 
#'          For examples GoPros time lapse will use only 4 digits sequential ID, so that when reaching 9999 the next picture is 0001. 
#' - For videos:
#'		   sequential Id is never added since usually video
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' 
#' 
#' @export 


imgRename<-function(imagery_path=NULL, n_lev=NULL, ignore_lev1=NULL,ignore_nested=c('fit'), img_format=c('png','jpg'), vid_format=c('mp4') , add_seq=T)
{ #browser()
	# check params
	if (is.null(n_lev) | !is.numeric(n_lev)) stop('n_lev must be specified and it must be numeric')
	if (!is.character(imagery_path)) stop('enter the path to the folder containing the imagery')
	if (!is.logical(add_seq)) stop('add_seq must be T or F')
	if (!is.null(ignore_lev1) & !is.character(ignore_lev1)) stop('ignore_lev1 if specified, it must be a character')
	if (!is.null(ignore_nested) & !is.character(ignore_nested)) stop('ignore_nested if specified, it must be a character')

	levs<-list.dirs(imagery_path,recursive=T, full.names=F)
	# get rid of the empty element at the first place
	levs<-levs[-1]

	# find number of levels by counting "/" (e.g., path with 2 "/" it has 3 levels)
	num_lev<-sapply(gregexpr(levs, pattern='/'), function(x) {
										if(sum(x!=-1)>0) { y<- length(x)	
												} else { y<-0}
										y+1 }
										)
	# identify all paths with exactely n_lev (this include partial paths)
	lev_select<-levs[num_lev==n_lev]
	# identify paths with more than n_lev
	lev_plus<-levs[num_lev>n_lev]	

	# remove from lev_plus the good paths with the subfolders to be ignored.
	if (!is.null(ignore_nested)) {
		ignore_nest_pattern<-paste0(ignore_nested, collapse='|')
		ignore_nest_indx<-grepl(lev_plus, pattern=ignore_nest_pattern)
		lev_plus<-lev_plus[!ignore_nest_indx]
	}

	# identify partial paths among lev_select
	lent<-sapply(lev_select, function(x) sum(grepl(lev_plus, pattern=paste0('^',x)) ) )


	# extract the names of the paths with n_lev that ARE NOT partial paths
	paths2use<-names(lent[lent==0])
	# remove folder at level 1 that should be ignored (otherwise problems arise in cases when no folders with more levels than needed are present and some of these should be ignored)
	ignore_lev1_pattern<-paste0(ignore_lev1, collapse='|')
	paths2use<-paths2use[!grepl(paths2use, pattern=ignore_lev1_pattern)]
	

	# add WARNING with request of confirmation in case ignore NULL and target level paths containing subfolders 
	if (is.null(ignore_nested)){
		warn_folder<-names(lent[lent==1])
		warning (immediate.=TRUE, paste('\nThe folder(s):', warn_folder, ' contain subfolders preventing renaming to proceed for listed folder(s).\n To rename files in', warn_folder,' SET ARGUMENT "ignore_nested".'))
		my.resp <-NA
		while (!my.resp %in% c('Y','N') ) {my.resp <- readline(prompt="Proceed anyway? (Y/N): ")	}
		if (my.resp=='N') stop('Please set "ignore_nested" and rerun.')
	}	
	
	# make formats case insensitive
	img_format<-c(tolower(img_format),toupper(img_format))
	vid_format<-c(tolower(vid_format),toupper(vid_format))
	formats<-c(img_format,vid_format)

	for (i in 1: length(paths2use)) {
			tmp_path<-paste0(imagery_path,'/',paths2use[i])
			# get names of the files in the level 2 folder
			imgr_names<-list.files(tmp_path,recursive=F, full.names=F )
			# select only accepted imagery formats
			form_patt<-paste0('\\.',formats, collapse='|')
			imgr_names_indx<- grepl(imgr_names, pattern=form_patt)
			# imagery names (including videos)
			imgr_names<-imgr_names[imgr_names_indx]
			from_names<-imgr_names

			if (add_seq==T) { 
				# identify which are photos
				pics<-grepl(imgr_names, pattern=paste0('\\.',img_format, collapse='|'))
				# need to check that there are images. If there are only videos the rle function return an epty list causing an error.
				if (sum(pics)>0){
								pics_names_comp<-strsplit(imgr_names[pics],split='\\.')
								pics_names<-sapply(pics_names_comp, function(x) x[1])
								pics_formt<-sapply(pics_names_comp, function(x) x[2])
								# identify which and how many picture files have the same name excluding the format (i.e., are the same picture)
								x<-rle(sapply(pics_names_comp, function(x) x[1]))
								# create ID vector
								pics_seq_ID<-rep(1:length(x$lengths),x$lengths)
								# add ID to base name
								pics_names_seq<-paste0(pics_names,'-',pics_seq_ID,'.',pics_formt)
								# replace pics with sequential ID in vector imgr_names
								# imgr_names_seq<-imgr_names
								imgr_names[pics]<-pics_names_seq
							}
				base_name<-gsub(paths2use[i], pattern='/', replacement='-')
				name_tag<-paste0(base_name,'-',imgr_names)
				} else {

					base_name<-gsub(paths2use[i], pattern='/', replacement='-')
					name_tag<-paste0(base_name,'-',imgr_names)
					}
			file.rename(from=paste0(tmp_path,'/',from_names), to=paste0(tmp_path,'/',name_tag))
	}

}


# bench
# ---------

# imagery_path<-'F:/PPO_godbout/img_test'
# 
# imgRename(imagery_path='F:/PPO_godbout/imagery')

# imgRename(imagery_path='C:/Users/Utilisateur/Desktop/prova2',
# 			n_lev=1,
# 			ignore_lev1=c('tre_livelli'),
# 			ignore_nested=('no_rename'),
# 			img_format=c('txt','bmp'), 
# 			vid_format=c('docx'), 
# 			add_seq=T
# 			)

# example of used code
# Rename time lapse folders
# imgRename(imagery_path='G:/Godbout-60x60_mosaic/data', n_lev=3, ignore_lev1=c('calib.files'), ignore_nested=c('dive_1-aborted','colorcorrected-JL'), img_format=c('jpg','gpr','dng'), vid_format=c('mp4') , add_seq=T)
