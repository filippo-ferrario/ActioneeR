# ===============================================================================
# Name   	: Copy and Rename with folder structure
# Author 	: Filippo Ferrario
# Date   	: 01-06-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: Copy files from a camera and copy them onto another drive while organizing them into a folder hierarchy. 
# 				Working idea:
# 				files to copy can be selected by specifying:
# 				- a set of file names or an interval of names
# 				- an interval of timestamps
# 				The function should be able to create the desired folder hierarchy and paste the file correctely in the desired level of the folder tree.
# ===============================================================================

#' Copy and Rename with folder structure
#' 
#' Copy files from a camera and copy them onto another drive while organizing them into a folder hierarchy. 
#' 
#' @param source absolute or relative path to the folder/drive where the files are stored (e.g., an SD )
#' @param dest absolute path to where the files need to be copied
#' @param img_format character vector (case insensitive) specifying in which formats the image files could be provided. List all formats that are being used in the project. Any format not specified will not be considered. Formats are case insensitive: formats differing only in case will be treated as the same format (i.e., "jpg" == "JPG")
#' @param vid_format character vector (case insensitive) specifying in which formats the video files could be provided. List all formats that are being used in the project. Any format not specified will not be considered. Formats are case insensitive: formats differing only in case will be treated as the same format (i.e., "mp4" == "MP4")
#' @param name_levs numeric, given the path of `dest`, it specify the last N levels of the path to be used in renaming a file (e.g., with dest= c:/lev1/lev2/lev3/lev4 and names_levs=2 then lev3 and lev4 will be used in the name)
#' @param rename logical. Default to TRUE means that the copied files will also be renamed.
#' @param sep character use to separate the different components of the file name
#' @param add_seq logical. If TRUE (default) a sequential number is added at the end of the name, before the format extension, for each format type. 
#' @param prefix (optional) a string to be attaced to the beginning of the file name
#' @param from_file name of the first file of those in the interval to be copied (and renamed). See [details]
#' @param to_file name of the last file file of those in the interval to be copied (and renamed). See [details]
#' @param from_daytime timestamp marking the beginning of the time interval that includes the files to be copied on the base of when they were created originally (i.e., when the were created by the device that generated them).
#' @param to_daytime timestamp marking the end of the time interval that includes the files to be copied on the base of when they were created originally (i.e., when the were created by the device that generated them).
#' @param time_format format in which `from_daytime` and `to_daytime` are expressed as in [strptime].
#' 
#' @details
#' The time selection is based on the variable `ModifyDate` from the dataframe obtained using [exiftoolr::exif_read]. This because on development it seemed more reliable than CreatDate (which had unreal values for GRP files). 
#' 
#' From_file and to_file could be specified with or without extension. In case the extension is not provided, all files matching the name will be copied.
#' Selection of file by name has priority over time selection.
#' 
#' The function rely on Exif data from the original file to get the file metadata to work with. In particular, Exif is used to get the time stamp of when the file was first created (i.e., by a device such as camera, or phone or gps)
#' For files other than those created by divices that provide Exif metadata, the function should still work but only wit name-based selection 
#' 
#' #Warning
#' The function will not overwrite existing file, however no warning that files already exist is give. To be on the safe side erase files in the destination folder before copying. 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export

cam2disk<-function(source=NULL, dest=NULL, 
					# formats=c('jpg','gpr','mp4'), 
					img_format=c('png','jpg','gpr'), vid_format=c('mp4'),
					names_levs=3, rename=TRUE,sep='-', prefix=NULL, add_seq=TRUE,
					from_file=NULL, to_file=NULL, 
					from_daytime=NULL, to_daytime=NULL, time_format='%Y:%m:%d %H:%M:%S')
{
	# checks for arguments
	# ---------------------
	if (!is.character(source)) stop('source must be a character')
	if (!is.character(dest)) stop('dest must be a character')
	if (!is.null(img_format) & !is.character(img_format)) stop('img_format must be a character')
	if (!is.null(vid_format) & !is.character(vid_format)) stop('vid_format must be a character')
	if (!is.numeric(names_levs)) stop('names_levs must be a number')
	if (rename==TRUE & !is.character(sep)) stop('sep must be a character')
	if (!is.null(prefix) & !is.character(prefix)) stop('prefix must be a character')
	if (!is.null(from_file) & !is.character(from_file)) stop('from_file must be a character')
	if (!is.null(to_file) & !is.character(to_file)) stop('to_file must be a character')
	if (!is.null(from_daytime) & !is.character(from_daytime)) stop('from_daytime must be a character')
	if (!is.null(to_daytime) & !is.character(to_daytime)) stop('to_daytime must be a character')
	if (!is.null(from_daytime) & !is.character(time_format)) stop('time_format must be a character')


	# make formats case insensitive
	
	img_format<-c(tolower(img_format),toupper(img_format))
	vid_format<-c(tolower(vid_format),toupper(vid_format))
	formats<-c(img_format,vid_format)

	form_patt<-paste0('\\.',formats, collapse='|')
	# obtain the list of paths of the files with specified pattern 
	flst<-list.files(source, recursive=T, pattern=form_patt)
	# get the metadata
	meta<-exiftoolr::exif_read(paste0(source,'/',flst))
	meta$FileModifyDate<-as.POSIXct(strptime(meta$FileModifyDate, format='%Y:%m:%d %H:%M:%S')) # the time format is the match the one used in the dataframe created bt exitoolr
	
	# Select the files to be copied and renamed
	# -------------------------------------------
	# select file to copy based on time
	if(! (is.null(from_file) & is.null(to_file))){
		# From and to file lowercase to avoid case sensitive problems
		from_file<-tolower(from_file)
		to_file<-tolower(to_file)
		meta_name<-tolower(meta$FileName)
		rowid<-which(grepl(meta_name, pattern=paste0(from_file,'|',to_file)))
		if (length(rowid)<2) {
			stop('less than 2 names matching!')
		}
		if (length(rowid)>2) {
			# rowid<-range(rowid)
			# Warning if more than to files are matching
			warning('more than 2 matches for FROM and TO file names. provide extension to refine selection')
		}
		# select file to copy based on names
		sub_meta<-meta[min(rowid):max(rowid),]
		warning('File selection by FILE NAME!')
	} else {
		# select file to copy based on time
		if (! (is.null(from_daytime) & is.null(to_daytime))) {
		# convert reference daytime into a POSIX format
		t1<-as.POSIXct(strptime(from_daytime, format=time_format))
		t2<-as.POSIXct(strptime(to_daytime, format=time_format))
		if (is.na(t1)| is.na(t2)) {
				stop('provided daytime argument not in the format of time_format!')
			}
		# select file to copy based on time
		sub_meta<-meta[meta$FileModifyDate>=t1 & meta$FileModifyDate<=t2,]
		warning('File selection by TIME STAMP!')
		} 
	
	}


	# create dest path and copy file to destination
	dir.create(dest, recursive=TRUE)
	# get original name of the file
	orig<-sub_meta$FileName
	# Copy (and optionally rename) files
	if (rename==TRUE){
		# Prepare the file name
		# -----------------------
		# parsing dest path
		tree_levs<- unlist(strsplit(dest, split='/'))
		# define name tag using the last 'names_levs' levels of the folder hierarchy
		hier_levs<-length(tree_levs)
		name_tag<- paste0(tree_levs[(hier_levs-names_levs+1):length(tree_levs)], collapse=sep)

		finname<-paste(name_tag,orig, sep=sep) 
		# add prefix
		if (!is.null(prefix)) finname<-paste(prefix,finname, sep=sep) 
		# add sequential number to still images
		if (add_seq==T) {
			# identify which are photos
			pics<-grepl(finname, pattern=paste0('\\.',img_format, collapse='|'))
			# need to check that there are images. If there are only videos the rle function return an epty list causing an error.
				if (sum(pics)>0){
								pics_names_comp<-strsplit(finname[pics],split='\\.')
								pics_names<-sapply(pics_names_comp, function(x) x[1])
								pics_formt<-sapply(pics_names_comp, function(x) x[2])
								# identify which and how many picture files have the same name excluding the format (i.e., are the same picture)
								x<-rle(sapply(pics_names_comp, function(x) x[1]))
								# create ID vector
								pics_seq_ID<-rep(1:length(x$lengths),x$lengths)
								# add ID to base name
								pics_names_seq<-paste0(pics_names,sep,pics_seq_ID,'.',pics_formt)
								# replace pics with sequential ID in vector finname
								# finname_seq<-finname
								finname[pics]<-pics_names_seq
							}
		}


		# copy and rename
		file.copy(sub_meta$SourceFile, paste0(dest,'/',finname),copy.date=TRUE)
	} else {
		# copy only	
		finname<-orig
		file.copy(sub_meta$SourceFile, paste0(dest,'/',finname),copy.date=TRUE)
	}

	# write log file
		sub_meta$copied_name<-finname
		# find which variable is a list  
		lid<-sapply(names(sub_meta), function(x) { 
							!is.list(sub_meta[,x]) 
						}
			)
		# exclude list variables from the log
	    log_out<-sub_meta[,c('copied_name',names(lid[lid]))]
		# sub_meta$FileModifyDate<-as.character(sub_meta$FileModifyDate)
		write.table(log_out, file=paste0(dest,'/log_copy.txt'), append=TRUE,sep='\t',quote=TRUE, row.names=FALSE)
	
	alarm()
			
}



# =======================================
# Bench
# =======================================

# source<-'C:/Users/ferrariof/Documents/GitHub/ActioneeR/data'
# dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-seq'
# # formats=c('jpg','gpr','mp4')
# img_format=c('png','jpg','gpr')
# vid_format=c('mp4')
# from_daytime='10/19/2021 11:50'
# to_daytime='06/01/2022 12:40'
# time_format='%m/%d/%Y %H:%M'
# names_levs=3
# rename=TRUE
# sep='-'
# prefix=NULL
# add_seq=TRUE
# from_file=NULL
# to_file=NULL
# # from_file='G0020009'
# # to_file='G0031996'

# # dplyr::select(meta, SourceFile, contains('Date'))

# range(sub_meta$ModifyDate)


# meta_card<-exiftoolr::exif_read('F:/DCIM', recursive=TRUE)
# dplyr::select(meta_card, SourceFile, contains('Date')) [4243:4253,]
# range(meta_card$ModifyDate)


# exiftoolr::exif_read('C:/Users/ferrariof/Desktop/DFO - usefull links.txt')

# =======================================
# Test
# =======================================

# # copy and rename; both file names and times provided
# cam2disk(source='GitHub/ActioneeR/data', dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-names', 
# 		formats=c('jpg','gpr','mp4'), 
# 		names_levs=3, rename=TRUE,sep='-', prefix=NULL,
# 		from_file='G0020009', to_file='G0031996', 
# 		from_daytime='06/01/2022 10:38', to_daytime='06/01/2022 10:40', time_format='%Y:%m:%d %H:%M:%S')

# # copy and rename; only names 
# cam2disk(source='GitHub/ActioneeR/data', dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-names', 
# 		formats=c('jpg','gpr','mp4'), 
# 		names_levs=3, rename=TRUE,sep='-', prefix=NULL,
# 		from_file='G0020009', to_file='G0031996', 
# 		from_daytime=NULL, to_daytime=NULL, time_format='%Y:%m:%d %H:%M:%S')

# # copy and rename; only times
# cam2disk(source='GitHub/ActioneeR/data/DCIM-gopro', dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-time', 
# 		formats=c('jpg','gpr','mp4'), 
# 		names_levs=3, rename=TRUE,sep='-', prefix=NULL,
# 		# from_file='G0020009', to_file='G0031996', 
# 		from_daytime='06/01/2022 10:38', to_daytime='06/01/2022 12:00', time_format='%m/%d/%Y %H:%M')

# # only copy ; only times
# cam2disk(source='GitHub/ActioneeR/data/DCIM-gopro', dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-time-copy_only', 
# 		formats=c('jpg','gpr','mp4'), 
# 		names_levs=3, rename=F,sep='-', prefix=NULL,
# 		# from_file='G0020009', to_file='G0031996', 
# 		from_daytime='06/01/2022 10:38', to_daytime='06/01/2022 12:00', time_format='%m/%d/%Y %H:%M')

# # only copy ; only times = sequential
# cam2disk(source='GitHub/ActioneeR/data/DCIM-gopro', dest='C:/Users/ferrariof/Desktop/TL_05s/20220601-test/gp10-time-copy_only', 
# 		img_format=c('png','jpg','gpr'), vid_format=c('mp4'),
# 		names_levs=3, rename=F, sep='-', prefix=NULL,
# 		# from_file='G0020009', to_file='G0031996', 
# 		add_seq=TRUE,
# 		from_daytime='10/19/2021 11:50', to_daytime='06/01/2022 12:00', time_format='%m/%d/%Y %H:%M')


