# ===============================================================================
# Name   	: Exif-based synchronized images finder 
# Author 	: Filippo Ferrario
# Date   	: 02-03-2023 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	:  
# ===============================================================================


#' Exif-based image pair finder 
#' 
#' 
#'			   
#' @inheritParams cam2disk
#' @param main_img character vector of names of the image files (with file extension) that serves as reference to look for paired images.
#' @param cleanTemp TRUE to save a copy of the exif dataset in a temp file. FALSE to use an existing copy in the temp file. 
#' @param timeLapse Time Lapse interval (in seconds).  
#' @param sync_ref character vector specifying the names of the files which were deemed to be representing the same time point. 
#' 
#' 
#' @details
#' 
#' Files listed in `sync_ref` are ideally images that the operator deemed to be synchronous. Most of the time this are not really shot at the same exact time, but they are manually "aligned" or "synchronized", and so they provide a baseline to determine synchronicity of other images.
#' 
#' Use `cleanTemp = TRUE` when running the function in a folder for the first time, or when the files in the folder were modified. Use FALSE otherwise to speed up the runtime.
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export


sync_finder<-function(source=NULL, main_img=NULL, img_format=c('png','jpg','gpr'), sync_ref=NULL, timeLapse=1, cleanTemp=T)
{
	# set the option for subSecond decimals
	# op<-options()
	# options("digits.secs"= sec.digits)

	# make formats case insensitive
	
	img_format<-c(tolower(img_format),toupper(img_format))
	form_patt<-paste0('\\.',img_format, collapse='|')



	# obtain the list of paths of the files with specified pattern 
	flst<-list.files(source, recursive=T, pattern=form_patt)
	# get the metadata
	tempfile<-paste0(tempdir(),'\\exifmeta.csv')
	stored<-file.exists(tempfile)
	if (cleanTemp==T){
		meta<-exiftoolr::exif_read(paste0(source,'/',flst))
		meta<-meta[,c('FileName','Directory',grep(names(meta), pattern='SubSec|Date', value=T))]
		write.csv(meta, file=tempfile)
	} else {
		if(stored==F) stop("the temp file doesn't exist yet")
		meta<-read.csv(tempfile)
	}
	# Ensuring the existance of the time column to work on (for some reason the column meta$SubSecCreateDate does not always have values, but CreateDate and SubSecTime yes )
	meta$SubSecCreateDateBCKP<-paste0(meta$CreateDate,'.',meta$SubSecTime)
	meta$SubSecCreateDateBCKP<-as.POSIXct(strptime(meta$SubSecCreateDateBCKP, format='%Y:%m:%d %H:%M:%OS')) # the time format is the match the one used in the dataframe created bt exitoolr

	# Calclulate pairwise time difference between reference images 
	reft<-meta[meta$FileName %in% sync_ref,]$SubSecCreateDateBCKP
	names(reft)<-meta[meta$FileName %in% sync_ref,]$FileName  #this line to ensure same order in names in the vector as opposed to assign names from sync_ref 
	pairTdiff<-outer(reft,reft, FUN='-')
	# make order of columns and rows in the matrix to correspond to the order of sync_ref
	pairTdiff<-pairTdiff[sync_ref,sync_ref]
	# create a list of files dived by folder
	list_dir<-split(meta, f=meta$Directory)
	
	# lapply(list_dir, function(x){
	# 	head(x[,c('FileName',grep(names(x), pattern='Date', value=T))])
	# 	})

	# find in which folder is the main_img file to determine which are the folder where to look for files to be paired.
	listID<-lapply(list_dir, function(x){ #browser()
			sum(x$FileName==main_img)
		}) 
	lookDir<-which(listID==0)
	nam_lookDir<-names(list_dir[lookDir])
	refDir<-list_dir[-lookDir][[1]] 
	nam_refDir<-names(list_dir[-lookDir])
	# find the reference time
	reftime<- refDir[refDir$FileName==main_img,]$SubSecCreateDateBCKP

	file2pair<-lapply(list_dir[lookDir], function(x){ #browser()
		colid<-which(sync_ref %in% refDir$FileName)
		rowid<-which(sync_ref %in% x$FileName)
		timePaired<-reftime+pairTdiff[rowid,colid]
		dif<-abs(x$SubSecCreateDateBCKP-timePaired)
		mindif<-min(dif)
		# x$FileName[dif==mindif]
		# To avoid the problem of missing images at a give time point (e.g., in case a camera skipped taking some pics):
		if(mindif<=timeLapse) {
			x$FileName[dif==mindif]
			} else {
			NA	
			}



		# x[,c('FileName',grep(names(x), pattern='SubSec', value=T))]

		# x[dif==min(dif),c('FileName',grep(names(x), pattern='SubSec', value=T))]
		# refDir[refDir$FileName==main_img,c('FileName',grep(names(x), pattern='SubSec', value=T))]


		})
	# result
	# options()<-op
	file2pair


}



# BENCH
# ------------

# source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx'
# main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013190-51.JPG'
# img_format=c('jpg')
# sync_ref<-c('IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017613-102.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013248-109.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014117-106.JPG')
# timeLapse=1
# cleanTemp=T


# sync_finder( source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx',
# 			 main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013190-51.JPG',
# 			 img_format='jpg',
# 			 timeLapse=1,
# 			 sync_ref=c('IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017613-102.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013248-109.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014117-106.JPG'),
#  			 cleanTemp=T)



# source='F:2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx'
# main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0018508-997.JPG' 
# img_format='jpg' 
# sync_ref=c('IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017613-102.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013248-109.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014117-106.JPG') 
# cleanTemp= F

