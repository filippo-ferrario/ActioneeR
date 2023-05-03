# ===============================================================================
# Name   	: Exif-based image pair finder 
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
#' @param cleanTemp Use
#' @param timeLapse
#' @param sync_ref
#' 
#' 
#' @details
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
# #' @export

pair_finder<-function(source=NULL, main_img=NULL, img_format=c('png','jpg','gpr'), sync_ref=NULL, cleanTemp=T)
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
		write.csv(meta, file=tempfile)
	} else {
		if(stored==F) stop("the temp file doesn't exist yet")
		meta<-read.csv(tempfile)
	}
	
	meta$SubSecCreateDate<-as.POSIXct(strptime(meta$SubSecCreateDate, format='%Y:%m:%d %H:%M:%OS')) # the time format is the match the one used in the dataframe created bt exitoolr

	# Calclulate pairwise time difference between reference images 
	reft<-meta[meta$FileName %in% sync_ref,]$SubSecCreateDate
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
	reftime<- refDir[refDir$FileName==main_img,]$SubSecCreateDate

	file2pair<-lapply(list_dir[lookDir], function(x){ #browser()
		colid<-grep(sync_ref, pattern=paste0(refDir$FileName,collapse='|') )
		rowid<-grep(sync_ref, pattern=paste0(x$FileName,collapse='|') )
		timePaired<-reftime+pairTdiff[rowid,colid]
		dif<-abs(x$SubSecCreateDate-timePaired)
		mindif<-min(dif)
		# if(mindif<=abs(pairTdiff[rowid,colid])) {
		# 	x$FileName[dif==mindif]
		# 	} else {
		# 	NA	
		# 	}

		x$FileName[dif==mindif]


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

source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx'
main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013190-51.JPG'
img_format=c('jpg')
sync_ref<-c('IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017613-102.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013248-109.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014117-106.JPG')
# timeLapse=1
cleanTemp=T


sync_ref2<-c('IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017615-104.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013250-111.JPG','IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014119-108.JPG')
reft2<-meta[meta$FileName %in% sync_ref2,]$SubSecCreateDate
	names(reft2)<-sync_ref2
	pairTdiff2<-outer(reft2,reft2, FUN='-')


# pair_finder( source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx',
# 			 main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017610-99.JPG',
# 			 img_format='jpg',
# 			 cleanTemp=T)

# pair_finder( source='C:/Users/ferrariof/Documents/2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx',
# 			 main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017610-99.JPG',
# 			 img_format='jpg',
# 			 cleanTemp=F)

pair_finder( source='F:2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx',
			 main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0018508-997.JPG',
			 img_format='jpg',
 			 cleanTemp=T)

pair_finder( source='F:2022-CSRF_urchin_kelp/imagery/QC-ile_blanche-mosaicing/SW_site/TL_1s/20220705-T60_0Rx-T42_0Rx',
			 main_img='IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0018508-997.JPG',
			 img_format='jpg',
 			 cleanTemp=F)

meta[
meta$FileName %in% c(
'IAL-SW-20220705-T60_0Rx-T42_0Rx-lx-9_23-G0017613-102.JPG',
'IAL-SW-20220705-T60_0Rx-T42_0Rx-cx-9_4-G0013248-109.JPG',
'IAL-SW-20220705-T60_0Rx-T42_0Rx-rx-9_21-G0014117-106.JPG'),
c('FileName',grep(names(meta), pattern='SubSec', value=T))]
